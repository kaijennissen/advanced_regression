## RMSLE: 0.30

## LIBRARIES ======================================================================================
library("caret")
library("recipes")
library("glmnet")
library("tidyverse")
library("lubridate")

rm(list = ls())
df_all <- read_csv2("./01_data/02_processed/train_test_stacked.csv")

df_all <- df_all %>%
  group_by(Neighborhood) %>%
  summarise(mean_Neigh = median(SalePrice, na.rm = T)) %>%
  right_join(df_all, by = "Neighborhood") %>%
  select(Id, SalePrice, everything(), -Neighborhood)

## MODEL===========================================================================================

## ANY NA'S LEFT
map_lgl(df_all, ~ sum(is.na(.x)) > 0) %>% keep(~ .x == TRUE)

df_train <- df_all %>%
  dplyr::filter(partition == "train") %>%
  dplyr::select(Id, SalePrice, everything(), -partition)

df_test <- df_all %>%
  dplyr::filter(partition == "test") %>%
  dplyr::select(Id, SalePrice, everything(), -partition)

BN_obj <- bestNormalize::bestNormalize(df_train$SalePrice,
  standardize = TRUE,
  allow_lambert_s = TRUE,
  allow_lambert_h = TRUE,
  allow_orderNorm = FALSE,
  loo = TRUE
  # r = 5,
  # k = 5
)
df_train$SalePrice <- predict(BN_obj)
hist(df_train$SalePrice)

rec <- recipe(df_train) %>%
  update_role("SalePrice", new_role = "outcome") %>%
  update_role("Id", new_role = "id") %>%
  update_role(-all_outcomes(), -has_role("id"), new_role = "predictor") %>%
  step_nzv(all_predictors()) %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_BoxCox(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_normalize(all_predictors())

set.seed(123)
preped <- prep(rec, df_train)
df_tr <- bake(preped, df_train)
df_ts <- bake(preped, df_test)

# sale_mean <- mean(df_tr$SalePrice)
# sale_sd <- sd(df_tr$SalePrice)
# df_tr$SalePrice <- (df_tr$SalePrice - sale_mean) / sale_sd

## GAUSSIAN PROCESS REGRESSION =========================================================================

# 'gaussprLinear'

# 'gaussprPoly',
gpGrid <- expand.grid(degree = seq(0, 12, 1), scale = seq(10^-4, 10^-1, 2.5 * 10^-3))
gpGrid <- expand.grid(degree = 4:6, scale = 10^-4+5 * 10^-3)

# 'gaussprRadial'
# gpGrid <- expand.grid(sigma = seq(0, 1, .1))

fit <- caret::train(SalePrice ~ . - Id,
  data = df_tr,
  method = "gaussprPoly",
  metric = "RMSE",
  # tuneLength = 20,
  tuneGrid = gpGrid,
  trControl = trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5,
    # savePredictions = "final",
    search = "grid",
    verboseIter = TRUE,
    allowParallel = TRUE
  )
)

fit$results[order(fit$results$RMSE), ][1:5, ]


df_ts$SalePrice <- predict(BN_obj,
  newdata = predict.train(fit, newdata = df_ts),
  inverse = TRUE
)

df_submission <- df_ts %>% dplyr::select(Id, SalePrice)
write_csv(
  x = df_submission,
  path = "01_data/03_prediction/03_submission/submission_gppoly_09_14.csv"
)
