## RMSLE: 0.3110348

## LIBRARIES ======================================================================================
library("caret")
library("recipes")
library("glmnet")
library("tidyverse")
library("lubridate")

rm(list = ls())
df_all <- read_csv2("./01_data/02_processed/train_test_stacked.csv")

## MODEL===========================================================================================

## ANY NA'S LEFT
map_lgl(df_all, ~ sum(is.na(.x)) > 0) %>% keep(~ .x == TRUE)

df_train <- df_all %>%
  dplyr::filter(partition == "train") %>%
  dplyr::select(Id, SalePrice, everything(), -partition)

df_test <- df_all %>%
  dplyr::filter(partition == "test") %>%
  dplyr::select(Id, SalePrice, everything(), -partition)

df_train$SalePrice <- log(df_train$SalePrice)

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

sale_mean <- mean(df_tr$SalePrice)
sale_sd <- sd(df_tr$SalePrice)
df_tr$SalePrice <- (df_tr$SalePrice - sale_mean) / sale_sd

## GAUSSIAN PROCESS REGRESSION =========================================================================

# 'svmLinear'
svmGrid <- expand.grid(C = seq(10^-4, 10^-1, by = 5*10^-4))

# 'gaussprPoly',
# gpGrid <- expand.grid(degree = seq(4, 10, 1), scale = 10^seq(-4, -2, 1))

# 'gaussprRadial'
# gpGrid <- expand.grid(sigma = seq(10^-4, 10^-2, 10^-4))

fit <- caret::train(SalePrice ~ . - Id,
  data = df_tr,
  method = "svmLinear",
  metric = "RMSE",
  # tuneLength = 2,
  tuneGrid = svmGrid,
  trControl = trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5,
    # savePredictions = "final",
    search = "random",
    verboseIter = TRUE,
    allowParallel = TRUE
  )
)

fit$results[order(fit$results$RMSE),]
fit$bestTune


df_ts$SalePrice <- exp(predict.train(fit, newdata = df_ts) * sale_sd + sale_mean)

df_submission <- df_ts %>% select(Id, SalePrice)
write_csv(
  x = df_submission,
  path = "01_data/03_prediction/03_submission/submission_gppoly.csv"
)
