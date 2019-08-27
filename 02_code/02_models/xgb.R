## RMSLE: 0.119015

## LIBRARIES ======================================================================================
library("caret")
library("recipes")
library("glmnet")
library("tidyverse")
library("lubridate")

rm(list = ls())
df_all <- read_csv2("./01_data/02_processed/stacked_features.csv")

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
  # step_log(all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_YeoJohnson(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) # %>%
# step_center()


nn <- nrow(df_train)
set.seed(123)
sample_train <- sample(nn, round(nn * .75))
df_tr <- df_train[sample_train, ]
df_ts <- df_train[-sample_train, ]
preped <- prep(rec, df_tr)
df_tr <- juice(preped)
df_ts <- bake(preped, df_ts)
df_tt <- bake(preped, df_train)
df_test <- bake(preped, df_test)

col_tr <- colnames(df_tr)
col_ts <- colnames(df_ts)
col_tt <- colnames(df_tt)

setdiff(col_tr, col_ts)
setdiff(col_ts, col_tr)
setdiff(col_tt, col_tr)

# sum(is.na(df_tr_baked))
# sum(is.na(df_ts_baked))
# map_lgl(df_tr_baked, ~sum(is.na(.x))>0) %>%
#   keep(~.x == TRUE)

sale_mean <- mean(df_tr$SalePrice)
sale_sd <- sd(df_tr$SalePrice)
df_tr$SalePrice <- (df_tr$SalePrice - sale_mean) / sale_sd


## GAUSSIAN PROCESS REGRESSION =========================================================================

# 'gaussprPoly', 'gaussprLinear', 'gaussprRadial'
fit <- caret::train(SalePrice ~ . - Id,
  data = df_tr,
  method = "xgbTree",
  metric = "RMSE",
  tuneLength = 20,
  # tuneGrid = glmGrid,
  trControl = trainControl(
    method = "cv",
    number = 10,
    # savePredictions = "final",
    search = "random",
    verboseIter = TRUE,
    allowParallel = TRUE
  )
)

fit$bestTune

df_ts$SalePriceFit_xgb <- predict.train(fit, newdata = df_ts) * sale_sd + sale_mean
RMSE(pred = df_ts$SalePriceFit_xgb, obs = df_ts$SalePrice)

df_train_ <- bake(preped, df_train)
sale_mean <- mean(df_train$SalePrice)
sale_sd <- sd(df_train$SalePrice)
df_train$SalePrice <- (df_train$SalePrice - sale_mean) / sale_sd

df_tt$SalePriceFit_xgb <- predict.train(fit, newdata = df_tt) * sale_sd + sale_mean
RMSE(pred = df_tt$SalePriceFit_xgb, obs = df_tt$SalePrice)

write_csv(
  x = dplyr::select(df_tt, Id, starts_with("SalePrice")),
  path = "01_data/03_predictions/01_train/xgb_prediction.csv"
)


df_test$SalePriceFit_xgb <- predict.train(fit, newdata = df_test) * sale_sd + sale_mean

write_csv(
  x = dplyr::select(df_test, Id, starts_with("SalePrice")),
  path = "01_data/03_predictions/02_test/xgb_prediction.csv"
)