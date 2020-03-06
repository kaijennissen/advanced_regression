## LIBRARIES ======================================================================================
library("caret")
library("recipes")
library("glmnet")
library("tidyverse")
library("lubridate")

rm(list = ls())
# source(paste0("./02_code/01_data_prep/features.R"))

df_all <- read_csv2("./01_data/02_processed/train_test_stacked.csv")

## FEATURE ENGINEERING =========== ================================================================
ret_col_type <- function(df) {
  num_vars <- colnames(df_all)[map_lgl(df_all, is.numeric)]
  cat_vars <- colnames(df_all)[map_lgl(df_all, is.factor)]
  return(list(num = num_vars, cat = cat_vars))
}
col_type <- ret_col_type(df_all)
num_vars <- setdiff(col_type$num, c("SalePrice", "partition"))
cat_vars <- setdiff(col_type$cat, c("SalePrice", "partition"))
other_vars <- setdiff(colnames(df_all), union(union(num_vars, cat_vars), c("SalePrice", "partition", "Id")))
if (length(other_vars) > 0) {
  print(paste0(other_vars, " are neither numeric nor factors."))
}
pred_vars <- setdiff(colnames(df_all), c("SalePrice", "partition", "Id"))

#df_all
for (pred_var in pred_vars) {
  if (pred_var %in% num_vars) {
    p <- ggplot(df_all, aes_string(x = pred_var, y = "SalePrice")) +
      geom_point() +
      #geom_smooth(method = 'lm') +
      geom_smooth(method = 'loess')
  } else if (pred_var %in% cat_vars) {
    p <- ggplot(df_all, aes_string(x = pred_var, y = "SalePrice")) +
      geom_boxplot()
  }
  print(p)
  Sys.sleep(2)
}


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
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_BoxCox(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_normalize(all_predictors()) ## step_center(all_predictors()) %>% # step_scale(all_predictors()) # %>%


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

sale_mean <- mean(df_tr$SalePrice)
sale_sd <- sd(df_tr$SalePrice)
df_tr$SalePrice <- (df_tr$SalePrice - sale_mean) / sale_sd


## ELASTIC NET REGRESSION =========================================================================
glmGrid <- expand.grid(alpha = seq(0, 1, .1), lambda = 10^seq(-4, 1, 1))

fit <- caret::train(SalePrice ~ . - Id,
  data = df_tr,
  method = "glmnet",
  metric = "RMSE",
  # tuneLength = 10,
  tuneGrid = glmGrid,
  trControl = trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final",
    search = "grid",
    verboseIter = TRUE,
    allowParallel = TRUE
  )
)

plot(fit)#$bestTune

df_ts$SalePriceFit_enet <- predict.train(fit, newdata = df_ts) * sale_sd + sale_mean
RMSE(pred = df_ts$SalePriceFit_enet, obs = df_ts$SalePrice)

df_tt$SalePriceFit_enet <- predict.train(fit, newdata = df_tt) * sale_sd + sale_mean
RMSE(pred = df_tt$SalePriceFit_enet, obs = df_tt$SalePrice)

write_csv(
  x = dplyr::select(df_tt, Id, starts_with("SalePrice")),
  path = "01_data/03_predictions/01_train/enet_prediction.csv"
)

df_test$SalePriceFit_enet <- predict.train(fit, newdata = df_test) * sale_sd + sale_mean

write_csv(
  x = dplyr::select(df_test, Id, starts_with("SalePrice")),
  path = "01_data/03_predictions/02_test/enet_prediction.csv"
)











#########################
rec_prepared <- function(rec, data, split = FALSE) {
  if (split) {
    nn <- nrow(data)
    set.seed(123)
    sample_train <- sample(nn, round(nn * .75))
    df_tr <- data[sample_train, ]
    df_ts <- data[-sample_train, ]
  } else {
    df_tr <- data
    df_ts <- data
  }
  preped <- prep(rec, df_tr)
  df_tr_baked <- bake(preped, df_tr)
  df_ts_baked <- bake(preped, df_ts)
  ls_rec <- list(train = df_tr_baked, test = df_ts_baked)
  
  return(ls_rec)
}
