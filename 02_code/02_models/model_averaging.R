## LIBRARIES ======================================================================================
# library("mgcv")
library("caret")
library("recipes")
library("glmnet")
library("tidyverse")
library("lubridate")


## RUN PREDICTIONS ==================
rm(list = ls())
pred_models <- c("gp", "enet", "svm", "xgb")

for (x in pred_models) {
  source(paste0("./02_code/02_models/", x, ".R"))
}

pred_models <- c("gp", "enet", "svm", "xgb")
# "grp_lasso", "SpikeAndSlab",, "bayesian_regression")
dir_path <- "./01_data/03_predictions/01_train/"
df_train <- tibble()
for (pred_model in pred_models) {
  xx <- read_csv(paste0(dir_path, pred_model, "_prediction", ".csv"))
  if (pred_model == pred_models[1]) {
    df_train <- xx
  } else {
    df_train <- left_join(df_train, select(xx, -SalePrice), by = "Id")
  }
}




df_train

glmGrid <- expand.grid(alpha = seq(0, 1, .1), lambda = 10^seq(-4, 1, 1))

fit <- caret::train(SalePrice~.-Id, data = df_train,
                       method = "glmnet",
                       metric = "RMSE",
                       # tuneLength = 10,
                       tuneGrid = glmGrid,
                       trControl = trainControl(
                         method = "cv",
                         number = 5,
                         # savePredictions = "final",
                         search = "grid",
                         verboseIter = TRUE,
                         allowParallel = TRUE)
)

fit$bestTune

df_train$SalePriceFit_stacked <- predict.train(fit, newdata = df_train)
RMSE(pred = df_train$SalePriceFit_stacked, obs = df_train$SalePrice)


dir_path <- "./01_data/03_predictions/02_test/"
df_test <- tibble()
for (pred_model in pred_models) {
  xx <- read_csv(paste0(dir_path, pred_model, "_prediction", ".csv"))
  if (pred_model == pred_models[1]) {
    df_test <- xx
  } else {
    df_test <- left_join(df_test, select(xx, -SalePrice), by = "Id")
  }
}

colnames(df_test)
df_test$SalePrice <- predict.train(fit, newdata = df_test)
df_test <- df_test %>% dplyr::mutate_at(colnames(df_test)[-1], exp)

for (pred_model in pred_models) {
  
  var_name <- paste0("submission_", pred_model)
  pred_name <- paste0("SalePriceFit_", pred_model)
  x <- df_test %>% dplyr::select(Id, !!pred_name)
  colnames(x) <- c("Id", "SalePrice")
  
  save_path <- paste0("./01_data/03_predictions/03_submission/submission_", pred_model, ".csv")
  write_csv(x = x, path = save_path)

}

save_path <- paste0("./01_data/03_predictions/03_submission/submission_stacked.csv")
write_csv(x = dplyr::select(df_test, Id, SalePrice), path = save_path)

