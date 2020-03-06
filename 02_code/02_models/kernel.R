## RMSLE: ???

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
  dplyr::select(Id, SalePrice, everything(), -Neighborhood)


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

# glmGrid <- expand.grid(alpha = seq(0, 1, .1), lambda = 10^seq(-4, 1, 1))
fit <- caret::train(SalePrice ~ . - Id,
                    data = df_tr,
                    method = "bartMachine",
                    metric = "RMSE",
                    tuneLength = 4,
                    # tuneGrid = glmGrid,
                    trControl = trainControl(
                      method = "cv",
                      # number = 5,
                      # savePredictions = "final",
                      search = "random",
                      verboseIter = TRUE,
                      allowParallel = TRUE
                    )
)

head(fit$results[order(fit$results$RMSE),])
coefs <- coef(fit$finalModel, fit$bestTune$lambda)
coefs==0
coefs@i
lasso_nonzero <- coefs@Dimnames[[1]][coefs@i][-1]


df_tr_2 <- df_tr[union("SalePrice", lasso_nonzero)]
# df_ts_2 <- 

glmGrid <- expand.grid(alpha = seq(0, 1, .1), lambda = 10^seq(-4, 4, 1))
fit <- caret::train(SalePrice ~ . ,
                    data = df_tr_2,
                    method = "glmnet",
                    metric = "RMSE",
                    # tuneLength = 10,
                    tuneGrid = glmGrid,
                    trControl = trainControl(
                      method = "repeatedcv",
                      number = 5,
                      # savePredictions = "final",
                      search = "grid",
                      verboseIter = TRUE,
                      allowParallel = TRUE
                    )
)








df_ts$SalePriceFit_knn <- predict.train(fit, newdata = df_ts) * sale_sd + sale_mean
RMSE(pred = df_ts$SalePriceFit_knn, obs = df_ts$SalePrice)

df_tt$SalePriceFit_knn <- predict.train(fit, newdata = df_tt) * sale_sd + sale_mean
RMSE(pred = df_tt$SalePriceFit_knn, obs = df_tt$SalePrice)

write_csv(
  x = dplyr::select(df_tt, Id, starts_with("SalePrice")),
  path = "01_data/03_predictions/01_train/knn_prediction.csv"
)

df_test$SalePriceFit_knn <- predict.train(fit, newdata = df_test) * sale_sd + sale_mean

write_csv(
  x = dplyr::select(df_test, Id, starts_with("SalePrice")),
  path = "01_data/03_predictions/02_test/knn_prediction.csv"
)
