## RMSE: 
## RMSLE: 0.1197594

## LIBRARIES ======================================================================================
# library("mgcv")
library("caret")
# library("rsample")
library("recipes")
library("glmnet")
# library("forecast")
# library("corrplot")
# library("ggvis")
# library("splines")
library("tidyverse")
library("lubridate")

rm(list=ls())
df_all <- read_csv2("./01_data/02_processed/stacked_features.csv")

## MODEL===========================================================================================

## ANY NA'S LEFT
map_lgl(df_all, ~sum(is.na(.x))>0) %>% keep(~.x == TRUE)

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
  step_dummy(all_nominal())  %>% 
  # step_log(all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_YeoJohnson(all_numeric(), -all_outcomes(), -has_role("id")) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors())# %>% 
  # step_center()


set.seed(123)
sample_train <- sample(1:1456, round(1456*.8))
df_tr <- df_train[sample_train, ]
df_ts <- df_train[-sample_train, ]
preped <- prep(rec, df_tr)
df_tr_baked <- bake(preped, df_tr)
df_ts_baked <- bake(preped, df_ts)
df_test <- bake(preped, df_test)

# sum(is.na(df_tr_baked))
# sum(is.na(df_ts_baked))
# map_lgl(df_tr_baked, ~sum(is.na(.x))>0) %>%
#   keep(~.x == TRUE)

sale_mean <- mean(df_tr_baked$SalePrice)
sale_sd <- sd(df_tr_baked$SalePrice)
df_tr_baked$SalePrice <- (df_tr_baked$SalePrice-sale_mean)/sale_sd

## LINEAR REGRESSION ==============================================================================

# fit_lm <- caret::train(SalePrice~.-Id, data = df_tr_baked,
#                         method = "lm",
#                         metric = "RMSE",
#                         # tuneLength = 10,
#                         # tuneGrid = glmGrid,
#                         trControl = trainControl(
#                           method = "LOOCV",
#                           # number = L,
#                           savePredictions = "final",
#                           search = "grid",
#                           verboseIter = TRUE,
#                           allowParallel = TRUE)
# )


# df_ts_baked$SalePriceFit <- predict.train(fit_lm, newdata = df_ts_baked)*sale_sd+sale_mean
# RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)


## RIDGE REGRESSION ===============================================================================
glmGrid <- expand.grid(alpha = 0, lambda = 10^seq(-4, 1, 1))

fit_ridge <- caret::train(SalePrice~.-Id, data = df_tr_baked,
                        method = "glmnet",
                        metric = "RMSE",
                        # tuneLength = 10,
                        tuneGrid = glmGrid,
                        trControl = trainControl(
                          method = "cv",
                          number = 10,
                          savePredictions = "final",
                          search = "grid",
                          verboseIter = TRUE,
                          allowParallel = TRUE)
)


df_ts_baked$SalePriceFit <- predict.train(fit_ridge, newdata = df_ts_baked)*sale_sd+sale_mean
RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)


## LASSO REGRESSION ===============================================================================
glmGrid <- expand.grid(alpha = 1, lambda = 10^seq(-4, 1, 1))

fit_lasso <- caret::train(SalePrice~.-Id, data = df_tr_baked,
                        method = "glmnet",
                        metric = "RMSE",
                        # tuneLength = 10,
                        tuneGrid = glmGrid,
                        trControl = trainControl(
                          method = "cv",
                          number = 10,
                          savePredictions = "final",
                          search = "grid",
                          verboseIter = TRUE,
                          allowParallel = TRUE)
)

df_ts_baked$SalePriceFit <- predict.train(fit_lasso, newdata = df_ts_baked)*sale_sd+sale_mean
RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)

## ELASTIC NET REGRESSION =========================================================================
glmGrid <- expand.grid(alpha = seq(0, 1, .1), lambda = 10^seq(-4, 1, 1))

fit_elnet <- caret::train(SalePrice~.-Id, data = df_tr_baked,
                        method = "glmnet",
                        metric = "RMSE",
                        # tuneLength = 10,
                        tuneGrid = glmGrid,
                        trControl = trainControl(
                          method = "cv",
                          number = 10,
                          savePredictions = "final",
                          search = "grid",
                          verboseIter = TRUE,
                          allowParallel = TRUE)
)

df_ts_baked$SalePriceFit <- predict.train(fit_glm, newdata = df_ts_baked)*sale_sd+sale_mean
RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)

## BAYESIAN REGRESSION ============================================================================
# library("BoomSpikeSlab")
# 
# y <- df_tr_baked$SalePrice
# X <- df_tr_baked[,-c(1, 2)] %>% as.matrix()
# y_new <- df_ts_baked$SalePrice
# X_new <- df_ts_baked[,-c(1, 2)] %>% as.matrix()
# 
# bayes_fit <- lm.spike(y ~ X, niter = 1e5, error.distribution = "student")
# 
# pred <- predict(bayes_fit, newdata = X_new, burn = 2e4)
# mean_pred <- rowMeans(pred)
# # median_pred <- map_dbl(as.data.frame(t(pred)), median, na.rm = T)
# 
# df_ts_baked$SalePriceFit <- mean_pred*sale_sd+sale_mean
# RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)


## BOOMSPIKESLAB PRIOR REGRESSION =================================================================
# library("BoomSpikeSlab")
# 
# y <- df_tr_baked$SalePrice
# X <- df_tr_baked[,-c(1, 2)] %>% as.matrix()
# y_new <- df_ts_baked$SalePrice
# X_new <- df_ts_baked[,-c(1, 2)] %>% as.matrix()
# K <- dim(X)[2]
# prior <- IndependentSpikeSlabPrior(cbind(1, X), y,
#                                    expected.model.size = 10,  # expect 3 nonzero predictors
#                                    prior.df = .01,           # weaker prior than the default
#                                    optional.coefficient.estimate = rep(0, K+1) # shrink to zero
# )
# bayes_fit <- lm.spike(y ~ X, prior = prior, niter = 1e5)
# summary(bayes_fit)
# pred <- predict(bayes_fit, newdata = X_new, burn = 2e4)
# mean_pred <- rowMeans(pred)
# # median_pred <- map_dbl(as.data.frame(t(pred)), median, na.rm = T)
# 
# df_ts_baked$SalePriceFit <- mean_pred*sale_sd+sale_mean
# RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)
# 
# df_ts_baked %>% 
#   ggplot(aes(x = SalePriceFit, y = SalePrice))+
#   geom_point()+
#   geom_abline(slope=1)


## xgbTREE ========================================================================================
# xgbGrid <- expand.grid(eta = seq(.1, .2, .05),
#                        max_depth = seq(1, 20, 2),
#                        gamma = seq(.1, .2, .05),
#                        colsample_bytree = seq(.4, .6, .05),
#                        min_child_weight = seq(5, 50, 5), 
#                        subsample = .7,
#                        nrounds = 414)

fit_xgb <- caret::train(SalePrice~.-Id, data = df_tr_baked,
                          method = "xgbTree",
                          metric = "RMSE",
                          tuneLength = 100,
                          # tuneGrid = xgbGrid,
                          trControl = trainControl(
                            method = "cv",
                            number = 5,
                            search = "random",
                            verboseIter = TRUE,
                            allowParallel = TRUE)
)

# df_ts_baked$SalePriceFit <- predict.train(fit_xgb, newdata = df_ts_baked)*sale_sd+sale_mean
# RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)

## SVM ============================================================================================
# glmGrid <- expand.grid(alpha = seq(0, 1, .1), lambda = 10^seq(-4, 1, 1))

fit_svm <- caret::train(SalePrice~.-Id, data = df_tr_baked,
                        method = "svmPoly",
                        metric = "RMSE",
                        tuneLength = 100,
                        # tuneGrid = glmGrid,
                        trControl = trainControl(
                          method = "cv",
                          number = 5,
                          search = "random",
                          verboseIter = TRUE,
                          allowParallel = TRUE)
)

df_ts_baked$SalePriceFit <- predict.train(fit_svm, newdata = df_ts_baked)*sale_sd+sale_mean
RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)


## MODEL AVERAGING ================================================================================
df_ts_baked$SalePriceFit_xgb <- predict.train(fit_xgb, newdata = df_ts_baked)*sale_sd+sale_mean
df_ts_baked$SalePriceFit_ridge <- predict.train(fit_ridge, newdata = df_ts_baked)*sale_sd+sale_mean
df_ts_baked$SalePriceFit_svm <- predict.train(fit_svm, newdata = df_ts_baked)*sale_sd+sale_mean
df_ts_baked$SalePriceFit_lasso <- predict.train(fit_lasso, newdata = df_ts_baked)*sale_sd+sale_mean


# df_ts_baked$SalePriceFit_bayes <- mean_pred*sale_sd+sale_mean

# df_ts_baked %>% 
# ggplot(aes(x = SalePriceFit_svm, y = SalePrice))+
#   geom_point()+
#   geom_abline(slope=1)
# 
# df_ts_baked %>% 
#   ggplot(aes(x = SalePriceFit_xgb, y = SalePrice))+
#   geom_point()+
#   geom_abline(slope=1)
# 
# df_ts_baked %>% 
#   ggplot(aes(x = SalePriceFit_ridge, y = SalePrice))+
#   geom_point()+
#   geom_abline(slope=1)
# 
# df_ts_baked %>% 
#   ggplot(aes(x = SalePriceFit_lasso, y = SalePrice))+
#   geom_point()+
#   geom_abline(slope=1)

SALE <- df_ts_baked %>% dplyr::select(starts_with("SalePrice"),-SalePriceFit)


## LM ASTACKING
mod_lm <- lm(SalePrice~SalePriceFit_svm, SALE)
betas <- coef(mod_lm)
df_ts_baked$SalePriceAvg <- cbind(1, df_ts_baked$SalePriceFit_svm)%*%betas
RMSE(pred = df_ts_baked$SalePriceAvg, obs = df_ts_baked$SalePrice)


## LASSO STACKING
mod_avg <- cv.glmnet(x = as.matrix(SALE[,-1]), y = SALE$SalePrice, alpha = 1, intercept = T, lambda = exp(-10:20))
s = mod_avg$lambda[which(mod_avg$cvm == min(mod_avg$cvm))]
plot(mod_avg)
betas <- coef(mod_avg, s = s) %>% as.numeric()
df_ts_baked$SalePriceAvg <- cbind(1, as.matrix(SALE[,-1]))%*%betas
RMSE(pred = df_ts_baked$SalePriceAvg, obs = df_ts_baked$SalePrice)


## BAYESIAN STACKING
# prior <- IndependentSpikeSlabPrior(cbind(1, X), y,
#                                    expected.model.size = 10,  # expect 3 nonzero predictors
#                                    prior.df = .01,           # weaker prior than the default
#                                    optional.coefficient.estimate = rep(0, K+1) # shrink to zero
# )
# 
# mod_bayes <- lm.spike(SALE[,1, drop=T]~as.matrix(SALE[,-1]), niter = 1e5)
# summary(mod_bayes)
# 
# pred <- predict(mod_bayes, newdata = as.matrix(SALE[,-1]), burn = 2e4)
# df_ts_baked$SalePriceAvg <- rowMeans(pred)
# RMSE(pred = df_ts_baked$SalePriceAvg, obs = df_ts_baked$SalePrice)



df_ts_baked %>%
ggplot(aes(x = SalePriceAvg, y = SalePrice))+
  geom_point()+
  geom_abline(slope=1)


## SUBMISSION =====================================================================================


# preped <- prep(rec, df_train)
# df_tr_baked <- bake(preped, df_train)
# df_test <- bake(preped, df_test)

# sale_mean <- mean(df_tr_baked$SalePrice)
# sale_sd <- sd(df_tr_baked$SalePrice)
# df_tr_baked$SalePrice <- (df_tr_baked$SalePrice-sale_mean)/sale_sd

df_test$SalePrice_ridge <- predict.train(fit_ridge, newdata = df_test)*sale_sd+sale_mean
df_test$SalePrice_xgb  <- predict.train(fit_xgb, newdata = df_test)*sale_sd+sale_mean
df_test$SalePrice_svm  <- predict.train(fit_svm, newdata = df_test)*sale_sd+sale_mean
df_test$SalePrice_lasso  <- predict.train(fit_lasso, newdata = df_test)*sale_sd+sale_mean

X <- as.matrix(df_test[,c("SalePrice_ridge", "SalePrice_xgb", "SalePrice_svm", "SalePrice_lasso")])

df_test$SalePrice <- exp(cbind(1, X) %*% betas)

submission <- df_test %>% dplyr::select(Id, SalePrice)
write_csv(x = submission, path = "01_data/02_processed/submission_stacked.csv")






