## LIBRARIES ====================================================
library("tidyverse")
library("lubridate")
library("mgcv")
library("caret")
library("rsample")
library("recipes")
library("glmnet")
library("forecast")
library("corrplot")
library("ggvis")
library("splines")
library("oem")
library("bestNormalize")

rm(list=ls())
## DATA
df_al <- read_csv2("./01_data/02_processed/stacked_features.csv")

## OEM ======================================================================
df_subsample <- df_all %>%
  dplyr::select(SalePrice, colnames(df_all)[3:5], -c(partition, Id))

df_train <- df_all %>% 
  filter(partition == "train") %>% 
  select(Id, SalePrice, everything(), -partition)

df_test <- df_all %>% 
  filter(partition == "test") %>% 
  select(Id, SalePrice, everything(), -partition)
df_train$SalePrice <- log(df_train$SalePrice)

rec <- recipe(df_train) %>% 
  update_role("SalePrice", new_role = "outcome") %>% 
  update_role("Id", new_role = "id") %>%
  update_role(-all_outcomes(), -has_role("id"), new_role = "predictor") %>% 
  # step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) %>% 
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal())  %>% 
  step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_YeoJohnson(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 

sample_train <- sample(1:1456, 1000)
df_tr <- df_train[sample_train, ]
df_ts <- df_train[-sample_train, ]
preped <- prep(rec, df_train)
df_tr_baked <- bake(preped, df_tr)
df_ts_baked <- bake(preped, df_ts)

map_lgl(df_tr_baked, ~sum(is.na(.x))>0) %>% keep(~.x == TRUE)

sum(is.na(df_tr_baked))
sum(is.na(df_ts_baked))

sale_mean <- mean(df_tr_baked$SalePrice)
sale_sd <- sd(df_tr_baked$SalePrice)
df_tr_baked$SalePrice <- (df_tr_baked$SalePrice-sale_mean)/sale_sd

X <- as.matrix(df_tr_baked[, -c(1:2)])
y <- as.matrix(df_tr_baked[, 2])
X_new <- as.matrix(df_ts_baked[, -c(1:2)])
y_new <- as.matrix(df_ts_baked[, 2])

feature_group_names <- flatten_chr(map(str_split(colnames(df_tr_baked)[-c(1:2)], pattern = "_"), pluck(1)))
group1 <- map(rle(feature_group_names)$length, ~{
  if (.x == 1){
    1
  } else {
    c(1, rep(0, .x-1))
  }
}) %>% flatten_dbl() %>% cumsum


fit_oem_fun <- function(model, hyper){
  
fit_oem <- cv.oem(x = X, y = y
               , penalty = model
               # c(
                 # "sparse.grp.lasso"
                 # ,"grp.lasso.net"
                 # , "grp.scad.net"
                 # )
               # , gamma = 2
               , groups = group1
               , alpha = hyper ## mixing param for *.net
               # , alpha = .9 ## mixing param for *.net
               # , gamma = 1.1
               # , tau =.9 ## mixing param for *sparse.grp.lasso
                , tau = hyper ## mixing param for *.net
               , compute.loss = TRUE
               # , standardize = TRUE
               , lambda.min.ratio = 1e-3
               # , lambda = exp(seq(-6, 30, .5))
               , nlambda = 50
               , nfolds = 10
               )

resu <- RMSE(obs = y_new, pred = (predict(fit_oem, newx = X_new)*sale_sd+sale_mean))
# print(paste0("RMSE for i = ", hyper, " is: ", resu))
return(resu)
} 

# for (i in seq(.1, .9, .1)) {
#   possibly(fit_oem_fun("sparse.grp.lasso", i), otherwise = NA)
# }

hyper <- expand.grid(model = c("sparse.grp.lasso", "grp.lasso.net"),
                hyper = seq(.1, .9, .1)) %>% as_tibble() %>% mutate(model = as.character(model))
hyper <- hyper %>% dplyr::mutate(rmse = map2(model, hyper, fit_oem_fun)) %>% unnest(rmse)

fit_oem <- cv.oem(x = X, y = y
                  , penalty = "grp.lasso.net"
                  , groups = group1
                  , alpha = .2
                  , compute.loss = TRUE
                  , standardize = TRUE
                  , lambda.min.ratio = 1e-3
                  , nlambda = 50
                  , nfolds = 10
)

df_ts_baked$SalePriceFit <- predict(fit_oem, newx = X_new)*sale_sd+sale_mean

df_ts_baked %>% 
  ggplot(aes(x = SalePrice, y = SalePriceFit)) + 
  geom_point() + 
  geom_abline(slope = 1, col = "red") + 
  geom_smooth()

df_all %>% 
ggplot(aes(y = log(SalePrice))) + 
  geom_boxplot() #+ 
  geom_abline(slope = 1, col = "red") + 
  geom_smooth()

boxplot(log(df_all$SalePrice), range = 1.5)
  
## SUBMISSION =================================================================

preped <- prep(rec, df_train)
df_tr_baked <- bake(preped, df_train)
df_ts_baked <- bake(preped, df_test)

sale_mean <- mean(df_tr_baked$SalePrice)
sale_sd <- sd(df_tr_baked$SalePrice)
df_tr_baked$SalePrice <- (df_tr_baked$SalePrice-sale_mean)/sale_sd

X <- as.matrix(df_tr_baked[, -c(1:2)])
y <- as.matrix(df_tr_baked[, 2])
X_pred <- as.matrix(df_ts_baked[, -c(1:2)])

## fit oem on all data
fit_oem <- cv.oem(x = X, y = y
                  , penalty = "grp.lasso.net"
                  , groups = group1
                  , alpha = .8
                  , compute.loss = TRUE
                  , standardize = TRUE
                  , lambda.min.ratio = 1e-3
                  , nlambda = 100
                  , nfolds = 10
)
plot(fit_oem)
df_ts_baked$SalePrice <- exp(predict(fit_oem, newx = X_pred)*sale_sd+sale_mean)
submission <- df_ts_baked %>% select(Id, SalePrice)
write_csv(x = submission, path = "01_data/02_processed/submission_grp_lasse_0108_1.csv")

