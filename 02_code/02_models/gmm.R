## RMSLE: 0.1164

## LIBRARIES ======================================================================================
library("caret")
library("recipes")
library("glmnet")
library("tidyverse")
library("lubridate")

rm(list = ls())
source(paste0("./02_code/01_data_prep/features.R"))

df_all <- read_csv2("./01_data/02_processed/train_test_stacked.csv")
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


set.seed(123)
preped <- prep(rec, df_train)
df_tr <- bake(preped, df_train)
df_ts <- bake(preped, df_test)

sale_mean <- mean(df_tr$SalePrice)
sale_sd <- sd(df_tr$SalePrice)
df_tr$SalePrice <- (df_tr$SalePrice - sale_mean) / sale_sd


sale_mean <- mean(df_tr$SalePrice)
sale_sd <- sd(df_tr$SalePrice)
df_tr$SalePrice <- (df_tr$SalePrice - sale_mean) / sale_sd


## GAUSSIAN MIXTURE MODEL =========================================================================
library("KRMM")

### SIMULATE DATA 
set.seed(123)
p=200
N=100

beta=rnorm(p, mean=0, sd=1.0)
X=matrix(runif(p*N, min=0, max=1), ncol=p, byrow=TRUE)  #X: covariates (i.e. predictors)

f=X%*%beta                    #f: data generating process (i.e. DGP)
E=rnorm(N, mean=0, sd=0.5)

Y=f+E                           #Y: observed response data

hist(f)
hist(beta)
Nb_train=floor((2/3)*N)

###======================================================================###
### CREATE TRAINING AND TARGET SETS FOR RESPONSE AND PREDICTOR VARIABLES ###
###======================================================================###

Index_train <- sample(1:nrow(df_tr), 0.75*nrow(df_tr), replace = FALSE)
### Covariates (i.e. predictors) for training and target sets

Predictors_train <- df_tr[Index_train, -c(1:2)]
Response_train <- df_tr$SalePrice[Index_train]

Predictors_target <- df_tr[-Index_train, -c(1:2)]
True_value_target <- df_tr$SalePrice[-Index_train]

###=================================================================================###
### PREDICTION WITH KERNEL RIDGE REGRESSION SOLVED WITHIN THE MIXED MODEL FRAMEWORK ### 
###=================================================================================###

#Linear kernel

Linear_KRR_model_train = Kernel_Ridge_MM(Y_train=Response_train, 
                                         Matrix_covariates_train=Predictors_train, method="RR-BLUP")

f_hat_target_Linear_KRR = Predict_kernel_Ridge_MM(Linear_KRR_model_train,
                                                  Matrix_covariates_target = Predictors_train,
                                                  Y_target = Predictors_train)


mean((True_value_target-f_hat_target_Linear_KRR)**2)



#Gaussian kernel

Gaussian_KRR_model_train = Kernel_Ridge_MM( Y_train=Response_train, 
                                            Matrix_covariates_train=Predictors_train, method="RKHS", rate_decay_kernel=5.0)

f_hat_target_Gaussian_KRR = Predict_kernel_Ridge_MM( Gaussian_KRR_model_train, 
                                                     Matrix_covariates_target=Predictors_target )


#Graphics for RR-BLUP

dev.new(width=30, height=20)
par(mfrow=c(3,1))	
plot(f_hat_target_Linear_KRR, True_value_target)
plot(Linear_KRR_model_train$Gamma_hat, xlab="Feature (i.e. covariate) number", 
     ylab="Feature effect (i.e. Gamma_hat)", main="BLUP of covariate effects based on training data")
hist(Linear_KRR_model_train$Gamma_hat, main="Distribution of BLUP of 
covariate effects based on training data" )


# Compare prediction based on linear (i.e. RR-BLUP) and Gaussian kernel

dev.new(width=30, height=20)
par(mfrow=c(1,2))
plot(f_hat_target_Linear_KRR, True_value_target)
plot(f_hat_target_Gaussian_KRR, True_value_target)

mean((f_hat_target_Linear_KRR - True_value_target)^2)
mean((f_hat_target_Gaussian_KRR - True_value_target)^2)


## End(Not run)
library(devtools)
install_github("jlaria/sglfast")
library(sglfast)

# We create beta="the true coefficient vector" to be used in the simulations.
beta = 1:5

# We generate the model matrix X with iid columns and rows and the response y
X = matrix(rnorm(100*400), nrow = 100)
y = X[,1:5]%*%beta

# We chose the variance of the error such that SNR = 3
snr = 3
error = rnorm(100, mean = 0, sd=sqrt(var(y)/snr))
y = y+error

# Rows in the training sample
train.idx = sample(100, 50)

# Group indices for the SGL  
group_index = rep(1:40, each=10)

# Input data for the iterative 
data.train = list(x=X[train.idx,], y=y[train.idx])
data.validate = list(x=X[-train.idx,], y=y[-train.idx])

# We run the (unpooled) iterative SGL. For the 2-parameter version use isg_simple()
isgl.fit = isgl(data.train, data.validate, group_index, type = "linear")

# Best model returned by the iSGL algorithm
isgl.fit$beta
isgl.fit$intercept



library(SGL)

set.seed(123)
n = 50; p = 100; size.groups = 10
index <- ceiling(1:p / size.groups)

str_len <- nchar(colnames(X))
split_pos <- str_locate(colnames(X), "_")[,1] - 1
split_pos <- ifelse(is.na(split_pos), str_len, split_pos)
group_names <- str_sub(colnames(X), start = 0, end = split_pos)
group_index <- cumsum(c(1, as.numeric(group_names[1:145]!=group_names[2:146])))

X <- df_tr[,-c(1:2)]
X_new <- df_ts[,-c(1:2)]
y <- df_tr$SalePrice
data = list(x = X, y = y)
cvFit = cvSGL(data, index = group_index, type = "linear")
plot(cvFit)

fit <- SGL(data, index = group_index, type = "linear", maxit = 1000)
predictSGL(fit, newX = X_new)




