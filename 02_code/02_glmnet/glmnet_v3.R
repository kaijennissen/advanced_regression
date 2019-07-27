## RMSE: 17701.343
## RMSLE: 0.09998

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
# library("lga") ## OSCAR - Octagonal Selection and Clustering Algorithm in Regression
# library("grplasso") ## Group Lasso
library("gglasso") ## Group Lasso
library("SGL") ## sparse group lasso
# library("grpreg")
library("oem")
library("tictoc")
# library("fastglm")
# install.packages("gglasso")

rm(list = ls())
## DATA
df_tr <- read_csv("01_data/01_raw/train.csv") %>% mutate(partition = "train")
df_ts <- read_csv("01_data/01_raw/test.csv") %>% mutate(partition = "test")
df_all <- bind_rows(df_tr, df_ts)

## MANUEL ENCODING ============================================================
df_all <- df_all %>%
  dplyr::rename(FrstFlrSF = `1stFlrSF`, ScndFlrSF = `2ndFlrSF`, ThrdSsnPorch = `3SsnPorch`) %>%
  mutate(
    LotShape = factor(LotShape, levels = c("IR3", "IR2", "IR1", "Reg"), ordered = FALSE),
    Utilities = factor(Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"), ordered = FALSE),
    LandSlope = factor(LandSlope, levels = c("Sev", "Mod", "Gtl"), ordered = FALSE),
    OverallQual = factor(OverallQual, levels = c(1:10), ordered = FALSE),
    OverallCond = factor(OverallCond, levels = c(1:10), ordered = FALSE),
    ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    BsmtQual = factor(BsmtQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    BsmtCond = factor(BsmtCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    BsmtExposure = factor(BsmtExposure, levels = c("No", "Mn", "Av", "Gd"), ordered = FALSE),
    BsmtFinType1 = factor(BsmtFinType1, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = FALSE),
    BsmtFinType2 = factor(BsmtFinType2, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = FALSE),
    HeatingQC = factor(HeatingQC, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    KitchenQual = factor(KitchenQual, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    Functional = factor(Functional, c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"), ordered = FALSE),
    FireplaceQu = factor(FireplaceQu, levels = c("TA", "Gd", "Ex"), ordered = FALSE),
    GarageFinish = factor(GarageFinish, levels = c("Uf", "RFn", "Fin"), ordered = FALSE),
    GarageQual = factor(GarageQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    GarageCond = factor(GarageCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    PavedDrive = factor(PavedDrive, levels = c("N", "P", "Y"), ordered = FALSE),
    PoolQC = factor(PoolQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    Fence = factor(Fence, levels = c("MnWw", "GdWo", "MnPrv", "GdPrv"), ordered = FALSE),
    MoSold = factor(MoSold, levels = c(1:12), ordered = FALSE),
    MSSubClass = factor(paste0("MS", MSSubClass), ordered = FALSE)
  ) %>%
  mutate_if(is_character, .funs = ~ factor(.x))

## predictor types
pred_vars <- setdiff(colnames(df_all), c("Id", "SalePrice", "partition"))

## categorical predictors
chr_preds <- intersect(colnames(df_all)[map_lgl(df_all, is_character)], pred_vars)
df_all <- df_all %>% mutate_at(chr_preds, function(x) {
  as.factor(x)
})
cat_pred_vars <- colnames(df_all)[map_lgl(df_all, is.factor)]

## numeric predictors
num_vars <- colnames(df_all)[map_lgl(df_all, is.numeric)]
num_pred_vars <- intersect(intersect(pred_vars, num_vars), pred_vars)


## MISSING VALUES =============================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## remove outliers
# df_all %>% filter(GrLivArea > 4000) %>% select(Id, GrLivArea, SalePrice)
df_all <- df_all %>% filter(!Id %in% c(524, 1299))

## missing values
miss_vals <- map_dfr(df_all, ~ sum(is.na(.x))) %>%
  gather() %>%
  filter(value > 0) %>%
  arrange(desc(value))

## NA means "No":
na_no <- c(
  "Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "LotFrontage",
  ##
  "GarageFinish", "GarageFinish", "GarageCond", "GarageQual", "GarageType",
  "GarageCars", "GarageArea",
  ##
  "BsmtCond", "BsmtExposure", "BsmtQual", "BsmtFinType2", "BsmtFinType1",
  "BsmtFullBath", "BsmtHalfBath", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF"
)

## NA means "missing":
na_missing <- c(
  "MasVnrType", "MasVnrArea", "MSZoning", "Utilities", "Functional",
  "Exterior1st", "Exterior2nd", "BsmtFinSF1", "Electrical", "KitchenQual",
  "GarageYrBlt", "SaleType"
)

df_all <- df_all %>%
  mutate_at(na_no, function(x) {
    if (is.factor(x)) {
      x <- fct_explicit_na(x, na_level = "No")
    } else {
      x <- replace_na(x, replace = 0)
    }
  }) %>%
  mutate_at(na_missing, function(x) {
    if (is.factor(x)) {
      x <- fct_explicit_na(x, na_level = names(which.max(table(x))))
    } else if (is.numeric(x)) {
      x <- replace_na(x, replace = median(x, na.rm = T))
    }
  })

map_dfr(df_all, ~ sum(is.na(.x))) %>%
  gather() %>%
  filter(value > 0) %>%
  arrange(desc(value))

# devtools::install_github("jaredhuling/oem")

## NEW FEATURES ===========================================================

##
# df_all <- df_all %>% mutate_if(is.factor, ~fct_lump(.x, prop = .05))

## ADD FEATURES
df_all <- df_all %>%
  mutate(
    YearsSinceBuilt = pmax(0, YrSold - YearBuilt),
    YearsSinceRemodel = pmax(0, YrSold - YearRemodAdd),
    EQualCond = as.numeric(ExterQual) * as.numeric(ExterQual),
    OQualCond = as.numeric(OverallQual) * as.numeric(OverallQual),
    # Pool = ifelse(PoolArea == 0, "No", "Yes"),
    YrSold = factor(YrSold)
  ) # %>% select(-c(YearBuilt, YearRemodAdd))

## SIMPLE FEATURES
re_fac <- function(x) {
  n <- length(levels(x))
  if (n == 10) {
    resu <- factor(cut(as.numeric(x),
      breaks = c(0, 3, 7, 10),
      labels = c("Poor", "Avg", "Good")
    ), ordered = TRUE)
  } else if (n == 5) {
    resu <- factor(cut(as.numeric(x),
      breaks = c(0, 1, 3, 5),
      labels = c("Poor", "Avg", "Good")
    ), ordered = TRUE)
  }
  return(resu)
}

df_all <- df_all %>% mutate(
  SimpleOverallQual = re_fac(OverallQual),
  SimpleOverallCond = re_fac(OverallQual),
  SimpleExterCond = re_fac(ExterCond),
  SimpleExterQual = re_fac(ExterQual)
)

## predictor types
pred_vars <- setdiff(colnames(df_all), c("Id", "SalePrice", "partition"))

## numeric predictors
num_vars <- colnames(df_all)[map_lgl(df_all, is.numeric)]
num_pred_vars <- intersect(intersect(pred_vars, num_vars), pred_vars)

## categorical predictors
chr_preds <- intersect(colnames(df_all)[map_lgl(df_all, is_character)], pred_vars)
df_all <- df_all %>% mutate_at(chr_preds, function(x) {
  as.factor(x)
})
cat_pred_vars <- colnames(df_all)[map_lgl(df_all, is.factor)]

## POLYNOMIAL TERMS
cor_mat <- round(cor(df_all[!is.na(df_all$SalePrice), c("SalePrice", num_pred_vars)]), 2)
# corrplot(cor_mat[,order(cor_mat[1,], decreasing = T)], type = "upper", tl.srt = 45, )

top_features <- setdiff(
  colnames(cor_mat[, order(cor_mat[1, ], decreasing = T)]),
  "SalePrice"
)

df_all <- df_all %>% mutate_at(top_features,
  .funs = list(
    poly2 = ~ .x**2,
    poly3 = ~ .x**3,
    # poly4 = ~.x**4,
    sqrt = ~ .x**.5
  )
)





## OEM ======================================================================
# colnames(df_all)[sample(2:80, 60)]
df_subsample <- df_all %>%
  dplyr::select(SalePrice, colnames(df_all)[3],
         -Id) %>% 
  filter(!is.na(SalePrice))
# cat_pred_vars

rec <- recipe(df_subsample) %>%
  ## assign roles
  update_role(SalePrice, new_role = "outcome") %>%
  # update_role(Id, new_role = "id") %>%
  update_role(-all_outcomes(), -has_role("id"), new_role = "predictor") %>%
  # step_nzv("GrLivArea") %>%
  # step_unknown(all_nominal()) %>%
  # step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), one_hot = FALSE)

## test recipe
rec_prep <- prep(rec, df_subsample)
df_tr_baked <- bake(rec_prep, df_subsample)

X <- as.matrix(df_tr_baked[, -1])
y <- as.matrix(df_tr_baked[, 1])

xx <- colnames(df_tr_baked)[-1] %>% str_split(pattern = "_") %>% map(pluck(1)) %>% flatten_chr()
abc <- rle(xx)$length
group2 <- map(abc, ~{
  if (.x == 1){
    1
  } else {
    c(1, rep(0, .x-1))
  }
  }) %>% flatten_dbl() %>% cumsum

sum(is.na(X))

# eigen(t(X)%*%X)

## oem
# library("oem")
# fit_oem <- cv.oem(x = X, y = y
#                , penalty = c("sparse.grp.lasso")
#                # , gamma = 2
#                , groups = group2
#                , alpha = .3 ## mixing param for *.net
#                , gamma = 0
#                , compute.loss = TRUE
#                , standardize = TRUE
#                , lambda.min.ratio = 1e-3
#                , nfolds = 3
#                )
# # 
# fit_oem$cvm <- map(fit_oem$cvm, sqrt)
# fit_oem$cvsd <- map(fit_oem$cvsd, sqrt)
# fit_oem$cvup <- map2(fit_oem$cvm, fit_oem$cvsd, ~.x+.y )
# fit_oem$cvlo <- map2(fit_oem$cvm, fit_oem$cvsd, ~.x-.y )
# plot(fit_oem, which.model = 1)
# plot(fit_oem, which.model = 2)
# plot(fit_oem, which.model = 3)
# plot(fit_oem, which.model = 4)
# plot(fit_oem, which.model = 5)


## SGL
sgl_fun <- function(alpha, lambda, gamma,  group_index, df) {

 fit_sgl <- SGL(data = df
                  , index = group_index
                  , type = "linear"
                  # , nlam = 20
                  , alpha = alpha
                  , gamma = gamma
                , lambdas = c(lambda, lambda)
                
  )
 return(fit_sgl)
  }

sgl_predict <- function(fit, dfX){
  pred_sgl <- predictSGL(x = fit, newX = df$x, lam = 1)
  metric <- RMSE(pred = log(pred_sgl), obs = log(df$y))
  return(metric)
}

df <- list(x = X, y = y)
dim(df$x)
df_params <- expand.grid(alpha = seq(.2, .7, .2),
                         gamma = seq(.3, .6, .2),
                         lambda = exp(0:1)
                         ) %>% as_tibble()

df_cv <- df_params %>% 
  dplyr::mutate(fit = pmap(df_params, .f = sgl_fun, group2, df)) %>%
  dplyr::mutate(rmsle = map(fit, sgl_predict, df)) %>% 
  unnest(rmsle)

df_cv %>% arrange(rmsle) %>% print(n = 100)

df_cv %>% pull(rmsle) %>% quantile(c(.1, .99))

fit_sgl <- SGL(data = df
               , index = group2
               , type = "linear"
               # , nlam = 20
               , alpha = 0
               , gamma = 0
               , lambdas = c(exp(7), exp(8))
)


# lam_max <- lambdamax(SalePrice ~ .,
#   data = df_tr_baked,
#   model = LinReg(),
#   center = TRUE,
#   standardize = TRUE
# )

# grplasso(SalePrice ~ .,
#   data = df_tr_baked,
#   index = c(NA, 1, rep(2, 4)),
#   lambda = lam_max,
#   model = LinReg(),
#   center = TRUE,
#   standardize = TRUE
# )

## MODEL ======================================================================
glmGrid <- expand.grid(alpha = seq(0, 1, .2), lambda = 10^seq(-3, 3, 1))

fit_glm <- caret::train(SalePrice ~ . - Id,
  data = df_train,
  method = "glmnet",
  metric = "RMSE",
  tuneGrid = glmGrid,
  trControl = trainControl(
    method = "cv",
    number = 10,
    # repeats = 2,
    search = "grid",
    # returnResamp = "final",
    # summaryFunction = RMSLE,
    verboseIter = TRUE,
    allowParallel = TRUE
  )
)

fit_glm$results[order(fit_glm$results[, 3]), ] %>% head(10)
fit_glm$bestTune

coef(fit_glm$finalModel, fit_glm$bestTune$lambda)

if (y_trans == "BoxCox") {
  ## BoxCox-Inverse
  df_train$SalePriceFit <- InvBoxCox(predict.train(fit_glm, newdata = df_train), lambda = bc_lambda)
  df_train$SalePrice <- InvBoxCox(df_train$SalePrice, lambda = bc_lambda)
  RMSE <- sqrt(mean((df_train$SalePrice - df_train$SalePriceFit)^2, na.rm = T))
  RMSLE <- sqrt(mean((log(df_train$SalePrice) - log(df_train$SalePriceFit))^2, na.rm = T))
  print(paste("RMSE:", RMSE))
  print(paste("RMSLE:", RMSLE))
} else if (y_trans == "Log") {
  ## log-Inverse
  df_train$SalePriceFit <- exp(predict.train(fit_glm, newdata = df_train))
  df_train$SalePrice <- exp(df_train$SalePrice)
  RMSE <- sqrt(mean((df_train$SalePrice - df_train$SalePriceFit)^2, na.rm = T))
  RMSLE <- sqrt(mean((log(df_train$SalePrice) - log(df_train$SalePriceFit))^2, na.rm = T))
  print(paste("RMSE:", RMSE))
  print(paste("RMSLE:", RMSLE))
} else if (y_trans == "Log-Z") {
  df_train$SalePriceFit <- exp(predict.train(fit_glm, newdata = df_train) * y_sd + y_mean)
  df_train$SalePrice <- exp(df_train$SalePrice * y_sd + y_mean)
  RMSE <- sqrt(mean((df_train$SalePrice - df_train$SalePriceFit)^2, na.rm = T))
  RMSLE <- sqrt(mean((log(df_train$SalePrice) - log(df_train$SalePriceFit))^2, na.rm = T))
  print(paste("RMSE:", RMSE))
  print(paste("RMSLE:", RMSLE))
} else if (y_trans == "Z") {
  df_train$SalePriceFit <- predict.train(fit_glm, newdata = df_train) * y_sd + y_mean
  df_train$SalePrice <- df_train$SalePrice * y_sd + y_mean
  RMSE <- sqrt(mean((df_train$SalePrice - df_train$SalePriceFit)^2, na.rm = T))
  RMSLE <- sqrt(mean((log(df_train$SalePrice) - log(df_train$SalePriceFit))^2, na.rm = T))
  print(paste("RMSE:", RMSE))
  print(paste("RMSLE:", RMSLE))
} else {
  df_train$SalePriceFit <- predict.train(fit_glm, newdata = df_train)
  df_train$SalePrice <- df_train$SalePrice
  RMSE <- sqrt(mean((df_train$SalePrice - df_train$SalePriceFit)^2, na.rm = T))
  RMSLE <- sqrt(mean((log(df_train$SalePrice) - log(df_train$SalePriceFit))^2, na.rm = T))
  print(paste("RMSE:", RMSE))
  print(paste("RMSLE:", RMSLE))
}

df_train %>% ggplot(aes(x = SalePrice, y = SalePriceFit)) + geom_point() + geom_abline(slope = 1, col = "red") + geom_smooth()


## SUBMISSION =================================================================

if (y_trans == "BoxCox") {
  df_test$SalePrice <- InvBoxCox(predict.train(fit_glm, newdata = df_test), lambda = bc_lambda)
} else if (y_trans == "Log") {
  df_test$SalePrice <- exp(predict.train(fit_glm, newdata = df_test))
} else if (y_trans == "Log-Z") {
  df_test$SalePrice <- exp(predict.train(fit_glm, newdata = df_test) * y_sd + y_mean)
} else if (y_trans == "Z") {
  df_test$SalePrice <- predict.train(fit_glm, newdata = df_test) * y_sd + y_mean
} else {
  df_test$SalePrice <- predict.train(fit_glm, newdata = df_test)
}

submission <- df_test %>% select(Id, SalePrice)
write_csv(x = submission, path = "01_data/02_processed/submission_gam_2207_4.csv")
