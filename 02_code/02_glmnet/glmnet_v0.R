## RMSE: 18172.2174785116
## RMSLE: 0.103256097607103

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

rm(list=ls())
## DATA
df_tr <- read_csv("01_data/01_raw/train.csv") %>% mutate(partition = "train")
df_ts <- read_csv("01_data/01_raw/test.csv") %>% mutate(partition = "test")
df_all <- bind_rows(df_tr, df_ts)

## MANUEL ENCODING ============================================================
df_all <- df_all %>% 
  dplyr::rename(FrstFlrSF = `1stFlrSF`, ScndFlrSF = `2ndFlrSF`, ThrdSsnPorch = `3SsnPorch`) %>% 
  mutate(
    LotShape = factor(LotShape, levels = c("IR3", "IR2", "IR1","Reg"), ordered = FALSE), 
    Utilities = factor(Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"), ordered = FALSE),
    LandSlope = factor(LandSlope, levels = c("Sev", "Mod", "Gtl"), ordered = FALSE),
    OverallQual = factor(OverallQual, levels = c(1:10), ordered = FALSE),
    OverallCond = factor(OverallCond, levels = c(1:10), ordered = FALSE),
    ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    BsmtQual = factor(BsmtQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    BsmtCond = factor(BsmtCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    BsmtExposure = factor(BsmtExposure, levels = c("No", "Mn", "Av", "Gd"), ordered = FALSE),
    BsmtFinType1 = factor(BsmtFinType1, levels = c("Unf", "LwQ","Rec", "BLQ", "ALQ", "GLQ"), ordered = FALSE),
    BsmtFinType2 = factor(BsmtFinType2, levels = c("Unf", "LwQ","Rec", "BLQ", "ALQ", "GLQ"), ordered = FALSE),
    HeatingQC = factor(HeatingQC, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    KitchenQual = factor(KitchenQual, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = FALSE),
    Functional = factor(Functional, c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2",  "Min1", "Typ"), ordered = FALSE),
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
  mutate_if(is_character, .funs = ~factor(.x))

## predictor types
pred_vars <- setdiff(colnames(df_all), c("Id", "SalePrice", "partition"))

## categorical predictors
chr_preds <- intersect(colnames(df_all)[map_lgl(df_all, is_character)], pred_vars)
df_all <- df_all %>% mutate_at(chr_preds, function(x) {as.factor(x)})
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
miss_vals <- map_dfr(df_all, ~sum(is.na(.x))) %>% gather() %>% 
  filter(value > 0) %>% arrange(desc(value))

## NA means "No": 
na_no <- c("Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "LotFrontage",
           ##
           "GarageFinish", "GarageFinish", "GarageCond", "GarageQual", "GarageType",
           "GarageCars", "GarageArea",
           ##
           "BsmtCond", "BsmtExposure", "BsmtQual", "BsmtFinType2", "BsmtFinType1",
           "BsmtFullBath", "BsmtHalfBath", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF")

## NA means "missing": 
na_missing <- c("MasVnrType", "MasVnrArea", "MSZoning", "Utilities", "Functional",
  "Exterior1st", "Exterior2nd", "BsmtFinSF1", "Electrical", "KitchenQual",
  "GarageYrBlt", "SaleType")

df_all <- df_all %>% 
  mutate_at(na_no, function(x){
  if(is.factor(x)){
    x <- fct_explicit_na(x, na_level = "No")
  } else{
    x <- replace_na(x, replace = 0)
  }
}) %>% 
    mutate_at(na_missing , function(x){
  if(is.factor(x)){
    x <- fct_explicit_na(x, na_level = names(which.max(table(x))))
  } else if(is.numeric(x)){
    x <- replace_na(x, replace = median(x, na.rm = T))
  }
}) 

map_dfr(df_all, ~sum(is.na(.x))) %>% gather() %>%
  filter(value > 0) %>% arrange(desc(value))

## PREPROCESSING ==============================================================


## STANDARIZE FEATURES
df_list <- list()
df_list[[1]] <- filter(df_all, partition == "train")
df_list[[2]] <- filter(df_all, partition == "test")

df_z <- map(df_list, function(df){
df <- df %>% mutate_at(num_pred_vars, ~(.x-mean(.x, na.rm = T))/sd(.x, na.rm = T))
return(df)
  }
) %>% reduce(bind_rows) 

# map_dfr(df_all, ~sum(is.na(.x))) %>% gather() %>%
#   filter(value > 0) %>% arrange(desc(value)) 


## MODEL=======================================================================
# df_all <- df_all %>%
#   mutate_at(top_features[1:5], ~BoxCox(.x, lambda = BoxCox.lambda(.x)))

df_train <- df_z %>% filter(partition == "train") %>% select(-partition)
df_test <- df_z %>% filter(partition == "test") %>% select(-partition)

y_trans <- "Log"
if(y_trans == "BoxCox"){
bc_lambda <- BoxCox.lambda(df_train$SalePrice)
df_train$SalePrice <- BoxCox(df_train$SalePrice, bc_lambda)
} else if (y_trans == "Log"){
df_train$SalePrice <- log(df_train$SalePrice)
} else if (y_trans == "Z"){
  df_train$SalePrice <- log(df_train$SalePrice)
  y_mean <- mean(df_train$SalePrice, na.rm = T)
  y_sd <- sd(df_train$SalePrice, na.rm = T)
df_train$SalePrice <- (df_train$SalePrice-y_mean)/y_sd
}

glmGrid <- expand.grid(alpha = seq(ß, 1, .05), lambda = 10^seq(-3, 3, 1))

fit_glm <- caret::train(SalePrice~.-Id, data = df_train,
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
                          allowParallel = TRUE)
                        )

fit_glm$results[order(fit_glm$results[,3]),] %>% head(10)

fit_glm$bestTune

coef(fit_glm$finalModel, fit_glm$bestTune$lambda)

if(y_trans == "BoxCox"){
## BoxCox-Inverse
df_train$SalePriceFit <- InvBoxCox(predict.train(fit_glm, newdata = df_train), lambda = bc_lambda)
df_train$SalePrice <- InvBoxCox(df_train$SalePrice, lambda = bc_lambda)
RMSE <- sqrt(mean((df_train$SalePrice-df_train$SalePriceFit)^2, na.rm = T))
RMSLE <- sqrt(mean((log(df_train$SalePrice)-log(df_train$SalePriceFit))^2, na.rm = T))
print(paste("RMSE:", RMSE)) 
print(paste("RMSLE:", RMSLE)) 
} else if (y_trans == "Log"){
## log-Inverse
df_train$SalePriceFit <- exp(predict.train(fit_glm, newdata = df_train))
df_train$SalePrice <- exp(df_train$SalePrice)
RMSE <- sqrt(mean((df_train$SalePrice-df_train$SalePriceFit)^2, na.rm = T))
RMSLE <- sqrt(mean((log(df_train$SalePrice)-log(df_train$SalePriceFit))^2, na.rm = T))
print(paste("RMSE:", RMSE)) 
print(paste("RMSLE:", RMSLE)) 
} else if (y_trans == "Z"){
  df_train$SalePriceFit <- exp(predict.train(fit_glm, newdata = df_train)*y_sd+y_mean)
  df_train$SalePrice <- exp(df_train$SalePrice*y_sd+y_mean)
  RMSE <- sqrt(mean((df_train$SalePrice-df_train$SalePriceFit)^2, na.rm = T))
  RMSLE <- sqrt(mean((log(df_train$SalePrice)-log(df_train$SalePriceFit))^2, na.rm = T))
  print(paste("RMSE:", RMSE)) 
  print(paste("RMSLE:", RMSLE)) 
} else {
  df_train$SalePriceFit <- predict.train(fit_glm, newdata = df_train)
  df_train$SalePrice <- df_train$SalePrice
  RMSE <- sqrt(mean((df_train$SalePrice-df_train$SalePriceFit)^2, na.rm = T))
  RMSLE <- sqrt(mean((log(df_train$SalePrice)-log(df_train$SalePriceFit))^2, na.rm = T))
  print(paste("RMSE:", RMSE)) 
  print(paste("RMSLE:", RMSLE)) 
}

df_train %>% ggplot(aes(x = SalePrice, y = SalePriceFit))+geom_point()+geom_abline(slope = 1, col = "red")+geom_smooth()


## SUBMISSION =================================================================

if(y_trans == "BoxCox"){
df_test$SalePrice <- InvBoxCox(predict.train(fit_glm, newdata = df_test), lambda = bc_lambda)
} else if (y_trans == "Log"){
df_test$SalePrice <- exp(predict.train(fit_glm, newdata = df_test))
} else if (y_trans == "Z"){
df_test$SalePrice <-  exp(predict.train(fit_glm, newdata = df_test)*y_sd+y_mean)
}
submission <- df_test %>% select(Id, SalePrice)
write_csv(x = submission, path = "01_data/02_processed/submission_gam_2207_2.csv")

