## RMSE: 
## RMSLE: 

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
  rename(FrstFlrSF = `1stFlrSF`, ScndFlrSF = `2ndFlrSF`, ThrdSsnPorch = `3SsnPorch`) %>% 
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

## MISSING VALUES =============================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## remove outliers
df_all %>% filter(GrLivArea > 4000) %>% select(Id, GrLivArea, SalePrice)
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

## MODEL==================================================================

df_train <- df_all %>% filter(partition == "train") %>% select(-partition)
df_test <- df_all %>% filter(partition == "test") %>% select(-partition)

map_dfr(df_all, ~sum(is.na(.x))) %>% gather() %>%
  filter(value > 0) %>% arrange(desc(value))

bc <- FALSE
if (bc){
bc_lambda <- BoxCox.lambda(df_train$SalePrice)
df_train$SalePrice <- BoxCox(df_train$SalePrice, bc_lambda)
} else{
df_train$SalePrice <- log(df_train$SalePrice)
}

# glmGrid <- expand.grid(alpha = seq(0, 1, .05), lambda = 10^seq(-3, 10, 1))

fit_glm <- caret::train(SalePrice~.-Id, data = df_train,
                        method = "xgbTree",
                        metric = "MAE",
                        # tuneGrid = glmGrid,
                        trControl = trainControl(
                          method = "cv",
                          number = 10,
                          # repeats = 2,
                          search = "random",
                          # returnResamp = "final",
                          # summaryFunction = RMSLE,
                          verboseIter = TRUE,
                          allowParallel = TRUE)
                        )

# fit_glm[4]$recipe$steps
fit_glm$results[order(fit_glm$results[,3]),] %>% head(10)
fit_glm$bestTune
coef(fit_glm$finalModel, fit_glm$bestTune$lambda)

if(bc){
## BoxCox-Inverse
df_train$SalePriceFit <- InvBoxCox(predict.train(fit_glm, newdata = df_train), lambda = bc_lambda)
df_train$SalePrice <- InvBoxCox(df_train$SalePrice, lambda = bc_lambda)
RMSE <- sqrt(mean((df_train$SalePrice-df_train$SalePriceFit)^2, na.rm = T))
RMSLE <- sqrt(mean((log(df_train$SalePrice)-log(df_train$SalePriceFit))^2, na.rm = T))
print(paste("RMSE:", RMSE)) 
print(paste("RMSLE:", RMSLE)) 
} else{
## log-Inverse
df_train$SalePriceFit <- exp(predict.train(fit_glm, newdata = df_train))
df_train$SalePrice <- exp(df_train$SalePrice)
RMSE <- sqrt(mean((df_train$SalePrice-df_train$SalePriceFit)^2, na.rm = T))
RMSLE <- sqrt(mean((log(df_train$SalePrice)-log(df_train$SalePriceFit))^2, na.rm = T))
print(paste("RMSE:", RMSE)) 
print(paste("RMSLE:", RMSLE)) 
}

df_train %>% ggplot(aes(x = SalePrice, y = SalePriceFit))+geom_point()+geom_abline(slope = 1, col = "red")+geom_smooth()

## RMSE: 22314.77
## RMSE: 19604.42 - BoxCox Transformed Sale Price
## RMSE: 17597.43 - Log-Transform Sale Price
## RMSE: 17574.06 - BS-Splines
## RMSE: 17544.09 - 0.12021 Log-Transform Sale Price, Features normal
## RMSE: 17521.22 - 0.12009 Log-Transform Sale Price, Features BoxCox
## RMSE: 17256.64 - Log-Transform Sale Price Top-Features
## RMSE: 17193.04 - Log-Transform Sale price Top-4
## RMSE: 17185.03 - Log-Transform SalePrice; Features normal
## RMSE: 17179.61 - Log-Transform SalePrice; Top-2-Features 
## RMSE: 17058.44 - Log-Transform Sale Price & BoxCox Top-Features
## RMSE: 17057.69 - 


## SUBMISSION =================================================================
length(predict.train(fit_glm, newdata = df_test))

if(bc){
df_test$SalePrice <- InvBoxCox(predict.train(fit_glm, newdata = df_test), lambda = bc_lambda)
}else{
df_test$SalePrice <- exp(predict.train(fit_glm, newdata = df_test))
}
submission <- df_test %>% select(Id, SalePrice)
write_csv(x = submission, path = "01_data/02_processed/submission_gam_2107_1.csv")

