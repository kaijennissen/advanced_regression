## RMSE: 17237.39
## RMSLE: 0.097

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
    LotShape = factor(LotShape, levels = c("IR3", "IR2", "IR1","Reg"), ordered = TRUE), 
    Utilities = factor(Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"), ordered = TRUE), 
    LandSlope = factor(LandSlope, levels = c("Sev", "Mod", "Gtl"), ordered = TRUE), 
    OverallQual = factor(OverallQual, levels = c(1:10), ordered = TRUE), 
    OverallCond = factor(OverallCond, levels = c(1:10), ordered = TRUE), 
    ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    BsmtQual = factor(BsmtQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    BsmtCond = factor(BsmtCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    BsmtExposure = factor(BsmtExposure, levels = c("No", "Mn", "Av", "Gd"), ordered = TRUE), 
    BsmtFinType1 = factor(BsmtFinType1, levels = c("Unf", "LwQ","Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE), 
    BsmtFinType2 = factor(BsmtFinType2, levels = c("Unf", "LwQ","Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE), 
    HeatingQC = factor(HeatingQC, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    KitchenQual = factor(KitchenQual, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    Functional = factor(Functional, c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2",  "Min1", "Typ"), ordered = TRUE), 
    FireplaceQu = factor(FireplaceQu, levels = c("TA", "Gd", "Ex"), ordered = TRUE), 
    GarageFinish = factor(GarageFinish, levels = c("Uf", "RFn", "Fin"), ordered = TRUE), 
    GarageQual = factor(GarageQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    GarageCond = factor(GarageCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    PavedDrive = factor(PavedDrive, levels = c("N", "P", "Y"), ordered = TRUE), 
    PoolQC = factor(PoolQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE), 
    Fence = factor(Fence, levels = c("MnWw", "GdWo", "MnPrv", "GdPrv"), ordered = TRUE), 
    MoSold = factor(MoSold, levels = c(1:12), ordered = TRUE), 
    MSSubClass = factor(paste0, ordered = TRUE), 
    ) %>% 
  mutate_if(is_character, .funs = ~factor(.x))

## MISSING VALUES =============================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## remove outliers
df_all <- df_all %>% filter(GrLivArea < 4000) #%>% select(Id, GrLivArea, SalePrice)
# df_all <- df_all %>% filter(!Id %in% c(524, 1299))

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

## NEW FEATURES ===========================================================

##
# df_all <- df_all %>% mutate_if(is.factor, ~fct_lump(.x, prop = .05))

## ADD FEATURES
df_all <- df_all %>% 
  mutate(
    YearsSinceBuilt = pmax(YrSold - YearBuilt, 0),
    YearsSinceRemodel = pmax(YrSold - YearRemodAdd, 0),
    EQualCond = as.numeric(ExterQual)*as.numeric(ExterQual),
    OQualCond = as.numeric(OverallQual)*as.numeric(OverallQual),
    # Pool = ifelse(PoolArea == 0, "No", "Yes"),
    YrSold = factor(YrSold)
  ) #%>% select(-c(YearBuilt, YearRemodAdd))

## SIMPLE FEATURES
re_fac <- function(x){
  n <- length(levels(x))
  if(n == 10){
  resu <- factor(cut(as.numeric(x), breaks = c(0, 3, 7, 10),
           labels = c("Poor", "Avg", "Good")), ordered = TRUE)
  } else if(n == 5){
    resu <- factor(cut(as.numeric(x), breaks = c(0, 1, 3, 5),
               labels = c("Poor", "Avg", "Good")), ordered = TRUE)
    }
    return(resu)
  }

df_all <- df_all %>% mutate(
  SimpleOverallQual = re_fac(OverallQual),
  SimpleOverallCond = re_fac(OverallQual),
  SimpleExterCond = re_fac(ExterCond),
  SimpleExterQual = re_fac(ExterQual)
) 

# predictor types
pred_vars <- setdiff(colnames(df_all), c("Id", "SalePrice", "partition"))

## numeric predictors
num_vars <- colnames(df_all)[map_lgl(df_all, is.numeric)]
num_pred_vars <- intersect(intersect(pred_vars, num_vars), pred_vars)

## categorical predictors
chr_preds <- intersect(colnames(df_all)[map_lgl(df_all, is_character)], pred_vars)
df_all <- df_all %>% mutate_at(chr_preds, function(x) {as.factor(x)})
cat_pred_vars <- colnames(df_all)[map_lgl(df_all, is.factor)]

## POLYNOMIAL TERMS
cor_mat <- round(cor(df_all[!is.na(df_all$SalePrice), c("SalePrice", num_pred_vars)]), 2)
# corrplot(cor_mat[,order(cor_mat[1,], decreasing = T)], type = "upper", tl.srt = 45, )

top_features <- setdiff(colnames(cor_mat[,order(cor_mat[1,], decreasing = T)]),
                        "SalePrice")

df_all <- df_all %>% mutate_at(top_features,
                               .funs = list(poly2 = ~.x**2,
                                            poly3 = ~.x**3,
                                            # poly4 = ~.x**4,
                                            sqrt = ~.x**.5))

## splines
spl <- map(df_all[top_features],
    function(x){
      res <- bs(x, degree = 3)
      res
    }) %>% reduce(cbind)

spl <- data.frame(spl)
colnames(spl) <- flatten_chr(map(top_features,
                ~paste0(.x, paste0("_bs", c(1, 2, 3)))))
df_all <- bind_cols(df_all, spl)

## MODEL==================================================================
# df_all <- df_all %>%
#   mutate_at(top_features[1:5], ~BoxCox(.x, lambda = BoxCox.lambda(.x)))

df_train <- df_all %>% filter(partition == "train") %>% select(Id, SalePrice, everything(), -partition)
df_test <- df_all %>% filter(partition == "test") %>% select(Id, SalePrice, everything(), -partition)
df_train$SalePrice <- log(df_train$SalePrice)

# df_

rec <- recipe(df_train) %>% 
  update_role("SalePrice", new_role = "outcome") %>% 
  update_role("Id", new_role = "id") %>% 
  update_role(-all_outcomes(), -has_role("id"), new_role = "predictor") %>% 
  step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) %>% 
  step_novel(all_nominal()) %>%
  step_ordinalscore("OverallQual") %>% 
  step_dummy(all_nominal())  %>% 
  # step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  step_BoxCox(all_numeric(), -all_outcomes(), -has_role("id")) 

set.seed(123)
sample_train <- sample(1:1456, 1200)
df_tr <- df_train[sample_train, ]
df_ts <- df_train[-sample_train, ]
preped <- prep(rec, df_train)
df_tr_baked <- bake(preped, df_tr)
df_ts_baked <- bake(preped, df_ts)

sum(is.na(df_tr_baked))
sum(is.na(df_ts_baked))

# df_tr_baked$SalePrice <- log(df_tr_baked$SalePrice)
# df_ts_baked$SalePrice <- log(df_ts_baked$SalePrice)
sale_mean <- mean(df_tr_baked$SalePrice)
sale_sd <- sd(df_tr_baked$SalePrice)
df_tr_baked$SalePrice <- (df_tr_baked$SalePrice-sale_mean)/sale_sd

# glmGrid <- expand.grid(alpha = seq(0, .3, .05), lambda = 10^seq(-3, 0, 1))

fit_glm <- caret::train(SalePrice~.-Id, data = df_tr_baked,
                        method = "glmnet",
                        metric = "RMSE",
                        # tuneGrid = glmGrid,
                        tuneLength = 100,
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


fit_glm$bestTune

## log-Inverse
df_ts_baked$SalePriceFit <- predict.train(fit_glm, newdata = df_ts_baked)*sale_sd+sale_mean
# df_ts_baked$SalePrice <- df_ts_baked$SalePrice*sale_sd+sale_mean
RMSE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)

## RMSLE
## .1239208


# df_train %>% ggplot(aes(x = SalePrice, y = SalePriceFit))+geom_point()+geom_abline(slope = 1, col = "red")+geom_smooth()


## SUBMISSION =================================================================
predict.train(fit_glm, newdata = df_test)

if(bc){
df_test$SalePrice <- InvBoxCox(predict.train(fit_glm, newdata = df_test), lambda = bc_lambda)
}else{
df_test$SalePrice <- exp(predict.train(fit_glm, newdata = df_test))
}
submission <- df_test %>% select(Id, SalePrice)
write_csv(x = submission, path = "01_data/02_processed/submission_gam_2107_1.csv")

