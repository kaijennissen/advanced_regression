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
    OverallQual = factor(OverallQual, levels = c(1:10), ordered = TRUE),
    OverallCond = factor(OverallCond, levels = c(1:10), ordered = TRUE),
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

pred_vars <- setdiff(colnames(df_all), c("Id", "SalePrice", "partition"))

## numeric predictors
num_vars <- colnames(df_all)[map_lgl(df_all, is.numeric)]
num_pred_vars <- intersect(intersect(pred_vars, num_vars), pred_vars)

## categorical predictors
chr_preds <- intersect(colnames(df_all)[map_lgl(df_all, is_character)], pred_vars)
df_all <- df_all %>% mutate_at(chr_preds, function(x) {as.factor(x)})
cat_pred_vars <- colnames(df_all)[map_lgl(df_all, is.factor)]


## MISSING VALUES =============================================================
## remove outliers
df_all %>% filter(GrLivArea > 4000) %>% select(Id, GrLivArea, SalePrice)
df_all <- df_all %>% filter(!Id %in% c(524, 1299))

## missing values 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## BsmtExposure, Electrical and MasVnrType have real missing values, 
## impute with most frequent value
## numeric LotFrontage, GarafeYrBlt, 
miss_vals <- map_dfr(df_all, ~sum(is.na(.x))) %>% gather() %>% 
  filter(value > 0) %>% arrange(desc(value))

# cat_miss_vals <- intersect(miss_vals[["key"]], cat_pred_vars)
# num_miss_vals <- intersect(miss_vals[["key"]], num_pred_vars)

## all Garage related features
## "GarageYrBlt", "GarageFinish", "GarageFinish", "GarageQual", "GarageType",
## "GarageCars", "GarageArea",
# no_garage <- is.na(df_all[["GarageYrBlt"]])
# df_all[no_garage, "GarageFinish"] <- "No"
# df_all[no_garage, "GarageQual"] <- "No"
# df_all[no_garage, "GarageType"] <- "No"
# df_all[no_garage, "GarageCars"] <- 0
# df_all[no_garage, "GarageArea"] <- 0
# df_all[no_garage, "GarageYrBlt"] <- 1978

## all Basement related features
## "BsmtCond", "BsmtExposure", "BsmtQual", "BsmtFinType2", "BsmtFinType1"
## "BsmtFullBath", "BsmtHalfBath", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF",
# df_all %>% select( TotalBsmtSF, starts_with("Bsmt")) %>%
#   filter(is.na(BsmtQual)&is.na(BsmtCond)) %>% print(n = 85)
# no_bsmt <- is.na(df_tr[["BsmtQual"]]) & is.na(df_tr[["BsmtCond"]])
# df_all[no_bsmt, "BsmtCond"] <- "No"
# df_all[no_bsmt, "BsmtQual"] <- "No"
# df_all[no_bsmt, "BsmtFinType1"] <- "No"
# df_all[no_bsmt, "BsmtFinType2"] <- "No"
# df_all[no_bsmt, "BsmtExposure"] <- "No"
# df_all[no_bsmt, "BsmtFullBath"] <- 0
# df_all[no_bsmt, "BsmtFinSF1"] <- 0
# df_all[no_bsmt, "BsmtFinSF2"] <- 0
# df_all[no_bsmt, "BsmtUnfSF"] <- 0
# df_all[no_bsmt, "TotalBsmtSF"] <- 0

## remaining missing values indicate that features ist not present
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


## NEW FEATURES  ===========================================================

cor_mat <- round(cor(df_all[!is.na(df_all$SalePrice), c("SalePrice", num_pred_vars)]), 2)
# corrplot(cor_mat[,order(cor_mat[1,], decreasing = T)], type = "upper", tl.srt = 45, )

top_features <- setdiff(colnames(cor_mat[,order(cor_mat[1,], decreasing = T)]),
                        "SalePrice")[1:15]

df_all %>% map_dfr(~sum(is.na(.x))) %>% gather() %>% 
  filter(value > 0) %>% arrange(desc(value))
## recipes

df_all <- df_all %>% mutate_at(top_features,
                               .funs = list(poly2 = ~.x**2,
                                            poly3 = ~.x**3,
                                            sqrt = ~.x**.5))

spl <- map(df_all[c("GrLivArea", "TotalBsmtSF")],
    function(x){
      res <- bs(x)  
      res
    }
    ) %>% reduce(cbind) 

colnames(as.data.frame(spl)) 
map(c("GrLivArea", "TotalBsmtSF"),
    ~paste0(.x, paste0("_bs", c(1, 2, 3)))) 

s(df_all$GrLivArea)~

df_all <- df_all %>% 
  mutate(
    YearsSinceBuilt = YrSold - YearBuilt,
    YearsSinceRemodel = YrSold - YearRemodAdd,
    EQualCond = as.numeric(ExterQual)*as.numeric(ExterQual),
    OQualCond <- as.numeric(OverallQual)*as.numeric(OverallQual),
    Pool = ifelse(PoolArea == 0, "No", "Yes")
    )


## build new features
# df_tr$OverallQual <-  factor(df_tr$OverallQual, levels = c(1:10), ordered = TRUE)
# df_tr$OverallCond <-  factor(df_tr$OverallQual, levels = c(1:10), ordered = TRUE)
# df_tr$MoSold <- factor(df_tr$MoSold, levels = c(1:12), ordered = TRUE)
# df_tr$ExterQual <- factor(df_tr$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"),
#                           ordered = TRUE)
# df_tr$ExterCond <- factor(df_tr$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"),
#                           ordered = TRUE)

## 
## add splines
# xx <- bs(df_all$LotFrontage, degree = 3)
# colnames(xx) <- c("a", "B", "c")
# data.frame(xx, col.names = paste0("LotFrontage_", paste0("bs", c(1:3))))
# bind_cols(df_all, data.frame(xx))

# cor_mat <- round(cor(df_tr[, c("SalePrice", num_pred_vars)]), 2)
# corrplot(cor_mat[,order(cor_mat[1,], decreasing = T)], type = "upper", tl.srt = 45, )

# for (i in num_pred_vars){
#   p <- df_tr %>%
#     ggplot(aes_string(x = i, y = "SalePrice"))+
#     geom_point()
#   print(p)
#   Sys.sleep(5)
# }

#   df_plot <- df_tr %>% select(ends_with("Cond"), SalePrice) %>% mutate(SalePrice = log(SalePrice))
#   cols <- colnames(df_plot)[df_plot %>% map_lgl(is_character)]
# for (i in cols){
#   p <- df_plot %>% 
#     ggplot(aes_string(x = i, y = "SalePrice"))+
#     geom_boxplot()
#   print(p)
#   Sys.sleep(5)
# }

# rec <- recipe(df_tr) %>%
#   ## assign roles
#   update_role(SalePrice, new_role = "outcome") %>%
#   update_role(Id, new_role = "id") %>%
#   update_role(-all_outcomes(), -has_role("id"), new_role = "predictor") %>%
#   # step_poly(GrLivArea)
#   step_novel(all_nominal(), new_level = "new") %>%
#   step_other(all_nominal(), threshold = .1) %>%
#   step_unknown(all_nominal()) %>%
#   step_normalize(all_predictors(), -all_nominal()) %>%
#   step_dummy(all_nominal(), one_hot = FALSE) %>%
#   step_nzv(all_predictors(), -all_nominal()) %>%
#   # step_center(all_predictors(), -all_nominal()) %>%
#   # step_scale(all_predictors(), -all_nominal()) %>% 
#   step_BoxCox(all_predictors(), -all_nominal())
#   # step_pca(all_predictors(), -all_nominal())
#   # step_medianimpute(all_numeric(),  -all_nominal()) %>% 

## test recipe
# train_sample <- sample(1:1460, 1000)
# train_data <- df_tr[train_sample,]
# test_data <- df_tr[-train_sample,]
# rec_prep <- prep(rec, train_data)
# df_tr_baked <- bake(rec_prep, train_data)
# df_tr_test <- bake(rec_prep, test_data)

glmGrid <- expand.grid(alpha = seq(0, 1, .1), lambda = 10^seq(-3, 3, 1))

df_train <- df_all %>% filter(partition == "train")
df_test <- df_all %>% filter(partition == "test")

fit_glm <- caret::train(SalePrice~.-Id, data = df_train,
                        method = "glmnet",
                        metric = "RMSE",
                        tuneGrid = glmGrid,
                        trControl = trainControl(
                          method = "cv",
                          number = 10,
                          # repeats = 10,
                          search = "grid",
                          # returnResamp = "final",
                          # summaryFunction = RMSLE,
                          verboseIter = TRUE,
                          allowParallel = TRUE)
                        )

# fit_glm[4]$recipe$steps
fit_glm$results %>% head()
coef(fit_glm$finalModel, fit_glm$bestTune$lambda)

df_test$SalePrice <- predict.train(fit_glm, newdata = df_test)
submission <- df_test %>% select(Id, SalePrice)
write_csv(x = submission, path = "01_data/02_processed/submission_gam_1907.csv")


## prep prediction set
rec_prep <- prep(rec, df_tr)
df_tr_baked <- bake(rec_prep, df_tr)
# df_ts_baked <- bake(rec_prep, df_test)
predict.train(fit_glm, newdata = df_tr_baked, )

fit_glm$control


inverse.BoxCoxTrans <- function(object, newdata) {
  if(!is.vector(newdata) || !is.numeric(newdata)) stop("newdata should be a numeric vector")
  if(is.na(object$lambda)) return(newdata) 
  
  lambda <- object$lambda
  if(lambda < object$fudge & lambda > -object$fudge)
    lambda <- 0
  else if(lambda < 1+object$fudge & lambda > 1-object$fudge)
    lambda <- 1
  
  if(lambda == 0) exp(newdata) else (lambda*newdata + 1)^(1/lambda) 
}





# 















## MODELLING ===============================================================
cv_eval <- function(y, y_hat){
  MAE <- mean(abs(y_hat-y)) #MAE
  RMSE <- sqrt(mean((y_hat-y)^2)) #RMSE
  RMSLE <-  sqrt(mean((log(y_hat)-log(y))^2)) #RMSLE
  MAPE <- mean(abs(y_hat-y)/y) #MAPE
  return(c(MAE, RMSE, RMSLE, MAPE))
}
cor_mat[,order(cor_mat[1,], decreasing = T)]
# missing_values <- train_data %>% map(~sum(is.na(.x)))
# missing_values$LotFrontage

## regression equation
reg_formula <- SalePrice ~ GrLivArea+TotalBsmtSF

n <- dim(data)[1]
cv_repeats <- 10
cv_split <- split(sample(seq(n), replace = FALSE), 1:cv_repeats)
cv_mat <- matrix(NA, nrow = cv_repeats, ncol = 8)
colnames(cv_mat) <- 
  map(list(a = "MAE",b = "RMSE", c = "RMSLE", d = "MAPE"),
          function(x, y) {
            xx <- paste(x, y, sep = "_")
            return(xx)
          }, y = c("lm", "rlm")) %>% flatten_chr()
  
for (i in 1:cv_repeats){
 train_data <- data[flatten_dbl(cv_split[-i]),]
  test_data <- data[flatten_dbl(cv_split[i]),]
  y <- X_train$SalePrice
  
  lm_fit <- lm(reg_formula, data = train_data)
  y_hat_lm <- predict(lm_fit, test_data)
  cv_mat[i, c(1,3,5,7)] <- cv_eval(y = y, y_hat = y_hat_lm)
  
  rlm_fit <- rlm(reg_formula, data = train_data)
  y_hat_rlm <- predict(rlm_fit, test_data)
  cv_mat[i, c(2,4,6,8)] <- cv_eval(y = y, y_hat = y_hat_rlm)
  }
  
cv_resu <- cv_mat %>% as_tibble() %>% summarise_all(mean) %>% 
  gather() %>% 
  separate(key, c("METRIC", "METHOD")) %>%
  spread("METRIC", "value")
cv_resu





## HETROSCEDASTICITY
## 1
summary(lm(log(resu^2)~log(data$GrLivArea)))
## 2
summary(lm(log(resu^2)~data$GrLivArea))
resu_fit <- lm(log(resu^2)~data$GrLivArea)
## 3
summary(lm(resu^2~data$GrLivArea))
resu_fit <- lm(resu^2~data$GrLivArea)
lm_wght <- coef(resu_fit)[2]*data$GrLivArea^.5

lm_fit2 <- lm(reg_formula, data = data, weights = lm_wght)
summary(lm_fit2)

data %>% 
  ggplot(aes(x = GrLivArea, y = SalePrice))+
  geom_point()+
  geom_abline(aes(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2]),
              col = "blue", size = 1.5, alpha = .9)+
  geom_abline(aes(intercept = coef(lm_fit2)[1], slope = coef(lm_fit2)[2]),
              col = "red", size = 1.5, alpha = .9)





