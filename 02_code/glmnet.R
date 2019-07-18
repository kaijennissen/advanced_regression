## LIBRARIES ==================================================================
library("tidyverse")
library("lubridate")
library("mgcv")
library("caret")
library("rsample")
library("recipes")
library("glmnet")
# library("MASS")
# library("corrplot")

## DATA PREP ==================================================================
cust_preProcess <- function(df) {
  ## ordinal vars to factor
  df <- df %>% dplyr::rename(FrstFlrSF = `1stFlrSF`, ScndFlrSF = `2ndFlrSF`,
                             ThrdSsnPorch = `3SsnPorch`)
  df$OQualCond <- df$OverallQual*df$OverallQual
  df$OverallQual <-  factor(df$OverallQual, levels = c(1:10), ordered = TRUE)
  df$OverallCond <-  factor(df$OverallQual, levels = c(1:10), ordered = TRUE)
  df$YearsSinceBuilt <- df$YrSold - df$YearBuilt
  df$YearsSinceRemodel <- df$YrSold - df$YearRemodAdd
  df$MoSold <- factor(df$MoSold, levels = c(1:12), ordered = TRUE)
  df$Pool <- ifelse(df$PoolArea > 0, "Yes", "No")
  df$MSSubClass <-   factor(df$MSSubClass)
  df$ExterQual <- factor(df$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$ExterCond <- factor(df$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$EQualCond <- as.numeric(df$ExterQual)*as.numeric(df$ExterQual)
  return(df)
}
df_tr <- read_csv("01_data/01_raw/train.csv")
df_ts <- read_csv("01_data/01_raw/test.csv")
df_tr <- cust_preProcess(df_tr)
df_ts <- cust_preProcess(df_ts)

## SELCET PREDICTORS #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
pred_vars <- setdiff(colnames(df_tr), c("Id", "SalePrice"))
## numeric predictors
num_cat_preds <- c("MSSubClass", "OverallQual", "OverallCond", "MoSold")
num_vars <- colnames(df_tr)[df_tr %>% map_lgl(is.numeric)]
num_pred_vars <- setdiff(intersect(pred_vars, num_vars), num_cat_preds)

## categorical predictors
chr_vars <- colnames(df_tr)[map_lgl(df_tr, is_character)]
cat_pred_vars <- union(intersect(pred_vars, chr_vars), num_cat_preds)
df_tr <- df_tr %>% mutate_at(cat_pred_vars, function(x) {as.factor(x)})
df_ts <- df_ts %>% mutate_at(cat_pred_vars, function(x) {as.factor(x)})

keep_pred_vars <- c("GrLivArea", "OQualCond", "OverallQual", "OverallCond", "Pool", "PoolArea",
                    "MSZoning", "LotArea", "Neighborhood", "YearsSinceBuilt",
                    "EQualCond", "ExterCond", "ExterQual")
keep_pred_vars <- union(num_pred_vars, cat_pred_vars)
# chr_pred <- intersect(pred_vars, chr_vars)
# other_vars <- setdiff(colnames(df_tr), union(c("SalePrice", "Id"), pred_vars))

df_train <- df_tr[c("SalePrice", "Id", keep_pred_vars)]
df_test <- df_ts[c("Id", keep_pred_vars)]

rec <- recipe(df_train) %>%
  ## assign roles
  update_role(SalePrice, new_role = "outcome") %>%
  update_role(Id, new_role = "id") %>%
  update_role(-all_outcomes(), -has_role("id"), new_role = "predictor") %>%
  # step_novel(all_nominal(), new_level = "new") %>%
  step_unknown(all_nominal()) %>% 
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  # step_nzv(all_numeric(), -all_nominal()) %>% 
  step_medianimpute(all_numeric(),  -all_nominal()) %>% 
  step_BoxCox(all_numeric(), all_nominal(), -all_outcomes()) 
  # step_knnimpute(all_numeric())

## test recipe
train_sample <- sample(1:1460, 1000)
train_data <- df_train[train_sample,]
test_data <- df_train[-train_sample,]
rec_prep <- prep(rec, train_data)
df_tr_baked <- bake(rec_prep, train_data)
df_tr_test <- bake(rec_prep, test_data)

## MODEL #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
glmGrid <- expand.grid(alpha = seq(0, 1, .01), lambda = 10^seq(-6, 6, 1))

fit_glm <- caret::train(rec, data = df_train,
                        method = "glmnet", metric = "RMSE",
                        # tuneGrid = glmGrid,
                        trControl = trainControl(method = "cv",
                                                 number = 10,
                                                 # repeats = 10,
                                                 search = "random",
                                                 # returnResamp = "final",
                                                 # summaryFunction = RMSLE,
                                                 verboseIter = TRUE,
                                                 allowParallel = TRUE))
fit_glm$results
fit_glm$bestTune
coef(fit_glm$finalModel, fit_glm$bestTune$lambda)
## RMSE
## xgbTree: 30911.33
## glmnet: 33057.58

## prep prediction set
rec_prep <- prep(rec, df_train)
df_tr_baked <- bake(rec_prep, df_train)
df_ts_baked <- bake(rec_prep, df_test)
df_ts_baked$SalePrice <- predict.train(fit_xgb, newdata = df_ts_baked)

submission <- df_ts_baked %>% select(Id, SalePrice)

write_csv(x = submission, path = "01_data/02_processed/submission_gam.csv")
