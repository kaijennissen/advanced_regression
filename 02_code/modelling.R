## LIBRARIES =====================================================
library("tidyverse")
library("lubridate")
library("mgcv")
library("caret")
library("rsample")
library("recipes")
library("glmnet")
# library("MASS")
library("corrplot")

## CARET =============================================================

cust_preProcess <- function(data) {
  data$OverallQual <- data$OverallQual %>% factor(levels = c(1:10), ordered = TRUE)
}

df_tr <- read_csv("01_data/01_raw/train.csv")
df_ts <- read_csv("01_data/01_raw/test.csv")

dat_caret %>% glimpse()
dat_caret <- dat_caret %>% rename(FrstFlrSF = `1stFlrSF`, ScndFlrSF = `2ndFlrSF`,
                                  ThrdSsnPorch = `3SsnPorch`)

pred_vars <- setdiff(colnames(dat_caret), c("Id", "SalePrice"))

## numeric predictors
num_cat_preds <- c("MSSubClass", "OverallQual", "OverallCond")
num_vars <- colnames(dat_caret)[dat_caret %>% map_lgl(is.numeric)]
num_pred_vars <- setdiff(intersect(pred_vars, num_vars), num_cat_preds)


## categorical predictors
chr_vars <- colnames(dat_caret)[dat_caret %>% map_lgl(is_character)]
cat_pred_vars <- union(intersect(pred_vars, chr_vars), num_cat_preds )
data <- dat_caret %>% mutate_at(cat_pred_vars, function(x) {as.factor(x)})

dat_caret <- dat_caret %>% dplyr::select(Id, SalePrice, OverallQual, Street, GrLivArea)

dat_caret$OverallQual <- dat_caret$OverallQual %>% factor(levels = c(1:10), ordered = TRUE)

rec <- recipe(dat_caret) %>%
  ## assign roles
  update_role(SalePrice, new_role = "outcome") %>%
  update_role(GrLivArea, Street, OverallQual, new_role = "predictor") %>%
  update_role(Id, new_role = "id") %>% 
  update_role(-all_predictors(), -all_outcomes(), -has_role("id"),
              new_role = "other") %>% 
  step_novel(Street, OverallQual, new_level = "new") %>% 
  step_string2factor(Street) %>%
  # step_num2factor(OverallQual, levels = c(1:10),
  # ordered = TRUE) %>%
  step_dummy(Street, OverallQual, one_hot = FALSE)

train_sample <- sample(1:1460, 100)
train_data <- dat_caret[train_sample,]
test_data <- dat_caret[-train_sample,]
rec_prep <- prep(rec, train_data)
tr_data <- bake(rec_prep, train_data)
ts_data <- bake(rec_prep, test_data)
# ts_data %>% select(Id,OverallQual)
ts_data$OverallQual
# rec_juice %>%
#   dplyr::select(Id, SalePrice, LotFrontage,
#                 starts_with("Street"), starts_with("OverallQual")) %>%
#   summarize_all(~sum(is.na(.x))) %>% t()

fit_xgb <- caret::train(rec, data = dat_caret,
                        method = "xgbTree",
                        metric = "RMSE",
                        trControl = trainControl(
                          method = "repeatedcv",
                          number = 10,
                          repeats = 10, 
                          search = "random",
                          returnResamp = "final",
                          # summaryFunction = RMSLE,
                          verboseIter = TRUE,
                          allowParallel = TRUE))
fit_xgb


