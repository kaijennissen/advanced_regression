## LIBRARIES ====================================================
library("tidyverse")
library("lubridate")
library("mgcv")
library("caret")
library("rsample")
library("recipes")
library("glmnet")
# library("MASS")
library("corrplot")


## DATA
data <- read_csv("01_data/01_raw/train.csv")
data %>% glimpse()
data <- data %>% rename(FrstFlrSF = `1stFlrSF`, ScndFlrSF = `2ndFlrSF`,
                      ThrdSsnPorch = `3SsnPorch`)

## VISUAL INSPECTION OF PREDICTORS =========================================

pred_vars <- setdiff(colnames(data), c("Id", "SalePrice"))

## numeric predictors
num_cat_preds <- c("MSSubClass", "OverallQual", "OverallCond", "MoSold")
num_vars <- colnames(data)[data %>% map_lgl(is.numeric)]
num_pred_vars <- setdiff(intersect(pred_vars, num_vars), num_cat_preds)

## categorical predictors
chr_vars <- colnames(data)[data %>% map_lgl(is_character)]
cat_pred_vars <- union(intersect(pred_vars, chr_vars), num_cat_preds )
data <- data %>% mutate_at(cat_pred_vars, function(x) {as.factor(x)})

cor_mat <- round(cor(data[, c("SalePrice", num_pred_vars)]), 2)
corrplot(cor_mat[,order(cor_mat[1,], decreasing = T)], type = "upper", tl.srt = 45, )

for (i in num_pred_vars){
  p <- data %>% 
    ggplot(aes_string(x = i, y = "SalePrice"))+
    geom_point()
  print(p)
  Sys.sleep(5)
}

for (i in cat_pred_vars){
  p <- data %>% 
    ggplot(aes_string(x = i, y = "SalePrice"))+
    geom_boxplot()
  print(p)
  Sys.sleep(1)
}

data %>% 
  ggplot(aes(x = log(SalePrice)))+
  geom_histogram()

## t-test
data %>% 
  ggplot(aes(x = Street, y = log(SalePrice)))+
  geom_boxplot()

t.test(log(SalePrice)~Street, data = data,
       var.equal = FALSE, alternative = "less")

na_vars <- data %>% map_dbl(~sum(is.na(.x)))
data %>% glimpse()

# reg_formula <- SalePrice ~ GrLivArea#+TotalBsmtSF
reg_formula <- SalePrice ~ .
lm_fit <- lm(reg_formula, data = data)
summary(lm_fit)

data %>% 
  ggplot(aes(x = GrLivArea, y = SalePrice))+
  geom_point()+
  geom_abline(aes(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2]),
              col = "blue", size = 1.5, alpha = .9)+
  geom_abline(aes(intercept = coef(lm_fit2)[1], slope = coef(lm_fit2)[2]),
              col = "red", size = 1.5, alpha = .9)


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





