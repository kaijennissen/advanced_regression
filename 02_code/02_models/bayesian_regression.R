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

params <- SuppDists::JohnsonFit(df_all$SalePrice[!is.na(df_all$SalePrice)])

# df_all %>% 
#   filter(!is.na(SalePrice)) %>%
#   ggplot(aes(x = SalePrice)) + 
#   ggplot2::geom_histogram(aes(y = ..density..), bins = 30) +
#   ggplot2::stat_function(fun = dnorm, color = "red",
#                          args = list(mean = mean(df_all$SalePrice, na.rm = T),
#                                      sd = sd(df_all$SalePrice, na.rm = T)))+
#   ggplot2::stat_function(fun = dlnorm, color = "blue", 
#                          args = list(meanlog = mean(log(df_all$SalePrice), na.rm = T),
#                                      sdlog = sd(log(df_all$SalePrice), na.rm = T)))+
#   ggplot2::stat_function(fun = SuppDists::dJohnson, color = "green",
#                          args = list(params))

df_train <- df_all %>% 
  filter(partition == "train") %>% 
  select(Id, SalePrice, everything(), -partition)

# Johnson_params <- Johnson::RE.Johnson(df_all$SalePrice[!is.na(df_all$SalePrice)])
# df_train$SalePrice <- Johnson_params$transformed
# df_train$SalePrice <- log(df_train$SalePrice)

df_test <- df_all %>% 
  filter(partition == "test") %>% 
  select(Id, SalePrice, everything(), -partition)
   
rec <- recipe(df_train) %>% 
  update_role("SalePrice", new_role = "outcome") %>% 
  update_role("Id", new_role = "id") %>%
  update_role(-all_outcomes(), -has_role("id"), new_role = "predictor") %>% 
  # step_YeoJohnson("SalePrice") %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal())  %>% 
  step_nzv(all_numeric(), -all_outcomes(), -has_role("id")) #%>%
  # step_BoxCox(all_numeric(), -all_outcomes(), -has_role("id")) %>%
  # step_center(all_predictors()) %>%
  # step_scale(all_predictors())

sample_train <- sample(1:1456, 1000)
df_tr <- df_train[sample_train, ]
df_ts <- df_train[-sample_train, ]
preped <- prep(rec, df_train)
df_tr_baked <- bake(preped, df_tr)
df_ts_baked <- bake(preped, df_ts)

yj_obj <- yeojohnson(df_tr_baked$SalePrice, standardize = TRUE)
df_tr_baked$SalePrice <- predict(yj_obj , df_tr_baked$SalePrice)
df_ts_baked$SalePrice <- predict(yj_obj , df_ts_baked$SalePrice)

# map_lgl(df_tr_baked, ~sum(is.na(.x))>0) %>% keep(~.x == TRUE)
# sum(is.na(df_tr_baked))
# sum(is.na(df_ts_baked))

sale_mean <- 0#mean(df_tr_baked$SalePrice)
sale_sd <- 1#sd(df_tr_baked$SalePrice)
# df_tr_baked$SalePrice <- (df_tr_baked$SalePrice-sale_mean)/sale_sd

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

RMSLE <- function(pred, obs){
  RMSLE <- RMSE(pred = log(pred), obs = log(obs), na.rm = T)
  return(RMSLE)
}
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
               , standardize = TRUE
               , lambda.min.ratio = 1e-3
               # , lambda = exp(seq(-6, 30, .5))
               , nlambda = 100
               , nfolds = 10
               )

y_pred <- predict(fit_oem, newx = X_new)
y_pred <- predict(yj_obj , y_pred, inverse = TRUE)
y_new  <- predict(yj_obj , y_new, inverse = TRUE)

resu <- RMSLE(obs = y_new, pred = y_pred)
# print(paste0("RMSE for i = ", hyper, " is: ", resu))
return(resu)
} 

hyper <- expand.grid(model = c("grp.lasso.net"),
                hyper = seq(.1, .9, .1)) %>% as_tibble() %>% mutate(model = as.character(model))
hyper <- hyper %>% dplyr::mutate(rmsle = map2(model, hyper, fit_oem_fun)) %>% unnest(rmsle)
hyper %>% arrange(rmsle)

fit_oem <- cv.oem(x = X, y = y
                  , penalty = "grp.lasso.net"
                  , groups = group1
                  , alpha = .4
                  , compute.loss = TRUE
                  # , standardize = TRUE
                  , lambda.min.ratio = 1e-3
                  , nlambda = 100
                  , nfolds = 10
)
plot(fit_oem)

df_ts_baked$SalePriceFit <- predict(fit_oem, newx = X_new)*sale_sd+sale_mean
df_ts_baked$SalePriceFit <- predict(yj_obj , df_ts_baked$SalePriceFit, inverse = TRUE)
df_ts_baked$SalePrice <- predict(yj_obj , df_ts_baked$SalePrice, inverse = TRUE)

RMSLE(pred = df_ts_baked$SalePriceFit, obs = df_ts_baked$SalePrice)

df_ts_baked %>% 
  ggplot(aes(x = SalePrice, y = SalePriceFit)) + 
  geom_point() + 
  geom_abline(slope = 1, col = "red") + 
  geom_smooth()




library(extrafont)

download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest="xkcd.ttf", mode="wb")
system("mkdir ~/.fonts")
system("cp xkcd.ttf  ~/.fonts")
font_import(paths = "~/.fonts", pattern="[X/x]kcd")
fonts()
loadfonts()


p7 <- ggplot(airquality, aes(x = Ozone)) +
  geom_histogram(aes(y = ..count..), binwidth = 10,
                 colour = "black", fill = "#56B4E9") +
  scale_x_continuous(name = "Mean ozone in\nparts per billion",
                     breaks = seq(0, 175, 25),
                     limits=c(0, 175)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Frequency histogram of mean ozone") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 20, family="xkcd-Regular"),
        text=element_text(size = 16, family="xkcd-Regular"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))
p7

  
## SUBMISSION =================================================================

preped <- prep(rec, df_train)
df_tr_baked <- bake(preped, df_train)
df_ts_baked <- bake(preped, df_test)

yj_obj <- yeojohnson(df_tr_baked$SalePrice, standardize = TRUE)
df_tr_baked$SalePrice <- predict(yj_obj , df_tr_baked$SalePrice)
# df_ts_baked$SalePrice <- predict(yj_obj , df_ts_baked$SalePrice)

# sale_mean <- mean(df_tr_baked$SalePrice)
# sale_sd <- sd(df_tr_baked$SalePrice)
# df_tr_baked$SalePrice <- (df_tr_baked$SalePrice-sale_mean)/sale_sd

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

y_pred <- predict(fit_oem, newx = X_pred)
df_ts_baked$SalePrice <- predict(yj_obj , y_pred, inverse = TRUE)
# df_ts_baked$SalePrice <- exp(predict(fit_oem, newx = X_pred)*sale_sd+sale_mean)
submission <- df_ts_baked %>% select(Id, SalePrice)
write_csv(x = submission, path = "01_data/02_processed/submission_grp_lasse_0108_1.csv")



abc <- boxplot.default(df_train$LotFrontage, range = 3)$out

setdiff(df_train$LotFrontage, boxplot.default(df_train$LotFrontage, range = 3)$out)
step_


