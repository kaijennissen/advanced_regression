## LIBRARIES ======================================================================================
library("tidyverse")
library("lubridate")
library("splines")
library("caret")
library("recipes")

## DATA
df_tr <- read_csv("./01_data/01_raw/train.csv") %>% mutate(partition = "train")
df_ts <- read_csv("./01_data/01_raw/test.csv") %>% mutate(partition = "test")

df_all <- bind_rows(df_tr, df_ts)
df_all %>% select(Id, SalePrice, partition, everything())


## MANUEL ENCODING ================================================================================
order_factors <- FALSE
df_all <- df_all %>%
  dplyr::rename(FrstFlrSF = `1stFlrSF`, ScndFlrSF = `2ndFlrSF`, ThrdSsnPorch = `3SsnPorch`) %>%
  dplyr::mutate(
    LotShape = factor(LotShape, levels = c("IR3", "IR2", "IR1", "Reg"), ordered = order_factors),
    Utilities = factor(Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"), ordered = order_factors),
    LandSlope = factor(LandSlope, levels = c("Sev", "Mod", "Gtl"), ordered = order_factors),
    OverallQual = factor(OverallQual, levels = c(1:10), ordered = order_factors),
    OverallCond = factor(OverallCond, levels = c(1:10), ordered = order_factors),
    ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    BsmtQual = factor(BsmtQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    BsmtCond = factor(BsmtCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    BsmtExposure = factor(BsmtExposure, levels = c("No", "Mn", "Av", "Gd"), ordered = order_factors),
    BsmtFinType1 = factor(BsmtFinType1, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = order_factors),
    BsmtFinType2 = factor(BsmtFinType2, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = order_factors),
    HeatingQC = factor(HeatingQC, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    KitchenQual = factor(KitchenQual, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    Functional = factor(Functional, c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"), ordered = order_factors),
    FireplaceQu = factor(FireplaceQu, levels = c("TA", "Gd", "Ex"), ordered = order_factors),
    GarageFinish = factor(GarageFinish, levels = c("Uf", "RFn", "Fin"), ordered = order_factors),
    GarageQual = factor(GarageQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    GarageCond = factor(GarageCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    PavedDrive = factor(PavedDrive, levels = c("N", "P", "Y"), ordered = order_factors),
    PoolQC = factor(PoolQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = order_factors),
    Fence = factor(Fence, levels = c("MnWw", "GdWo", "MnPrv", "GdPrv"), ordered = order_factors),
    MoSold = month(MoSold, label = ),
    MSSubClass = factor(paste0("MS", MSSubClass), ordered = order_factors)
  ) %>%
  dplyr::mutate_if(is_character, .funs = ~ factor(.x))


## VISUAL ANALYSIS ================================================================================
ret_col_type <- function(df) {
  num_vars <- colnames(df_all)[map_lgl(df_all, is.numeric)]
  cat_vars <- colnames(df_all)[map_lgl(df_all, is.factor)]
  return(list(num = num_vars, cat = cat_vars))
}

col_type <- ret_col_type(df_all)
num_vars <- setdiff(col_type$num, c("SalePrice", "partition"))
cat_vars <- setdiff(col_type$cat, c("SalePrice", "partition"))
other_vars <- setdiff(colnames(df_all), union(union(num_vars, cat_vars), c("SalePrice", "partition")))
if (length(other_vars) > 0) {
  print(paste0(other_cols, " are neither numeric nor factors."))
}
pred_vars <- setdiff(colnames(df_all), c("SalePrice", "partition", "Id"))

# for (pred_var in pred_vars) {
#   if (pred_var %in% num_vars) {
#     p <- ggplot(df_all, aes_string(x = pred_var, y = "SalePrice")) +
#       geom_point() +
#       #geom_smooth(method = 'lm') +
#       geom_smooth(method = 'loess')
#   } else if (pred_var %in% cat_vars) {
#     p <- ggplot(df_all, aes_string(x = pred_var, y = "SalePrice")) +
#       geom_boxplot()
#   }
#   print(p)
#   Sys.sleep(5)
# }

df_all %>%
  ggplot(aes(x = OverallQual, y = SalePrice)) +
  geom_point()

## MISSING VALUES =================================================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## REMOVE OUTLIER
# df_all <- df_all %>% filter(GrLivArea < 4000) #%>% select(Id, GrLivArea, SalePrice)
# df_all <- df_all %>% filter(!Id %in% c(524, 1299))

## MISSING VALUES
miss_vals <- map_dfr(df_all, ~ sum(is.na(.x))) %>%
  gather() %>%
  filter(value > 0) %>%
  arrange(desc(value))
miss_vals %>% print(n = 200)

# df_all[is.na(df_all$GarageYrBlt), c("GarageFinish", "GarageFinish", "GarageCond", "GarageQual", "GarageType",
# "GarageCars", "GarageArea", "GarageYrBlt")]

## NA means "No":
na_no <- c(
  "Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "LotFrontage",
  ##
  "GarageFinish", "GarageFinish", "GarageCond", "GarageQual", "GarageType",
  "GarageCars", "GarageArea", "GarageYrBlt",
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

df_tr <- df_all %>% filter(partition == "train")
df_ts <- df_all %>% filter(partition == "test")

df_tr <- df_tr %>%
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
      # x <- replace_na(x, replace = median(x, na.rm = T))
      x <- replace_na(x, replace = getmode(x))
    }
  })


df_ts <- df_ts %>%
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
      # x <- replace_na(x, replace = median(x, na.rm = T))
      x <- replace_na(x, replace = getmode(x))
    }
  })

df_all <- bind_rows(df_tr, df_ts)

map_dfr(df_all, ~ sum(is.na(.x))) %>%
  gather() %>%
  filter(value > 0) %>%
  arrange(desc(value))

## SIMPLE FEATURES ================================================================================
five_level_simple <- c(
  "ExterQual", "ExterCond", "BsmtQual", "KitchenQual",
  "GarageQual", "BsmtCond", "GarageCond", "HeatingQC", "PoolQC"
)
five_level_recode <- function(x) {
  fct_recode(x, bad = "Po", avgerage = "Fa", avgerage = "TA", good = "Gd", good = "Ex")
}

six_level_simple <- c("BsmtFinType1", "BsmtFinType2")
six_level_recode <- function(x) {
  fct_recode(x,
    `1` = "Unf", `1` = "LwQ", `2` = "Rec",
    `2` = "BLQ",
    `3` = "ALQ", `3` = "GLQ"
  )
}

ten_level_simple <- c("OverallQual", "OverallCond")
ten_level_recode <- function(x) {
  fct_recode(x,
    `1` = "1", `1` = "2", `1` = "3", `2` = "4", `2` = "5",
    `2` = "6", `2` = "7", `3` = "8", `3` = "9", `3` = "10"
  )
}

df_all <- df_all %>%
  dplyr::mutate_at(five_level_simple,
    .funs = list(Simple = five_level_recode)
  ) %>%
  dplyr::mutate_at(six_level_simple,
    .funs = list(Simple = six_level_recode)
  ) %>%
  dplyr::mutate_at(ten_level_simple,
    .funs = list(Simple = ten_level_recode)
  ) %>%
  dplyr::mutate_at("Functional",
    .funs = list(Simple = ~ fct_recode(.x,
      `1` = "Sal", `1` = "Sev", `1` = "Maj2",
      `2` = "Maj1", `2` = "Mod", `2` = "Min2",
      `3` = "Min1", `3` = "Typ"
    ))
  )

colnames(df_all) <- str_replace(colnames(df_all), "_", "")

## CREATE NEW FEATURES ============================================================================

##
df_all <-
  df_all %>%
  mutate(
    YearsSinceBuilt = pmax(YrSold - YearBuilt, 0),
    YearsSinceRemodel = pmax(YrSold - YearRemodAdd, 0),
    ExterGrade = as.numeric(ExterCond) * as.numeric(ExterQual),
    ExterGradeSimple = as.numeric(ExterCondSimple) * as.numeric(ExterQualSimple),
    OverallGrade = as.numeric(OverallCond) * as.numeric(OverallQual),
    OverallGradeSimple = as.numeric(OverallCondSimple) * as.numeric(OverallQualSimple),
    GarageGrade = as.numeric(GarageCond) * as.numeric(GarageQual),
    GarageGradeSimple = as.numeric(GarageCondSimple) * as.numeric(GarageQualSimple),
    KitchenScore = KitchenAbvGr * as.numeric(KitchenQual),
    KitchenScoreSimple = KitchenAbvGr * as.numeric(KitchenQualSimple),
    FireplaceScore = Fireplaces * as.numeric(FireplaceQu),
    GarageScore = GarageArea * as.numeric(GarageQual),
    GarageScoreSimple = GarageArea * as.numeric(GarageQualSimple),
    PoolScore = PoolArea * as.numeric(PoolQC),
    PoolScoreSimple = PoolArea * as.numeric(PoolQCSimple),
    TotalBath = BsmtFullBath + .5 * BsmtHalfBath + .5 * HalfBath + FullBath,
    AllSF = GrLivArea + TotalBsmtSF,
    AllFlrsSF = FrstFlrSF + ScndFlrSF,
    AllPorchSF = OpenPorchSF + EnclosedPorch + ThrdSsnPorch + ScreenPorch + WoodDeckSF,
    HasMasVnr = fct_recode(MasVnrType,
      Yes = "BrkCmn", Yes = "BrkFace", Yes = "CBlock",
      Yes = "Stone", No = "None"
    ),
    BoughtOffPlan = fct_recode(SaleCondition,
      Yes = "Partial",
      No = "Abnorml", No = "Alloca", No = "AdjLand", No = "Family",
      No = "Normal"
    ),
    HasPool = ifelse(PoolArea > 0, "Yes", "No"),
    HasScnFloor = ifelse(ScndFlrSF > 0, "Yes", "No"),
    HasGarage = ifelse(GarageArea > 0, "Yes", "No"),
    HasBsmt = ifelse(BsmtFinSF1 > 0, "Yes", "No"),
    HasFireplace = ifelse(Fireplaces > 0, "Yes", "No"),
    Artery = ifelse(Condition1 == "Artery", "Yes", ifelse(Condition2 == "Artery", "Yes", "No")),
    Feedr = ifelse(Condition1 == "Feedr", "Yes", ifelse(Condition2 == "Feedr", "Yes", "No")),
    Norm = ifelse(Condition1 == "Norm", "Yes", ifelse(Condition2 == "Norm", "Yes", "No")),
    RRNn = ifelse(Condition1 == "RRNn", "Yes", ifelse(Condition2 == "RRNn", "Yes", "No")),
    RRAn = ifelse(Condition1 == "RRAn", "Yes", ifelse(Condition2 == "RRAn", "Yes", "No")),
    PosN = ifelse(Condition1 == "PosN", "Yes", ifelse(Condition2 == "PosN", "Yes", "No")),
    PosA = ifelse(Condition1 == "PosA", "Yes", ifelse(Condition2 == "PosA", "Yes", "No")),
    RRNe = ifelse(Condition1 == "RRNe", "Yes", ifelse(Condition2 == "RRNe", "Yes", "No")),
    RRAe = ifelse(Condition1 == "RRAe", "Yes", ifelse(Condition2 == "RRAe", "Yes", "No"))
  ) %>%
  dplyr::mutate_if(is_character, .funs = ~ factor(.x))


cond_vals <- union(levels(df_all$Condition1), levels(df_all$Condition2))

# map(cond_vals, {
# df_all %>%
#     mutate(
#       !!.x := ifelse(Condition1 == "Artery", "Yes",
#                    ifelse(Condition2 == "Artery", "Yes", "No"))
#     )
#   }
# )

# predictor types
col_type <- ret_col_type(df_all)

num_pred_vars <- setdiff(col_type$num, c("SalePrice", "partition", "Id"))
cat_pred_vars <- setdiff(col_type$cat, c("SalePrice", "partition", "Id"))
other_vars <- setdiff(colnames(df_all), union(union(num_pred_vars, cat_pred_vars), c("SalePrice", "partition", "Id")))
if (length(other_vars) > 0) {
  print(paste0(other_cols, " are neither numeric nor factors."))
}

## TOP FEATURES BASED ON DIFFERENT CORRELATION COEFFICIENTS
cor_type <- "pearson" ## pearson, spearman, kendall
cor_df <- df_all[!is.na(df_all$SalePrice), c("SalePrice", num_pred_vars)]
cor_vals <- sort(cor(cor_df, method = cor_type)[1, ], decreasing = T)
top_features <- setdiff(
  names(cor_vals)[abs(cor_vals) > .55],
  "SalePrice"
)

## POLYNOMIAL TERMS
# df_all <- df_all %>%
#   dplyr::mutate_at(top_features, .funs = list(
#     poly2 = ~ .x**2,
#     poly3 = ~ .x**3,
#     poly4 = ~.x**4,
#     sqrt = ~ .x**.5,
#     log = ~log(.x)
#   ))

## SPLINES
# spl <- map(
#   df_all[top_features],
#   function(x) {
#     res <- bs(x, degree = 5)
#     res
#   }
# ) %>% reduce(cbind)
#
# spl <- data.frame(spl)
# colnames(spl) <- flatten_chr(map(
#   top_features,
#   ~ paste0(.x, paste0("_bs", c(1, 2, 3)))
# ))
# df_all <- bind_cols(df_all, spl)

# write_csv2(df_all, "./01_data/02_processed/train_test_stacked.csv")

## FEATURE ENGINEERING =========== ================================================================
ret_col_type <- function(df) {
  num_vars <- colnames(df_all)[map_lgl(df_all, is.numeric)]
  cat_vars <- colnames(df_all)[map_lgl(df_all, is.factor)]
  return(list(num = num_vars, cat = cat_vars))
}
col_type <- ret_col_type(df_all)
num_vars <- setdiff(col_type$num, c("SalePrice", "partition"))
cat_vars <- setdiff(col_type$cat, c("SalePrice", "partition"))
other_vars <- setdiff(colnames(df_all), union(union(num_vars, cat_vars), c("SalePrice", "partition", "Id")))
if (length(other_vars) > 0) {
  print(paste0(other_vars, " are neither numeric nor factors."))
}
pred_vars <- setdiff(colnames(df_all), c("SalePrice", "partition", "Id"))

# for (pred_var in pred_vars) {
#   if (pred_var %in% num_vars) {
#     p <- ggplot(df_all, aes_string(x = pred_var, y = "SalePrice")) +
#       geom_point() +
#       #geom_smooth(method = 'lm') +
#       geom_smooth(method = 'loess')
#   } else if (pred_var %in% cat_vars) {
#     p <- ggplot(df_all, aes_string(x = pred_var, y = "SalePrice")) +
#       geom_boxplot()
#   }
#   print(p)
#   Sys.sleep(2)
# }

num_cat_vars <- map(df_all, ~ {
  if (is.factor(.x)) {
    ret_val <- !all(is.na(as.numeric(levels(.x))))
  } else {
    ret_val <- FALSE
  }
  return(ret_val)
})

num_cat_vars <- colnames(df_all)[num_cat_vars == 1]

# df_all <- df_all %>% mutate_at(num_cat_vars, ~{as.numeric(.x)})


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


nn <- nrow(df_train)
set.seed(791)
sample_train <- sample(nn, round(nn * .75))
df_tr <- df_train[sample_train, ]
df_ts <- df_train[-sample_train, ]
preped <- prep(rec, df_tr)
df_tr <- juice(preped)
df_ts <- bake(preped, df_ts)
df_tt <- bake(preped, df_train)
df_test <- bake(preped, df_test)

sale_mean <- mean(df_tr$SalePrice)
sale_sd <- sd(df_tr$SalePrice)
df_tr$SalePrice <- (df_tr$SalePrice - sale_mean) / sale_sd


## ELASTIC NET REGRESSION =========================================================================
glmGrid <- expand.grid(alpha = seq(0, 1, .1), lambda = 10^seq(-4, 1, 1))

fit <- caret::train(SalePrice ~ . - Id,
  data = df_tr,
  method = "gp",
  metric = "RMSE",
  # tuneLength = 10,
  tuneGrid = glmGrid,
  trControl = trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final",
    search = "grid",
    verboseIter = TRUE,
    allowParallel = TRUE
  )
)

fit$bestTune

df_ts$SalePriceFit_enet <- predict.train(fit, newdata = df_ts) * sale_sd + sale_mean
RMSE(pred = df_ts$SalePriceFit_enet, obs = df_ts$SalePrice)

# df_tt$SalePriceFit_enet <- predict.train(fit, newdata = df_tt) * sale_sd + sale_mean
# RMSE(pred = df_tt$SalePriceFit_enet, obs = df_tt$SalePrice)
# 
# write_csv(
#   x = dplyr::select(df_tt, Id, starts_with("SalePrice")),
#   path = "./01_data/03_predictions/01_train/enet_prediction.csv"
# )


df_test$SalePriceFit_enet <- exp(predict.train(fit, newdata = df_test) * sale_sd + sale_mean)

df_submission <- df_test %>%
  mutate(SalePrice = SalePriceFit_enet) %>% 
  dplyr::select(Id, SalePrice)

write_csv(
  x = df_submission,
  path = "./01_data//03_prediction/03_submission/enet_prediction.csv"
)











#########################
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
