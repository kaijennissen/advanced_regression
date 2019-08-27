## LIBRARIES ====================================================
library("tidyverse")
library("lubridate")
library("splines")

rm(list=ls())
## DATA
df_tr <- read_csv("01_data/01_raw/train.csv") %>% mutate(partition = "train")
df_ts <- read_csv("01_data/01_raw/test.csv") %>% mutate(partition = "test")
df_all <- bind_rows(df_tr, df_ts)

## MANUEL ENCODING ============================================================
ordered_factors = FALSE
df_all <- df_all %>% 
  dplyr::rename(FrstFlrSF = `1stFlrSF`, ScndFlrSF = `2ndFlrSF`, ThrdSsnPorch = `3SsnPorch`) %>% 
  dplyr::mutate(
    LotShape = factor(LotShape, levels = c("IR3", "IR2", "IR1","Reg"), ordered = ordered_factors), 
    Utilities = factor(Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"), ordered = ordered_factors), 
    LandSlope = factor(LandSlope, levels = c("Sev", "Mod", "Gtl"), ordered = ordered_factors), 
    OverallQual = factor(OverallQual, levels = c(1:10), ordered = ordered_factors), 
    OverallCond = factor(OverallCond, levels = c(1:10), ordered = ordered_factors), 
    ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    BsmtQual = factor(BsmtQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    BsmtCond = factor(BsmtCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    BsmtExposure = factor(BsmtExposure, levels = c("No", "Mn", "Av", "Gd"), ordered = ordered_factors), 
    BsmtFinType1 = factor(BsmtFinType1, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = ordered_factors), 
    BsmtFinType2 = factor(BsmtFinType2, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = ordered_factors), 
    HeatingQC = factor(HeatingQC, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    KitchenQual = factor(KitchenQual, c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    Functional = factor(Functional, c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2",  "Min1", "Typ"), ordered = ordered_factors), 
    FireplaceQu = factor(FireplaceQu, levels = c("TA", "Gd", "Ex"), ordered = ordered_factors), 
    GarageFinish = factor(GarageFinish, levels = c("Uf", "RFn", "Fin"), ordered = ordered_factors), 
    GarageQual = factor(GarageQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    GarageCond = factor(GarageCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    PavedDrive = factor(PavedDrive, levels = c("N", "P", "Y"), ordered = ordered_factors), 
    PoolQC = factor(PoolQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = ordered_factors), 
    Fence = factor(Fence, levels = c("MnWw", "GdWo", "MnPrv", "GdPrv"), ordered = ordered_factors), 
    MoSold = month(MoSold, label = TRUE), 
    MSSubClass = factor(paste0("MS", MSSubClass), ordered = ordered_factors)
    ) %>% 
  dplyr::mutate_if(is_character, .funs = ~factor(.x)) 


## MISSING VALUES =============================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## REMOVE OUTLIER
# df_all <- df_all %>% filter(GrLivArea < 4000) #%>% select(Id, GrLivArea, SalePrice)
df_all <- df_all %>% filter(!Id %in% c(524, 1299))

## MISSING VALUES
miss_vals <- map_dfr(df_all, ~sum(is.na(.x))) %>% gather() %>% 
  filter(value > 0) %>% arrange(desc(value))
# miss_vals %>% print(n = 200)

# df_all[is.na(df_all$GarageYrBlt), c("GarageFinish", "GarageFinish", "GarageCond", "GarageQual", "GarageType",
       # "GarageCars", "GarageArea", "GarageYrBlt")]

## NA means "No": 
na_no <- c("Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "LotFrontage",
           ##
           "GarageFinish", "GarageFinish", "GarageCond", "GarageQual", "GarageType",
           "GarageCars", "GarageArea", "GarageYrBlt",
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
    # x <- replace_na(x, replace = median(x, na.rm = T))
    x <- replace_na(x, replace = getmode(x))
  }
}) 

map_dfr(df_all, ~sum(is.na(.x))) %>% gather() %>%
  filter(value > 0) %>% arrange(desc(value))

## SIMPLE FEATURES ============================================================
five_level_simple <- c("ExterQual", "ExterCond", "BsmtQual", "KitchenQual",
                     "GarageQual", "BsmtCond", "GarageCond", "HeatingQC", "PoolQC")
five_level_recode <- function(x){
  fct_recode(x, bad = "Po", avgerage = "Fa", avgerage = "TA", good = "Gd", good = "Ex")
}

six_level_simple <- c("BsmtFinType1", "BsmtFinType2")
six_level_recode <- function(x){
  fct_recode(x, `1` = "Unf", `1` = "LwQ", `2` = "Rec", 
             `2` = "BLQ",
             `3` = "ALQ", `3` = "GLQ")
}

ten_level_simple <- c("OverallQual", "OverallCond")
ten_level_recode <- function(x){
  fct_recode(x, `1`= "1", `1`= "2", `1`= "3", `2`= "4", `2`= "5",
             `2`= "6", `2`= "7", `3`= "8", `3`= "9", `3`= "10")
}

         
df_all <- df_all %>% 
  dplyr::mutate_at(five_level_simple,
                   .funs = list(Simple = five_level_recode)) %>%
  dplyr::mutate_at(six_level_simple,
                   .funs = list(Simple = six_level_recode)) %>%
  dplyr::mutate_at(ten_level_simple,
                   .funs = list(Simple = ten_level_recode)) %>%
  dplyr::mutate_at("Functional",
                   .funs = list(Simple = ~fct_recode(.x, `1` = "Sal", `1` = "Sev", `1` = "Maj2",
                                          `2` = "Maj1", `2` = "Mod", `2` = "Min2",	
                                          `3` = "Min1", `3` = "Typ")) 
                     )

colnames(df_all) <- str_replace(colnames(df_all), "_", "")

## NEW FEATURES ===============================================================

## SIMPLE FEATURES
df_all <-
  df_all %>% 
  mutate(
    YearsSinceBuilt = pmax(YrSold - YearBuilt, 0),
    YearsSinceRemodel = pmax(YrSold - YearRemodAdd, 0),
    ExterGrade = as.numeric(ExterCond)*as.numeric(ExterQual),
    ExterGradeSimple = as.numeric(ExterCondSimple)*as.numeric(ExterQualSimple),
    OverallGrade = as.numeric(OverallCond)*as.numeric(OverallQual),
    OverallGradeSimple = as.numeric(OverallCondSimple)*as.numeric(OverallQualSimple),
    GarageGrade = as.numeric(GarageCond)*as.numeric(GarageQual),
    GarageGradeSimple = as.numeric(GarageCondSimple)*as.numeric(GarageQualSimple),
    KitchenScore = KitchenAbvGr*as.numeric(KitchenQual),
    KitchenScoreSimple = KitchenAbvGr*as.numeric(KitchenQualSimple),
    FireplaceScore = Fireplaces*as.numeric(FireplaceQu),
    GarageScore = GarageArea*as.numeric(GarageQual),
    GarageScoreSimple = GarageArea*as.numeric(GarageQualSimple),
    PoolScore = PoolArea*as.numeric(PoolQC),
    PoolScoreSimple = PoolArea*as.numeric(PoolQCSimple),
    # FireplaceScoreSimple = Fireplaces*as.numeric(FireplaceQuSimple),
    TotalBath = BsmtFullBath+.5*BsmtHalfBath+.5*HalfBath+FullBath,
    AllSF = GrLivArea+TotalBsmtSF,
    AllFlrsSF = FrstFlrSF+ScndFlrSF,
    AllPorchSF = OpenPorchSF + EnclosedPorch + ThrdSsnPorch + ScreenPorch+ WoodDeckSF,
    HasMasVnr =  fct_recode(MasVnrType, Yes = "BrkCmn", Yes = "BrkFace", Yes = "CBlock",
                            Yes = "Stone", No = "None"),
    BoughtOffPlan = fct_recode(SaleCondition, Yes = "Partial", 
                               No = "Abnorml", No = "Alloca",  No = "AdjLand", No = "Family",
                               No = "Normal"),
    HasPool = ifelse(PoolArea > 0, "Yes", "No"),
    HasScnFloor = ifelse(ScndFlrSF > 0, "Yes", "No"),
    HasGarage = ifelse(GarageArea > 0, "Yes", "No"),
    HasBsmt = ifelse(BsmtFinSF1 > 0, "Yes", "No"),
    HasFireplace = ifelse(Fireplaces > 0, "Yes", "No")
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

## TOP FEATURES BASED ON DIFFERENT CORRELATION COEFFICIENTS
cor_type = "spearman"

cor_df <- df_all[!is.na(df_all$SalePrice), c("SalePrice", num_pred_vars)]
cor_vals <- sort(cor(cor_df, method = cor_type)[1,], decreasing = T)
top_features <- setdiff(names(cor_vals)[abs(cor_vals)>.5],
                        "SalePrice")


## POLYNOMIAL TERMS
df_all <- df_all %>%
  mutate_at(top_features, .funs = list(poly2 = ~.x**2,
                                       poly3 = ~.x**3,
                                       # poly4 = ~.x**4,
                                       sqrt = ~.x**.5))

## SPLINES
# spl <- map(df_all[top_features],
#     function(x){
#       res <- bs(x, degree = 5)
#       res
#     }) %>% reduce(cbind)
# 
# spl <- data.frame(spl)
# colnames(spl) <- flatten_chr(map(top_features,
#                 ~paste0(.x, paste0("_bs", c(1, 2, 3)))))
# df_all <- bind_cols(df_all, spl)


write_csv2(df_all, "./01_data/02_processed/stacked_features.csv")
