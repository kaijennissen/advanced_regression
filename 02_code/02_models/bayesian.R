## LIBRARIES ====================================================
library("tidyverse")
library("lubridate")
library("mgcv")
library("caret")
library("rsample")
library("recipes")
library("glmnet")
library("boomspikeslab")
library("rjags")
library("")

rm(list=ls())
## DATA
df_al <- read_csv2("./01_data/02_processed/stacked_features.csv")

## bsts ======================================================================








