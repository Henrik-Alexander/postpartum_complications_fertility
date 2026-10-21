###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Prepare the analysis data
# Load the conversion codes
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 03.12.2025
###

rm(list=ls()); gc(TRUE)

library(data.table)
library(tidyverse)
library(zoo)
library(mgcv)

# Load the parameters
source("parameters.R")

# Load the data ================================================================

# Load the graphic template
source("functions/graphic_template.R")

# Load the paths
source("paths.R")

# Load the KUHR-data
load("data/kuhr_mothers_2006+.Rda")

# Load the conversion codes
load("data/conversion_codes.Rda")

# Load the birth grid
load("data/birth_data.Rda")

# Load the sample ids
load("data/sample_ids.Rda")

# Load the maternal grid
load("data/mfr_grid.Rda")

# Load the corresponding disease codes
source("disease_codes.R")

# 1. Identify individuals with postpartum complications =========================

## 1.1. Prepare the pregnancy complications ------------------------------------

# Select the relevant spells in the kuhr data
kuhr_pregnancy <- kuhr[lopenr_k2_%in% sample_ids & diagnose %in% icpc2_pregnancy, ]

# Subset those first births
pregnancy_grid <- mfr_grid[id_mor_k2_ %in% sample_ids & time %in% -9:-1 & parity5_m==0, .(id_mor_k2_, year, month)]

# Create a disease_number
kuhr_pregnancy <- kuhr_pregnancy[order(lopenr_k2_, year, month)]

# Merge the kuhr long data with the grid
kuhr_pregnancy_long <- merge(kuhr_pregnancy, pregnancy_grid, by.x=c("lopenr_k2_", "year", "month"), by.y=c("id_mor_k2_", "year", "month"))
save(kuhr_pregnancy_long, file = "data/disease_pregnancy.Rda")

# Aggregate the diseases
pregnancy_diagnosis <- kuhr_pregnancy_long[, .(diagnosis=list(diagnose)), by=.(lopenr_k2_)]
rm(kuhr_pregnancy, pregnancy_grid, kuhr_pregnancy_long, kuhr_any_pregnancy); gc()

## 1.3 Prepare the postpartum data --------------------------------------------

# Select the relevant spells in the kuhr data
kuhr <- kuhr[lopenr_k2_%in% sample_ids & diagnose %in% icpc2_pattern, ]

# Subset those first births
postnat_grid <- mfr_grid[id_mor_k2_ %in% sample_ids & time %in% start_postnat:end_postnat & parity5_m==0, .(id_mor_k2_, year, month) ]

# Create a disease_number
kuhr <- kuhr[order(lopenr_k2_, year, month)]

# Merge the kuhr long data with the grid
kuhr_long <- merge(kuhr, postnat_grid, by.x=c("lopenr_k2_", "year", "month"), by.y=c("id_mor_k2_", "year", "month"))
save(kuhr_long, file = "data/disease_postnat.Rda")

# Aggregate the diseases
postnat_diagnosis <- kuhr_long[, .(diagnosis=list(diagnose)), by=.(lopenr_k2_)]
rm(kuhr, kuhr_long); gc()

# 2. Merge the birth data ======================================================

load("data/birth_data.Rda")

# Filter the first births in the MFR data
dt_mfr <- dt_mfr[parity5_m==0, ]

# Merge with the postpartum grid
df_analysis <- merge(dt_mfr, postnat_diagnosis, by.x="id_mor_k2_", by.y = c("lopenr_k2_"), all.x=T)

# Merge the pregnancy grid
df_analysis <- merge(df_analysis, pregnancy_diagnosis, by.x="id_mor_k2_", by.y = c("lopenr_k2_"), all.x=T, suffixes=c("_postpartum", "_pregnancy"))

# Filter the birth years
df_analysis <- df_analysis[faar %in% min_year:max_year, ]

# Create an indicator for a subsequent birth in the episode
df_analysis[, birth:=ifelse(!is.na(birth_spacing) & birth_spacing<=follow_up_short, 1, 0)]

# Create binary for any postpartum complications
df_analysis[, n_postnatal := sapply(diagnosis_postpartum, length)]
df_analysis[, n_pregnancy := sapply(diagnosis_pregnancy, length)]

# Create binary for any postpartum complications
df_analysis[, any_postnatal:= ifelse(n_postnatal>0, 1, 0)]
df_analysis[, any_pregnancy := ifelse(n_pregnancy>0, 1, 0)]

# Create age categories
df_analysis[, age_group := cut(age_m, breaks=c(0,  seq(20, 40, by=5), 50, 60))]

# Truncate the number of disease at five data
df_analysis[, postpartum_complications3 := cut(n_postnatal, breaks=c(0, 0.5, 1, 2, 100), include.lowest=T, labels = c("0", "1", "2", "3+"))]

# Create year at first birth
df_analysis[, year_firstbirth:=factor(faar)]

# 5. Include the regstat data ==================================================

# Load the registration data
load("data/ssb_regstat.Rda")

# Merge analysis and registration data
dt_regstat <- merge(dt_regstat, df_analysis[, .(id_mor_k2_, faar)], by.x=c("lopenr_k2_"), by.y=c("id_mor_k2_"))

# Sort the data
dt_regstat <- dt_regstat[order(lopenr_k2_, year), ]

# Filter the data
dt_regstat <- dt_regstat[year>=faar-2 & year<=faar+follow_up_long, ]

# Filter the observation periods
dt_regstat[, short_observation := ifelse(regstat==1 & year >= faar-2 & year <= faar+follow_up_short, 1, 0)]
dt_regstat[, long_observation := ifelse(regstat==1 & year >= faar-2 & year <= faar+follow_up_long, 1, 0)]

# Create a binary indicator
dt_regstat <- dt_regstat[, .(count_obs_short = sum(short_observation, na.rm=T), count_obs_long = sum(long_observation, na.rm=T)), by=lopenr_k2_]

# Show the completeness short and long: number of records must equal the observation period length (2 years before birth + birthyear + follow-ep time)
dt_regstat <- dt_regstat[, .(lopenr_k2_, complete_short = count_obs_short==(follow_up_short+3), complete_long = count_obs_long==(follow_up_long+3))]

# Merge the data to the analysis data
df_analysis <- merge(df_analysis, dt_regstat, by.x="id_mor_k2_", by.y="lopenr_k2_", all.x=T)

# 6. Merge the covariate data =================================================

# Load the covariates
load("data/covariates.Rda")

# Merge the covariates
df_analysis <- merge(df_analysis, covariates, by.x=c("id_mor_k2_", "faar"), by.y=c("lopenr_k2_", "birthyear"), all.x=T)

# Sample selection =============================================================

# Count the problematic cases
n_plural <- sum(df_analysis$plural>1)
n_migrated <- sum(!df_analysis$complete_short)

# Remove those who are not
df_analysis <- df_analysis[complete_short==TRUE, ]

# Remove any plural births
df_analysis <- df_analysis[plural==1, ]

# Remove those who are coded as men
df_analysis <- df_analysis[sex==2, ]

# Missing analysis =============================================================

# Collect the total number of missing values
tot_miss <- apply(df_analysis, 2, function(x) sum(is.na(x)))
prop_miss <- apply(df_analysis, 2, function(x) mean(is.na(x)))

# Define the covariates
covariates <- c("birth", "any_postnatal", "age_group", "year_firstbirth", "migrant_cat", "income_decile", "edu_level", "relationship", "delivery_mode", "any_pregnancy")

# REmove the missing observations
df_analysis <- df_analysis[rowSums(is.na(df_analysis[, ..covariates]))==0, ]

# Save analysis data
save(df_analysis, file="data/df_analysis.Rda")

### END ########################################################################