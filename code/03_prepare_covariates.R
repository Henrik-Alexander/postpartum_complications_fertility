###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: diagnosis incidence around pregnancy
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 28.11.2025
###

rm(list=ls()); gc(T)

library(data.table)
library(tidyverse)
library(zoo)

# Load the helper functions
source("functions/helper.R")

# Load the sample IDs
load("data/sample_ids.Rda")

# Load the birth data
load("data/birth_data.Rda")

# 1. Load the death data -------------------------------------------------------

# Column names:
# lopenr_k2_ = ID
# dob = date of birth
# dod = date of death
# sex = sex
# cob = country of birth
# migrant_category = migrant_category
# first_registration = first registration

# Load the population registration data
load("data/population.Rda")

# Select the basic demographic information
dt_demo <- dt_regstat[w19_1011_lnr_k2_ %in% sample_ids, .(w19_1011_lnr_k2_, foedsels_aar_mnd, doeds_aar_mnd, kjoenn, fodeland, invkat, forstdato)]

# Rename the columns
setnames(dt_demo, old=names(dt_demo), new=c("lopenr_k2_", "dob", "dod", "sex", "cob", "migrant_cat", "first_registration"))

# Create the dates
dt_demo[, dob := trans_int_date(dob)]
dt_demo[, dod := trans_int_date(dod)]
dt_demo[, first_registration:=trans_int_date(first_registration)]

# Clean the migrant category
dt_demo[, migrant_cat:=fcase(migrant_cat %in% c("A", "E", "F", "G"), "Norwegian",
                             migrant_cat %in% c("B"), "Migrant",
                             migrant_cat %in% c("C"), "Migration background")]

# Save the death data
save(dt_demo, file="data/demographics.Rda")

## Create the covariate data-file ==============================================

# Select the information for the sample
covariates <- dt_mfr[id_mor_k2_ %in% sample_ids & parity5_m == 0, .(lopenr_k2_ = id_mor_k2_, birthyear=faar, covariate_year=faar-2)]
rm(dt_mfr); gc()

# 1. Migration background ======================================================

# Merge the demographic data on top
covariates <- merge(covariates, dt_demo, by="lopenr_k2_", all.x=T)

# 2. Prepare the income data ===================================================

# Load the income data
load("data/income_data.Rda")

# Select the mothers
dt_inc <- dt_inc[lopenr_k2_ %in% sample_ids, ]

# Make the income data to a factor
dt_inc[, income_decile:=factor(income_quartile)]

# Merge the income data to the covariates
covariates <- merge(covariates, dt_inc, by.x=c("lopenr_k2_", "covariate_year"), by.y=c("lopenr_k2_", "year"), all.x=T)
rm(dt_inc); gc(T)

# 3. Load the educational data =================================================

# Load the educational data
load("data/education_levels_1970-2018.Rda")

# Filter the sample ids
dt_edu <- dt_edu[year>=2004 & w19_1011_lnr_k2_ %in% sample_ids, ]

# Combine the different educational levels
dt_edu[, edu_level := fcase(edu_level <= 2, "Basic",
                      edu_level >= 3 & edu_level <= 5, "Secondary",
                      edu_level >= 6, " Tertiary")]


# # Clean the edu level
# dt_edu[, edu_level := fcase(edu_level == 0, "No or only pre-school",
#                             edu_level == 1, "Primary school",
#                             edu_level == 2, "Secondary school",
#                             edu_level == 3, "Secondary school, basic",
#                             edu_level == 4, "Secondary school, final",
#                             edu_level == 5, "Extension to higher education",
#                             edu_level == 6, "University and college, lower",
#                             edu_level == 7, "University and college, higher",
#                             edu_level == 8, "Researcher")]


# Merge the educational data to the covariate data
covariates <- merge(covariates, dt_edu[, .(w19_1011_lnr_k2_, year, edu_level)], by.x=c("lopenr_k2_", "covariate_year"), by.y=c("w19_1011_lnr_k2_", "year"), all.x=T)
rm(dt_edu); gc(TRUE)

# 4. Load the Residence data ===================================================

# Load the educational data
load("data/residence_municipality.Rda")
load("data/municipality_context_data.Rda")

# Filter the sample ids
dt_resmun <- dt_resmun[year>=2004 & w19_1011_lnr_k2_ %in% sample_ids, ]

# Merge the context data on to the municipality data
dt_resmun <- merge(dt_resmun, mun_agg[, .(res_municipality, year, pop_size)], by = c("res_municipality", "year"), all.x=T)

# Regional codes for the big cities
region_code_oslo <- "0301"; region_code_stavanger <- "1103"
region_code_trondheim <- "5001"; region_code_bergen <- "4601"

# Collect the region codes
regional_codes <- mget(ls(pattern="^region_code"))

# Create indicator for Oslo, Bergen and Trondheim and small and larger communicties
dt_resmun[, mun_cat := ordered(fcase(pop_size < 5000 & !(res_municipality %in% regional_codes), "small rural municipality (pop<5,000)",
                                   pop_size >= 5000 & pop_size < 20000 & !(res_municipality %in% regional_codes), "medium rural municipality (pop=5,000-20,000)",
                                   pop_size >= 20000 & !(res_municipality %in% regional_codes), "large rural municipality (>20,000)",
                                   res_municipality %in% regional_codes[names(regional_codes) != "region_code_oslo"], "Bergen, Stavanger, Trondheim, Oslo",
                                   res_municipality == regional_codes$region_code_oslo, "Oslo"))]

# Merge the residence data to the covariate data
covariates <- merge(covariates, dt_resmun[, .(w19_1011_lnr_k2_, year, mun_cat)], by.x=c("lopenr_k2_", "covariate_year"), by.y=c("w19_1011_lnr_k2_", "year"), all.x=T)
rm(dt_resmun, regional_codes); gc(TRUE)

# 5. Partnership & Housing data ================================================

# Load the partnership and housing data
load("data/relationship_housing.Rda")

# Filter the mothers
fam_hus <- fam_hus[w19_1011_lnr_k2_ %in% sample_ids, ]

# Create relationship form
fam_hus[, relationship := fcase(str_detect(family_form, "^Married"), "Married",
                                    str_detect(family_form, "^Cohabi"), "Cohabitation",
                                    str_detect(family_form, "^Single"), "Single",
                                    str_detect(family_form, "^Other"), "Other")]

# Increase the year by one to account for the shorter duration of the data type
fam_hus[, year:=year-1]

# Merge the family-housing data to the covariate data
covariates <- merge(covariates, fam_hus, by.x=c("lopenr_k2_", "covariate_year"), by.y=c("w19_1011_lnr_k2_", "year"), all.x=T)
rm(fam_hus); gc(TRUE)

# Save the covariate data
save(covariates, file = "data/covariates.Rda")

### END ########################################################################