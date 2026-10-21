###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Robustness checks
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 17.12.2025
###

rm(list=ls()); gc()

library(plm)
library(fixest)
library(data.table)
library(stargazer)
library(stringr)
library(openxlsx)

source("functions/create_table.R")

# Load the analysis data
load("data/df_analysis.Rda")

# Load the graphic template
source("functions/graphic_template.R")

# Define the fixest table
setFixest_etable(
  coefstat="confint",
  ci = 0.95)

# 1. Using same sample for short-time fertility ================================

# Filter the data
df_long <- df_analysis[faar%in%2006:2011 & complete_long==TRUE, ]

# The different models
mod_same_sample1 <- lm(birth ~ any_postnatal, data=df_long)
mod_same_sample2 <- lm(birth ~ any_postnatal + age_group + year_firstbirth, data=df_long)
mod_same_sample3 <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship, data=df_long)
mod_same_sample4 <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship + delivery_mode + any_pregnancy, data=df_long)

# Coolect the models
models_same_sample <- mget(ls(pattern="mod_same_sample\\d"))

# Print the regression table
stargazer(models_same_sample,
          ci=T,
          type="latex", 
          label=paste0("tab:robustness_same_sample"),
          title=paste("Linear regression of the impact of a single diagosis of any diagnosis in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 3 years later using the sample of births between 2008 and 2013. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
         out="tables/robustness_same_sample.tex"
          )
create_table(models_same_sample, out = "tables/robustness_same_sample.xlsx")


# 2. Leave out those with a second child within 21 months ======================

# Filter the data
df_long <- df_analysis[birth_spacing > 21/12 | is.na(birth_spacing), ]

# The different models
mod_long1 <- lm(birth ~ any_postnatal, data=df_long)
mod_long2 <- lm(birth ~ any_postnatal + age_group + year_firstbirth, data=df_long)
mod_long3 <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship, data=df_long)
mod_long4 <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship + delivery_mode + any_pregnancy, data=df_long)

# Coolect the models
models_long <- mget(ls(pattern="mod_long"))

# Print the regression table
stargazer(models_long,
          ci=T,
          type="latex", 
          label=paste0("tab:robustness_long_spacing"),
          title=paste("Linear regression of the impact of a single diagosis of any diagnosis in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 3 years later using the sample of births between 2008 and 2013 removing cases with very short birth intervals (<21 months). Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables."),
          out="tables/robustness_long_birthspacing.tex"
)

create_table(models_long, out = "tables/robustness_long_birthspacing.xlsx")

# 3. Exclude the mental-health conditions =======================================

# Load the condition list
load("data/conversion_codes.Rda")

# Filter the mena
not_mental <- conversion_codes[!str_detect(icpc2, "^P"), ]$icpc2

# Create an alternative predictor for any not-mental health postnatal condition
df_analysis[ , not_mental := sapply(diagnosis_postpartum, function(x) sum(x %in% not_mental))]
df_analysis[ , not_mental := ifelse(not_mental>0, 1, 0)]

# The different models
mod_not_mental1 <- lm(birth ~ not_mental, data=df_analysis)
mod_not_mental2 <- lm(birth ~ not_mental + age_group + year_firstbirth, data=df_analysis)
mod_not_mental3 <- lm(birth ~ not_mental + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship, data=df_analysis)
mod_not_mental4 <- lm(birth ~ not_mental + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship + delivery_mode + any_pregnancy, data=df_analysis)

# Coolect the models
models_not_mental <- mget(ls(pattern="mod_not_mental"))

# Print the regression table
stargazer(models_not_mental,
          ci=T,
          type="latex", 
          label=paste0("tab:robustness_not_mental"),
          title=paste("Linear regression of the impact of any postpartum condition (excluding mental health conditions) in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 3 years later. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables."),
          out="tables/robustness_not_mental.tex"
)

create_table(models_not_mental, out = "tables/robustness_not_mental.xlsx")


# 4. Remove unhealthy women from the sample ====================================

# Load the pre-pregnancy data
source("code/prepregnancy_conditions.R")

# Load the prepregnancy data
load("data/conditions_prepregnancy.Rda")

# Merge with the analysis data
df_healthy <- merge(df_analysis, conditions_prepregnancy, by=c("id_mor_k2_", "faar", "fmnd"))

# Subset the healthy individuals
df_healthy <- df_healthy[conditions==0, ]

# The different models
mod_healthy1 <- lm(birth ~ any_postnatal, data=df_healthy)
mod_healthy2 <- lm(birth ~ any_postnatal + age_group + year_firstbirth, data=df_healthy)
mod_healthy3 <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship, data=df_healthy)
mod_healthy4 <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship + delivery_mode + any_pregnancy, data=df_healthy)

# Coolect the models
models_healthy <- mget(ls(pattern="mod_healthy"))

# Print the regression table
stargazer(models_healthy,
          ci=T,
          type="latex", 
          label=paste0("tab:robustness_healthy"),
          title=paste("Linear regression of the impact of a single diagosis of any diagnosis in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 3 years later using the sample of births between 2008 and 2013 removing cases with any health condition 24 to 9 months before the birth. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          out="tables/robustness_healthy.tex"
)

create_table(models_healthy, out = "tables/robustness_healthy.xlsx")

# 5. Sibling analysis ==========================================================

# Run the sibling analysis
source("code/sibling_identification.R")

# Load the sibling data
load("data/sibling_ids.Rda")

# Load the analysis data
load("data/df_analysis.Rda")

# Merge the sibling and the analysis data
dt_sibling <- merge(dt_sibling, df_analysis, by.x="w19_1011_lnr_k2_", by.y="id_mor_k2_")

# Create the number of observations
dt_sibling[, N:=.N, by=parental_ids]
dt_sibling <- dt_sibling[N>1, ]

# Create a panel data frame
dt_sibling <- pdata.frame(dt_sibling, index=c("parental_ids"))

# Estimate the models
mod_sib_fe1 <- feols(birth ~ any_postnatal | parental_ids, data=dt_sibling)
mod_sib_fe2 <- feols(birth ~ any_postnatal + age_group + year_firstbirth | parental_ids, data=dt_sibling)
mod_sib_fe3 <- feols(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship | parental_ids, data=dt_sibling)
mod_sib_fe4 <- feols(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship + delivery_mode + any_pregnancy | parental_ids, data=dt_sibling)

# Coolect the models
models_siblingFE <- mget(ls(pattern="mod_sib_"))

# Print the regression table
etable(models_siblingFE,
          tex=T, 
          replace=T,
          label=paste0("tab:sibFE_any_diagnosis"),
          title=paste("Linear regression of the impact of a single diagosis of any diagnosis in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 3 years later with sibling fixed effects. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          file="tables/robustness_sibFE_impact_birth_any.tex")


create_table(models_siblingFE, out = "tables/robustness_sibFE_impact_birth_any.xlsx")


### END ########################################################################