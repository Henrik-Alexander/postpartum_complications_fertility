###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Event-history analysis
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 04.12.2025
###

library(data.table)
library(openxlsx)
library(survival)
library(tidyverse)
library(ranger)
library(broom)
library(stargazer)

# Load the paths
source("paths.R")

source("functions/create_table.R")

# Load the parameters
source("parameters.R")

# Load the functions
source("functions/helper.R")

# Load the analysis data
load("data/df_analysis.Rda")

# Load the grpahic template
source("functions/graphic_template.R")

## Prepare the slekt data ------------------------------------------------------
# Slekt = Kinship information

# Load the slect data
slekt <- fread(file.path(path_rawdata, "original_data/k2_-w19_1011_4d_slektsopplysning.txt"))

# Transform the birth dates from parents into Date-format
slekt[, mor_fodtaar := trans_int_date(mor_fodtaar)]
slekt[, far_fodtaar := trans_int_date(far_fodtaar)]

## Prepare the faste data ------------------------------------------------------
# faste = family information

# Load the faste data
faste <- fread(file.path(path_rawdata, "Sept_SSB2024/k2_-w19_1011_FASTE_OPPL_IKKEDATO.csv"))

# Select the important variables
faste <- faste[, .(foedsels_aar_mnd, doeds_aar_mnd, kjoenn, w19_1011_lnr_k2_, lopenr_far_k2_, lopenr_mor_k2_)]

# Create a date
faste[, foedsels_aar_mnd := trans_int_date(foedsels_aar_mnd)]

## Combine faste and slekt data ------------------------------------------------

# Merge the faste and slekt data via lnr
df <- merge(slekt, faste, by = "w19_1011_lnr_k2_", all = TRUE)

# Estimate the order and parity
df <- df[order(lopenr_mor_k2_, foedsels_aar_mnd),]
df[, birth_order := seq_len(.N), by = lopenr_mor_k2_]

# Filter the individuals
df <- df[lopenr_mor_k2_ != "" & !is.na(lopenr_mor_k2_)]

# Create mother's age
df[, age_mother := as.numeric(difftime(foedsels_aar_mnd, mor_fodtaar))/365.25]

# Select the important variables
df <- df[, .(lopenr_mor_k2_, age_mother, birth_order, foedsels_aar_mnd, w19_1011_lnr_k2_ )]

# Reshape wide: per mother all information on one row, if there is a second birth
df <- dcast(data = df, formula = "lopenr_mor_k2_  ~ birth_order", value.var = c("age_mother", "foedsels_aar_mnd", "w19_1011_lnr_k2_"))

# Filter the observations with first births after 2006
df <- df[year(foedsels_aar_mnd_1) >= min_year, ]

# Create birth month for the first birth
df[, foedsels_aar_mnd_1 := fcase(!is.na(foedsels_aar_mnd_1), foedsels_aar_mnd_1,
                                 is.na(foedsels_aar_mnd_1), as.Date(paste0("01", str_remove(birth_month, " ")), format = "%d%b%Y"))]

# Select the important columns
df <- df[, .(lopenr_mor_k2_, age_mother_1, age_mother_2, foedsels_aar_mnd_1, foedsels_aar_mnd_2)]

# Merge df analysis and the event-history-data
df_analysis <- merge(df_analysis, df, by.x = "id_mor_k2_", by.y = "lopenr_mor_k2_", all.x=T)

# Create the last observation year
df_analysis$last_observation <- as.Date("2023-12-31")

# Create survival time for censored cases
df_analysis[, time := fcase(!is.na(foedsels_aar_mnd_2), foedsels_aar_mnd_2 - foedsels_aar_mnd_1,
                            is.na(foedsels_aar_mnd_2) & is.na(dod), last_observation- foedsels_aar_mnd_1,
                            is.na(foedsels_aar_mnd_2) & !is.na(dod), dod - foedsels_aar_mnd_1)]



# Create the absence of an event
df_analysis[, birth := ifelse(is.na(foedsels_aar_mnd_2), 0, 1)]

## Descriptives

# Kaplan Meier Survival curve
km <- Surv(df_analysis$time, df_analysis$birth)
km_fit <- survfit(Surv(time, birth) ~ 1, data = df_analysis)

# Summarize the KM model
summary(km_fit, times = 1:10 * 365)
plot(km_fit)

km_fit <- survfit(Surv(time, birth) ~ any_postnatal, data = df_analysis)

# Summarize the KM model
summary(km_fit, times = 1:10 * 365)
plot(km_fit)
abline(h=0.18)

# Create a data.frame
surv_curve <- data.frame(
  time = km_fit$time,
  n = km_fit$n.risk,
  survival = km_fit$surv,
  lower = km_fit$lower,
  upper = km_fit$upper,
  strata = c(rep("No", km_fit$strata[1]), rep("Yes", km_fit$strata[2]))
)

# Plot the survival curve with uncertainty intervals
ggplot(surv_curve, aes(x=time/(12 * 31), y = survival, group = strata, colour = strata)) +
  geom_ribbon(aes(ymin = lower, ymax= upper, fill = strata), alpha = 0.3) +
  geom_line() +
  scale_x_continuous("Survival time (in years)", expand = c(0, 0), n.breaks = 10) +
  scale_y_continuous("Proportion surviving", labels = scales::percent, n.breaks = 10, limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_viridis_d("Postnatal-health complications:") +
  scale_colour_viridis_d("Postnatal-health complications:")
ggsave(filename = "figures/km_survival_curve.svg", height = 12, width = 15, unit = "cm")

## Run simple cox-regressions

# Bivariate model
cox1 <- coxph(Surv(time, birth) ~ any_postnatal, data = df_analysis)

# With demographic controls
cox2 <- coxph(Surv(time, birth) ~ any_postnatal + age_group + year_firstbirth, data = df_analysis)

# With socio-economic controls
cox3 <- coxph(Surv(time, birth) ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship, data = df_analysis)

# With birth controls
cox4 <- coxph(Surv(time, birth) ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship + delivery_mode + any_pregnancy, data = df_analysis)

# collect the cox models
cox_models <- mget(ls(pattern = "cox[0-9]"))

stargazer(cox_models,
          apply.coef = exp,
          apply.ci = exp,
          type = "latex", title = "Results from cox regression", style = "demography",
          out = "tables/cox_regression_any_postnatal.tex")
create_table(cox_models, out = "tables/cox_regression_any_postnatal.xlsx", exp = TRUE)


### END ########################################################################