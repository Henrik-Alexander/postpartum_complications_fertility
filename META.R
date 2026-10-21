###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Meta-file
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 28.11.2025
###

rm(list=ls()); gc(TRUE)

# Create the folder structure ==================================================

folders <- c("code", "analysis", "functions", "data", "results", "functions", "tables")
lapply(folders, function(folder) if(!dir.exists(folder)) dir.create(folder))
rm(folders)

# Load the codes ===============================================================

# This file prepares the medical birth register data
source("code/01_prepare_mfr_grid.R")

# This file prepares the KUHR-data
source("code/02_prepare_kuhr.R") # Slow -> does not have to be run again!

# This file merges the different covariates
source("code/03_prepare_covariates.R")

# This file estimates the perinatal incidence 
source("code/04_perinatal_incidence.R") # Slow -> does not have to be run again!

# This file analyzes the incidence of different conditions in pregnancy and afterwards
source("code/05_postnatal_complications.R")

# This file merges the birth data (code1), the kuhr data (code 2) and the covaraites (code3)
source("code/06_prepare_analysis_data.R")

# This file estimates the different regression models
source("code/07_regression_modelling.R")

# This file prepares the results
source("code/08_result_presentation.R")

# Run the robustness checks
source("code/09_robustness_checks.R")

# Run the event-history model
source("code/event_history_analysis.R")


### END ########################################################################