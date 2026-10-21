###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: diagnosis incidence around pregnancy
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 28.11.2025
###

library(data.table)
library(tidyverse)
library(zoo)

# Load the paths
source("paths.R")

# Load the parameters
source("parameters.R")

# Load the data ================================================================

# List the data files
load("data/mfr_cleaned.Rda")

# Remove cases without ID
dt_mfr <- dt_mfr[id_mor_k2_!="", ]

# Recode the livebirth category
#dt_mfr[, livebirth:=ifelse(livebirth_cat=="still alive", 1, 0)]

# Estimate the total number of life births for indviiduals
dt_mfr[, n_births:=sum(livebirth, na.rm=T), by=id_mor_k2_]

# Filter the MFR grid
dt_mfr <- dt_mfr[faar>=2006 & id_mor_k2_ != "", ]

# Get the ids
sample_ids <- unique(dt_mfr$id_mor_k2_[dt_mfr$parity5_m==0 & dt_mfr$livebirth==1])
save(sample_ids, file="data/sample_ids.Rda")

# 1. Estimate the birth spacing ================================================

# Outcomes:
# 1. Time to next birth
# 2. Time to any other birth outcome

# Filter those births that have a first live birth in the observation period
dt_mfr <- dt_mfr[id_mor_k2_ %in% sample_ids, ]

# Select the most important variables
dt_mfr <- dt_mfr[order(id_mor_k2_, faar, parity5_m), .(id_mor_k2_, faar, fmnd, age_m, plural, parity5_m, livebirth, stillbirth, abortion, n_births, delivery_mode)]

# Create the month variable
dt_mfr[ , birth_month := as.yearmon(paste(faar, fmnd, sep = "-"))]

# Estimate the time difference to the next event
dt_mfr[, time_difference := shift(birth_month, type="lead")-birth_month, by=id_mor_k2_]

# Estimate the time difference to the next event
dt_mfr[livebirth==1, birth_spacing := shift(birth_month, type="lead")-birth_month, by=id_mor_k2_]

# Save the data file 
save(dt_mfr, file="data/birth_data.Rda")

# 2. Prepare the monthly grid ==================================================

# Remove the birth spacing variables
dt_mfr[, `:=`(time_difference=NULL, birth_spacing=NULL, delivery_mode=NULL)]

# Create the grid data
for (i in horizon_start:horizon_end) {
  dt_mfr[, paste("pregnancy", i, sep="_") := birth_month + i/12] 
}

# Melt the data
mfr_grid <- dt_mfr[faar>2006, ]
mfr_grid <- melt(dt_mfr, id.vars=c("id_mor_k2_", "faar", "fmnd", "birth_month", "age_m", "plural", "livebirth", "stillbirth", "abortion", "parity5_m", "n_births"), variable.name = "month", value.name="date")
mfr_grid <- mfr_grid[, time := as.numeric(str_remove(month, "pregnancy_"))]

# Order the data
mfr_grid <- mfr_grid[order(id_mor_k2_, date), ]

# Create the month and year for the spell in the mfr grid
mfr_grid[, month:=month(date)]
mfr_grid[, year:= year(date)]

# Save the MFR file
save(mfr_grid, file = "data/mfr_grid.Rda")

### END ########################################################################