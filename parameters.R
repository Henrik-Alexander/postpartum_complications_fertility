###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Set the parameters
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 09.12.2025
###

# 1. Load MFR grid =============================================================

horizon_start <- -24
horizon_end <- 24

# 2. Prepare analysis data =====================================================

# Start of the postnatal period
start_postnat <- 0
end_postnat <- 12

# The first year of birth included in the data
min_year <- 2006

# The last year of birth included in the data
max_year <- 2019

# The time until when a birth should be observed
follow_up_short <- 3
follow_up_long <- 10

# 3. Disease codes =============================================================

# Should the pregnancy be considered as period as well
pregnancy_period <- FALSE

# Should codes from the data-driven approach be included?
data_driven <- TRUE

# Should the expert-opinion be included as well?
expert_selection <- TRUE

# Collect the parameters
parameters <- c(pregnancy_period, data_driven, expert_selection)

# Pick a cut-off for the data driven approach
cutoff_datadriven <- 1.5

### END ########################################################################