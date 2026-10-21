###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: diagnosis incidence around pregnancy
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 28.11.2025
###

# Regional codes for the big cities
region_code_oslo <- "0301"
region_code_stavanger <- "1103"
region_code_trondheim <- "5001"
region_code_bergen <- "4601"

# Collect the region codes
regional_codes <- mget(ls(pattern="^region_code"))

### END ##################################################