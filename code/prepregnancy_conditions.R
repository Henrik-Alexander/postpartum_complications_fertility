###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Pre-pregnancy conditions
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 19.12.2025
###


library(data.table)

# Load the analysis data
load("data/df_analysis.Rda")

# Load the KUHR-data
load("data/kuhr_mothers_2006+.Rda")

# Load the conversion codes
load("data/conversion_codes.Rda")

# Load the birth grid
load("data/mfr_grid.Rda")

## Prepare the data ------------------------------------------------------------

# Subset the number of individuals in the mfr grid
mfr_grid <- mfr_grid[mfr_grid$id_mor_k2_ %in% unique(df_analysis$id_mor_k2_), ]
kuhr <- kuhr[lopenr_k2_ %in% unique(df_analysis$id_mor_k2_), ]

# Create a list of conditions per id, month, year
kuhr <- kuhr[, .(diagnose=list(unique(diagnose))), by=.(lopenr_k2_, year, month)]

# Merge the data
mfr_grid <- merge(mfr_grid, kuhr, by.x=c("id_mor_k2_", "year", "month"), by.y=c("lopenr_k2_", "year", "month"), all.x=T)

# Filter the time points
mfr_grid <- mfr_grid[time < -9, ]

# Create a length variable
mfr_grid[, n_cond := sapply(diagnose, length)]

# Aggregate the conditions
conditions_prepregnancy <- mfr_grid[, .(n_cond=sum(n_cond), conditions = ifelse(sum(n_cond)>0, 1, 0)), by=.(id_mor_k2_, faar, fmnd)]

# Save the data
save(conditions_prepregnancy, file="data/conditions_prepregnancy.Rda")

### END #########################################################################