###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Present the results
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 04.12.2025
###


# Set the path for the export
path_export <- "N:/durable/FilesYouWantExported/Henrik_Oystein"

# Create directory if it does not yet exist
if(!dir.exists(path_export)) dir.create(path_export)


## Export the figures -------------------------------------------------------------

# Export the figures
figures <- c("km_survival_curve", "lpm_coeff_group_long", "lpm_coeff_group_short",
             "lpm_res_long", "lpm_res_model4_long", "lpm_res_model4_short", "lpm_res_short",
             "cfr_coeff_group_long")
figures <- paste0("figures/", figures, ".svg")

# Create a folder for figures
path_figures <- file.path(path_export, "figures")
if(!dir.exists(path_figures)) dir.create(path_figures)
lapply(figures, file.copy, to = path_figures)


## Export the tables ---------------------------------------------------------------

path_tables <- file.path(path_export, "tables")
if(!dir.exists(path_tables)) dir.create(path_tables)
tables <- list.files("tables", pattern=".xlsx")
lapply(file.path("tables", tables), file.copy, to = path_tables)

results <- list.files("results", pattern=".xlsx")
lapply(file.path("results", results), file.copy, to = path_tables)

results <- list.files("results", pattern=".csv")
lapply(file.path("results", results), file.copy, to = path_tables)


## Export the codes

path_code <- file.path(path_export, "code")
if(!dir.exists(path_code)) dir.create(path_code)
codes <- list.files("code", pattern=".R")
lapply(file.path("code", codes), file.copy, to = path_code)

path_functions <- file.path(path_export, "functions")
if(!dir.exists(path_functions)) dir.create(path_functions)
functionss <- list.files("functions", pattern=".R")
lapply(file.path("functions", functionss), file.copy, to = path_functions)

### END ############################################################################