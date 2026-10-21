###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: prepare the KUHR-data
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 28.11.2025
###

library(data.table)
library(haven)

# Load the paths
source("paths.R")

# Load the sample ids: Women with a firstbirth after 2006
load("data/sample_ids.Rda")

# Load the conversion codes
load(file.path(path_henriks_project, "data/conversion_codes.Rda"))

# Load the kuhr files
files_kuhr <- list.files(path=file.path(path_henriks_project, "raw"), pattern="kuhr\\d+", full.names=T)

# Prepare the conversion codes =================================================

# Load the dictionary
conversion_codes <- as.data.table(read_dta("documentation/konverter.dta"))
conversion_codes <- conversion_codes[, .(ICPC2kode, ICPC2kodetekst)]

# Load the english disease names
english_codes <- data.table(read.fwf("documentation/diags.txt", widths=c(3, -1, 200), col.names=c("icpc2", "disease")))

# Rename the codes
setnames(conversion_codes, old=names(conversion_codes), new=c("icpc2", "condition_nor"))

# Keep the unique values
conversion_codes <- unique(conversion_codes, by=c("icpc2", "condition_nor"))

# Merge the english conversion codes
conversion_codes <- merge(conversion_codes, english_codes, all.x=T)

# Save the file
save(conversion_codes, file="data/conversion_codes.Rda")

# Prepare the kuhr data ========================================================

# Load the kuhr files
kuhr <- lapply(files_kuhr, function(kuhr_file) {
  
  cat(kuhr_file, '\n')
  
  # Load the kuhr data
  kuhr <- readRDS(kuhr_file)
  
  # Unlist the disease column in the kuhr data
  kuhr <- kuhr[, .(diagnose=unlist(diagnose)), by=.(lopenr_k2_, year, month)]
  
  # Merge the conversion codes to the kuhr data
  kuhr <- merge(x=kuhr, y=conversion_codes, by.x="diagnose", by.y="icpc2")
  
  return(kuhr)
})

# Bind the kuhr data together
kuhr <- rbindlist(kuhr)

# Filter the relevant mothers
kuhr <- kuhr[lopenr_k2_%in%sample_ids, ]

# Create uniue data
kuhr <- unique(kuhr, by = c("diagnose", "lopenr_k2_", "year", "month", "disease", "condition_nor"))

# Save the kuhr data 
save(kuhr, file="data/kuhr_mothers_2006+.Rda")
rm(list = ls()); gc(TRUE)

### END ########################################################################