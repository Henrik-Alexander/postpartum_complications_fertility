###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Find the siblings in the data
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 02.12.2025
###


# Load the ids
load("data/sample_ids.Rda")

# Load the regstat data
load("data/ssb_regstat.Rda")

source("paths.R")

# Load the faste data
file_faste <- list.files(file.path(path_rawdata, "Sept_SSB2024"), pattern="FASTE", full.names=T)
dt_faste <- fread(file_faste, sep=";", header=TRUE)

# Store the parental triads -> id for ego, mother and father
dt_faste <- dt_faste[w19_1011_lnr_k2_ %in% sample_ids & lopenr_far_k2_ != "" & lopenr_mor_k2_ != "", .(w19_1011_lnr_k2_, lopenr_mor_k2_, lopenr_far_k2_, foedsels_aar_mnd)]

# Create row ids
dt_faste[, nr_siblings := .N, by=.(lopenr_mor_k2_, lopenr_far_k2_)]

# Filter those with more than 1 row
dt_sibling <- dt_faste[nr_siblings>1, ]

# Create a group id
dt_sibling[, parental_ids :=.GRP, by=.(lopenr_mor_k2_, lopenr_far_k2_)]

# Sort and save
dt_sibling <- dt_sibling[order(parental_ids, foedsels_aar_mnd), ]

# Save the data
save(dt_sibling, file="data/sibling_ids.Rda")

### END ########################################################################