###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Load the raw-data
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 06.12.2025
###

library(data.table)
library(tidyverse)

source("paths.R")

# Load the regrestation infomration ============================================

## Note: The data is split into two files in different formats
# 1. Old: 1986-2018 -> wide format
# 2. new: 2019-2023 -> long format

## Old registration data -------------------------------------------------------


# Load the other registration data
dt_regstat_old <- fread(file.path(path_rawdata, "original_data/k2_-w19_1011_4b_regstatus_ut.TXT"), header=T, sep=";")

# Delete the variables that end with _dat
dt_regstat_old[, names(dt_regstat_old)[str_detect(names(dt_regstat_old), "_dat$")]:=NULL]

# Attach the first two digits to the names
years_regstat <- as.numeric(str_extract(names(dt_regstat_old), "\\d{2}$"))
years_regstat <- ifelse(years_regstat<25, years_regstat+2000, years_regstat+1900)
names(dt_regstat_old) <- ifelse(str_detect(names(dt_regstat_old), "_lnr_k2_"), "lopenr_k2_", paste("regstat", years_regstat, sep="_"))

# Create the long data
dt_regstat_old <- melt(dt_regstat_old,
                        id.vars=c("lopenr_k2_"), 
                        measure.vars = patterns("regstat_"),
                        variable.name="year", 
                        value.name="regstat",
                        value.factor=FALSE)

# Clean the year variable
dt_regstat_old[, year:= as.numeric(str_extract(year, "\\d+"))]

# Filter the years after 2004
dt_regstat_old <- dt_regstat_old[year>=2004, ]

## Load the new registration data ----------------------------------------------

# List the files
file_regstat <- list.files(file.path(path_rawdata, "SSB2024"), pattern="REGSTAT", full.names=T)

# Load the new registration datat
dt_regstat_new <- fread(file_regstat, sep=";", header=TRUE)

# Clean the data in the 
dt_regstat_new <- dt_regstat_new[, .(year=aar, regstat=regstatus, lopenr_k2_=w19_1011_lnr_k2_)]


## Bind the two registration files together
dt_regstat <- rbindlist(list(dt_regstat_new, dt_regstat_old), use.names=T)

# Create unique data entries
dt_regstat <- unique(dt_regstat)

# Save the data
save(dt_regstat, file="data/ssb_regstat.Rda")
rm(file_regstat, dt_regstat, dt_regstat_old, dt_regstat_new); gc(TRUE)


## Load the cohabitation data =================================================

# Load the familie and household files
files_fam <- list.files(path=path_data_orig, pattern="fam_hus", full.names=T)
fam_hus <- lapply(files_fam, fread)

# Clean the names
names(fam_hus) <- str_sub(files_fam, start=58, end=61)

# Bind the files together and create a numeric year variable
fam_hus <- rbindlist(fam_hus, idcol = "year", fill=T)
fam_hus$year <- as.numeric(fam_hus$year)

# Recode the crude family-type data: famtype
fam_hus[, relationship := fcase(famtype == 1, "Single",
                                famtype == 2, "Married without children",
                                famtype == 3, "Married with children",
                                famtype == 4, "Mother with children",
                                famtype == 5, "Father with children",
                                famtype == 6, "Cohabitation with children",
                                famtype == 7, "Partnership without/with children")]

# Make the more fine-grained family data: regstat_famtype
fam_hus[, family_form := fcase(regstat_famtyp < 200, "Single",
                               regstat_famtyp %in% c(211, 221, 321), "Married with children",
                               regstat_famtyp %in% c(212, 222, 322), "Cohabiting with children",
                               regstat_famtyp %in% 315:318, "Cohabitation without children",
                               regstat_famtyp %in% c(230:232, 331:332, 241:242), "Single with children",
                               regstat_famtyp %in% c(311:314), "Married without children",
                               regstat_famtyp == 341, "Other family types")]

# Recode the building type: bygntype
fam_hus[, housing_type := fcase(bygntype == 1, "Single_family house",
                                bygntype == 2, "Townhouse",
                                bygntype == 3, "Small apartment building",
                                bygntype == 4, "Big apartment building",
                                bygntype == 5, "Commercial building")]


# Select the important variables
fam_hus <- fam_hus[, .(w19_1011_lnr_k2_, year, family_form, housing_type)]

# Save the data
save(fam_hus, file="data/relationship_housing.Rda")

# Clean the MFR =================================================================

# Load the new MFR data
path_mfr <- list.files(file.path(path_rawdata, "Sept2025_New_NPR_MFR"), pattern="MFR", full.names=T)
dt_mfr <- fread(path_mfr)
names(dt_mfr) <- tolower(names(dt_mfr))

# Create the birth date through gestation and ultrasound appointment and last menstruation
# Note: The date of the ultrasound is sometimes after the birthmonth and it seems to account for gestation already
dt_mfr$birthdate_ultrasound <- as.Date(dt_mfr$ul_termin, format = "%d.%m.%Y")
dt_mfr$birthdate_menstruation <- as.Date(dt_mfr$smensd, format = "%d.%m.%Y") + dt_mfr$svlen_sm_dg

# Correct the month for the birth date
dt_mfr[month(birthdate_ultrasound) != fmnd, .(faar, fmnd, birthdate_ultrasound)]

# Create the birthdate
dt_mfr[ , birthdate := ifelse(smensd_kode==1, birthdate_menstruation,
                              ifelse(month(birthdate_ultrasound)==fmnd, birthdate_ultrasound, NA))]

# Select the important variables
dt_mfr <- dt_mfr[, .(
  # General information
  id_mor_k2_, id_far_k2_, bofylke_dagens, mor_faar, mfmnd, faar, fmnd, mors_alder, sivst, art,
  # Gestation
  svlen_dg, birthdate,
  # Birth history
  paritet_5, dodfodte_5,levendefodte, spabort_12_5, spabort_23_5,
  # Delivery mode
  ksnitt,
  # Maternal health and behaviour
  mor_roykte_oppl,  mor_roykte_foer_svsk, mor_roykte_1_trimester, mor_roykte_siste_trimester, kmi_foer, 
  # Birth and child outcomes
  plural, robson_10, daar, dmnd, dodkat, vekt, apgar1, apgar5, apgar10)]

# Rename the maternal variables
setnames(dt_mfr,
         old = c("bofylke_dagens", "mor_faar", "mfmnd", "mors_alder", "sivst"),
         new = c("county_today", "yob_m", "mob_m", "age_m", "civic"))

# Rename the gestation variables
setnames(dt_mfr,
         old=c("svlen_dg"),
         new=c("gestation"))

# Rename the medical birth history
setnames(dt_mfr, 
         old=c("paritet_5", "dodfodte_5", "levendefodte", "spabort_12_5", "spabort_23_5"),
         new=c("parity5_m", "stillbirths5_m", "livebirths", "miscarriage5_15", "miscarriage5_23"))

# Maternal health and behaviour
setnames(dt_mfr, 
         old=c("mor_roykte_oppl", "mor_roykte_foer_svsk", "mor_roykte_1_trimester", "mor_roykte_siste_trimester", "kmi_foer"),
         new=c("smoking_info", "smoking_before_m", "smoking_1trimester_m", "smoking_3trimester_m", "bmi_before_m"))

# Rename the child outcomes
setnames(dt_mfr,
         old=c("robson_10", "daar", "dmnd", "dodkat", "vekt"),
         new=c("robson", "yod_c", "mod_c", "death_c", "birthweight"))

# Rename the delivery mode variables
setnames(dt_mfr,
         old=c("ksnitt"),
         new=c("cesarean"))

# Recode the lifebirth outcomes
dt_mfr[, livebirth:=ifelse(death_c%in%c(0:6), 1, 0)]

# Create the still birth variable
dt_mfr[, stillbirth:=ifelse(death_c%in%c(7:9), 1, 0)]

# Create the abortion variable
dt_mfr[, abortion:=ifelse(death_c==10, 1, 0)]

# Create a variable for delivery-mode
dt_mfr[, delivery_mode := fcase(is.na(cesarean), "Natural delivery",
                                cesarean==1, "Elective cesarean section",
                                cesarean==2, "Emergency cesarean section",
                                cesarean==9, "Unspecified cesarean section")]

# Clean the information on cesarean secton
table(dt_mfr$cesarean, dt_mfr$cesarean_planned, useNA="always")

# Death category
dt_mfr[, livebirth_cat:=fcase(death_c==0, "still alive",
                              death_c==1, "died 0-24 hours",
                              death_c==2, "died 2-7 days",
                              death_c==3, "died 8-28 days",
                              death_c==4, "died 29 days-12 months",
                              death_c==5, "died 1-2 years",
                              death_c==6, "died 2+ years",
                              death_c==7, "stillbirth before birth",
                              death_c==8, "stillbirth during birth",
                              death_c==9, "stillbirth unknown time",
                              death_c==10, "abortion",
                              death_c%in%11:13, "livebirth, unknown circumstances")]


# Estimate the concpetion date
dt_mfr[, conception_date := birthdate-gestation]

## 2.1 Clean the variable -------------------------

# Recode the congenital anomalies according to the Eurocat categories

# Recode the smoking variables
dt_mfr[, smoking_before_m:=fcase(smoking_before_m==1, "No",
                                 smoking_before_m==2, "Sometimes",
                                 smoking_before_m==3, "Daily")]

# Recode the smoking first trimester variable
dt_mfr[, smoking_1trimester_m:=fcase(smoking_1trimester_m==1, "No",
                                     smoking_1trimester_m==2, "Sometimes",
                                     smoking_1trimester_m==3, "Daily")]

# Recode the smoking in the third trimester variable
dt_mfr[, smoking_3trimester_m:=fcase(smoking_3trimester_m==1, "No",
                                     smoking_3trimester_m==2, "Sometimes",
                                     smoking_3trimester_m==3, "Daily")]

# Save the data
save(dt_mfr, file="data/mfr_cleaned.Rda")

### END ########################################################################