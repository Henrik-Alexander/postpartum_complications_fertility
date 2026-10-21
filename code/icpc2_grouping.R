###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Group the 
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 11.12.2025
###

library(stringr)
library(ggplot2)

# Load the parameters
source("parameters.R")

# Load the ICPC2-converion codes
load("data/conversion_codes.Rda")

# Load the functions
source("functions/helper.R")

# Run the categories
source("disease_codes.R")

# Group the codes ==============================================================

# Infections:
#   1,"A78","Infeksjonssykdom IKA","Infectious disease other/NOS"
# 28,"R73","Nesebyll","Boil/abscess nose" 
# 41,"W70","Infeksjon/sepsis i barseltid","Puerperal infection/sepsis"
# "S04" infected finger
infections <- c("A78", "R73", "W70", "S09")

# General: 
#   2,"A87","Komplikasjon etter medisinsk behandling","Complication of medical treatment"
# 3,"A94","Perinatal sykelighet IKA","Perinatal morbidity other"
# 45,"W96","Komplikasjoner i barseltid IKA","Complications of puerperium other"
general <- c("A87", "A94", "W96")

# Blood/bleeding related:
#   4,"B82","Anemi IKA","Anaemia other/unspecified"
blood <- c("B82", "W17")

# Digestion:
#   7,"D24","Abdominal oppfylling/kul IKA","Abdominal mass NOS"
# 16,"K96","Hemoroider","Haemorrhoids"
#   6,"D17","Inkontinens avf?ring","Incontinence of bowel"
# "D91", "Abdominal hernia other"
#   9,"D95","Analfissur/perianal abscess","Anal fissure/perianal abscess"
# 10,"D98","Galleveisbetennelse/gallesten","Cholecystitis/cholelithiasis"
digestion <- c("D17", "D24", "D91",  "K96", "D95", "D98")

# Other:
# 15,"K94","?rebetennelse","Phlebitis/thrombophlebitis"
# 17,"L20","Ledd symptomer/plager IKA","Joint symptom/complaint NOS"
# 29,"S81","Hemangiom/lymfangiom","Haemangioma/lymphangioma"	 
# 30,"S82","Pigmentnevus","Naevus/mole"	 
# 31,"S85","Pilonidalcyste/-fistel","Pilonidal cyst/fistula"	 
# 32,"T07","Vekt?kning","Weight gain"	 
# 36,"U70","Nyrebekkenbetennelse","Pyelonephritis/pyelitis"
# "S94" Ingrowing nail
other <- c("K94", "L20", "S81", "S82", "S85", "T07", "U70", "S94")

# Nerve/sensitivity issues:	
#   19,"N06","Forstyrrelser i hudens f?lsomhet IKA","Sensation disturbance other"
# 20,"N93","Carpal tunnel syndrom","Carpal tunnel syndrome"
# 21,"N94","Perifer nevropati","Peripheral neuritis/neuropathy"
# 22,"N99","Sykdom i nervesystemet IKA","Neurological disease, other"
nerve <- c("N06", "N93", "N94", "N99")

# Mental health
#   23,"P03","Depresjonsf?lelse","Feeling depressed"	
# 25,"P74","Angstlidelse","Anxiety disorder/anxiety state"
# 26,"P76","Depressiv lidelse","Depressive disorder"
# P98 Psycosis
mental <- c("P03", "P74", "P76", "P98")

# Sexuality:	
#   24,"P08","Seksuell tilfredsstillelse hemmet","Sexual fulfilment reduced"	
# 46,"X04","Smerte ved samleie kvinne","Painful intercourse female"
sexuality <- c("P08", "X04")

# Breast: 
#   48,"X18","Smerte i bryst kvinne","Breast pain female"		
# 49,"X20","Brystvorte symptomer/plager kvinne","Nipple symptom/complaint female"
# 50,"X21","Bryst symptomer/plager IKA kvinne","Breast symptom/complt. female other"
# 40,"W19","Amming symptomer/plager","Breast/lactation symptom/complaint"
# 43,"W94","Mastitt i barseltid","Puerperal mastitis"	
# 44,"W95","Brystsykdom i svangerskap/barseltid IKA","Breast disorder in pregnancy other"
# 53,"X88","Godartet mammadysplasi","Fibrocystic disease breast"
breast <- c("X18", "X20", "X21", "W19", "W94", "W95", "X88")

# Female genital organs:
#   47,"X15","Skjede symptomer/plager IKA","Vaginal symptom/complaint other"
# 51,"X82","Skade i kvinnelige kj?nnsorgan","Injury genital female"
# 52,"X87","Fremfall av kvinnelige kj?nnsorgan","Uterovaginal prolapse"
# 54,"X99","Sykdom i kvinnelige kj?nnsorgan IKA","Genital disease female, other"
# 38,"W15","Infertilitet kvinne","Infertility/subfertility"
# 39,"W17","Bl?dning etter f?dsel","Post-partum bleeding"
# 35,"U04","Urininkontinens","Incontinence urine"
genital <- c("X15", "X82", "X87", "X99", "W15", "W17", "U04")


# Cardiovasculary
# 11,"K77","Hjertesvikt","Heart failure"		
# 12,"K85","Forh?yet blodtrykk IKA","Elevated blood pressure"
# 13,"K86","Hypertensjon ukomplisert","Hypertension uncomplicated"
# 14,"K93","Lungeemboli","Pulmonary embolism"
# 42,"W81","Svangerskapsforgiftning","Toxaemia of pregnancy"	 
cardiovasculary <- c("K77", "K85", "K86", "W81", "K93")

# Endocrine:
#   33,"T85","Hypertyreose/tyreotoksikose","Hyperthyroidism/thyrotoxicosis"	
# 34,"T86","Hypotyreose/myks?dem","Hypothyroidism/myxoedema"
# 18,"L95","Osteoporose","Osteoporosis"

endocrine <- c("T85", "T86", "L95")


## Attach the codes ============================================================

# Create a datat set with icpc2 categories
disease_groups <- conversion_codes[icpc2 %in% icpc2_pattern]

# Create the chapter
disease_groups[, chapter := translate_icpc2_chapter(icpc2)]

# Create the groups
disease_groups$group <- ""
disease_groups$group[disease_groups$icpc2 %in% infections] <- "Infections"
disease_groups$group[disease_groups$icpc2 %in% blood] <- "Hematological"
disease_groups$group[disease_groups$icpc2 %in% nerve] <- "Neurological"
disease_groups$group[disease_groups$icpc2 %in% digestion] <- "Digestive"
disease_groups$group[disease_groups$icpc2 %in% general] <- "General"
disease_groups$group[disease_groups$icpc2 %in% other] <- "Other"
disease_groups$group[disease_groups$icpc2 %in% genital] <- "Urogenital"
disease_groups$group[disease_groups$icpc2 %in% breast] <- "Breast"
disease_groups$group[disease_groups$icpc2 %in% mental] <- "Mental"
disease_groups$group[disease_groups$icpc2 %in% sexuality] <- "Sexual_health"
disease_groups$group[disease_groups$icpc2 %in% endocrine] <- "Endocrine"
disease_groups$group[disease_groups$icpc2 %in% cardiovasculary] <- "Cardiovascular"

# Save the data
save(disease_groups, file="documentation/disease_groups.Rda")

### END ########################################################################