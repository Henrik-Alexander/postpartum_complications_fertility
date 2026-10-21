###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Severe maternal morbidity
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 17.12.2025
###




## Identify the severe maternal morbidities using ICD-10 =======================



# Acute Myocardial infarction
smm_myocar <- c("I21.xx", "I22.x") 


# Aneurysm
smm_aneurysm <- c("I71.xx", "I79.0")


# Acute Renal Failure
smm_renal <- c("N17.x", "O90.4")

# Acute respiratory distress syndrome
smm_respiratory_distress <- c("J80", "J95.1", "J95.2", "J95.3", "J95.82x", "J96.0x", "J96.2x", "J96.9x", "R06.03", "R09.2")

# Amniotic fluid embolism 
smm_embolism <- c("O88.112", "O88.113", "O88.119", "O88.12", "O88.13")

# Cardiac arrest/ventricular fibrillation
smm_cardiac_arrest <- c("I46.x", "I49.0x")

# Conversion of Cardiac Rhythm
smm_cardiac_rhythm <- c("5A12012", "5A2204Z")


# Disseminated intravascular coagulation
smm_intravascular_coagulation <- c('D65', "D68.8", "D68.9", "O45.002", "O45.003", "O45.009", "O45.012",
                                   "O45.013", "O45.019", "O45.022", "O45.023", "O45.029", "O45.092", 
                                   "O45.093", "O45.099", "O46.002", "O46.003", "O46.009", "O46.012", 
                                   "O46.013", "O46.019", "O46.022", "O46.023", "O46.029", "O46.092",
                                   "O46.093", "O46.099", "O67.0", "O72.3")

# Blood transfusion
smm_blood_transfusion <- c("30230H0", "30230K0", "30230L0", "30230M0", "30230N0",
                           "30230P0", "30230R0", "30230T0", "30230H1", "30230K1",
                           "30230L1", "30230M1", "30230N1", "30230P1", "30230R1", 
                           "30230T1", "30233H0", "30233K0", "30233L0", "30233M0",
                           '30233N0', "30233P0", "30233R0", "30233T0", "30233H1",
                           "30233K1", "30233L1", "30233M1", "30233N1", "30233P1", 
                           "30233R1", "30233T1", "30240H0", "30240K0", "30240L0",
                           "30240M0", "30240N0", "30240P0", "30240R0", "30240T0",
                           "30240H1", "30240K1", "30240L1", "30240M1", "30240N1",
                           "30240P1", "30240R1", "30240T1", "30243H0", "30243K0",
                           "30243L0", "30243M0", "30243N0", "30243P0", "30243R0",
                           "30243T0", "30243H1", "30243K1", "30243L1", "30243M1",
                           "30243N1", "30243P1", "30243R1", "30243T1")

# Eclampsia
smm_eclampsia <- c("O15.X")

# Heart failure/arrest during surgery or procedure
smm_heart_failure <- c()

# PUerperal cerebrovascular disorders
smm_cerebrovascular <- c()

# PUlmonary edema / acute heart failure
smm_pulmonary <- c()

# Severe anesthesia complications
smm_anesthasia <- c()

# Sepsis
smm_sepsis <- c()

# Shock
smm_shock <- c()

# Sickle Cell Disease with crisis
smm_sickle_cell <- c()

# Air and thrombotic embolism
smm_air <- c"I26.x", "O88.012–O88.03", "O88.212–O88.23", "O88.312–O88.33", "O88.812–O88.83", "T80.0XXA")

# Hysterectomy
smm_hysterectomy <- c(0B110F4, 0B113F4, 0B114F4)

# Temporary tracheostomy
smm_temporary_tracheostomy <- c("0B110F4", "0B113F4", "0B114F4")

# Ventilation
smm_ventilation <- c("5A1935Z", "5A1945Z", "5A1955Z")

### END ########################################################################