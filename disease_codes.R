###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Select the disease codes
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 06.12.2025
###

library(stringr)
library(ggplot2)
library(data.table)

# Load the parameters
source("parameters.R")

# Load the ICPC2-converion codes
load("data/conversion_codes.Rda")

# Prepare the data analysis ----------------------------------------------------

# Load the incidence data
incidence_files <- list.files("results", pattern="incidence_[A-Z]\\d{2}.Rds")
incidence_list <- lapply(file.path("results", incidence_files), readRDS)
names(incidence_list) <- str_sub(incidence_files, start=11, end=13)
dt_incidence <- rbindlist(incidence_list)
save(dt_incidence, file="data/incidence_combined.Rda")

# Load the files
files_incidence <- list.files("results", pattern="csv")
incidence_list <- lapply(file.path("results", files_incidence), fread)
names(incidence_list) <- str_remove(str_remove(files_incidence, "\\.csv"), "tab_")

# List the codes from the literature-based, the data-driven approaches
icpc2_postpartum_literature <- unique(incidence_list$incidence_vogel$icpc2)
icpc2_postpartum_data <- incidence_list$rr_postpartum$icpc2[incidence_list$rr_postpartum$rr_postpartum>cutoff_datadriven]
icpc2_pregnancy_data <- incidence_list$rr_pregnancy$icpc2[incidence_list$rr_pregnancy$rr_pregnancy>cutoff_datadriven]

# Create indicators for the inclusion
dt_incidence[, literature_based_postpartum:= ifelse(icpc2 %in% icpc2_postpartum_literature, 1, 0)]
dt_incidence[, data_driven_postpartum:= ifelse(icpc2 %in% icpc2_postpartum_data, 1, 0)]
dt_incidence[, data_driven_pregnancy:= ifelse(icpc2 %in% icpc2_pregnancy_data, 1, 0)]

# Estimate the incidence rate
dt_incidence[, incidence_rate:=diagnosis/exposure]

# Clean the label for the plots
dt_incidence[, disease_label := paste(str_extract(disease, "[A-Za-z ]+"), icpc2)]

# Select the cases
tmp <- dt_incidence[data_driven_postpartum==1, ]

# Plot the incidence
ggplot(data=tmp, aes(x=time)) +
  geom_rect(xmin=0, xmax=12, ymin=0, ymax=1000, fill="red", alpha=0.2) +
  #geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_ribbon(aes(ymin=1000*(prediction - 1.96*se_prediction), ymax=1000*(prediction + 1.96*se_prediction)), alpha=0.5) +
  geom_line(aes(y= 1000 * prediction), size=1.2) +
  geom_point(aes(y=1000*incidence_rate), alpha=0.4) +
  scale_x_continuous("Time relative to birth (months)", expand=c(0, 0), breaks=seq(-24, 24, by=3)) +
  scale_y_continuous(paste0("Incidence per 1000 person-months"), expand=c(0.1, 0), n.breaks=6) +
  facet_wrap(~disease_label, scales="free_y")


# Select the postpartum conditions
postpartum_conditions <- c("A87", "A94", "B50", # Check B50
                           "B52", "D17", "D24", "D95",# Check D81, D93
                           "D91", "D95", "D98", "K77", "K85", "K86",
                           "K92", "K93", "K96", "L20", "L95", "N06", "N93",
                           "P98", "R73", "S81", "S82",
                           "T07", "U04", "S09", "S94",
                           "W17", "W19", "W70", "W81",
                           "W94", "W95", "W96", "X04", "X15", "X18", "X20",
                           "X21", "X82", "X87", "X88", "X99", "Z14", "Z30") 

# Correct the data driven incidence
dt_incidence[, data_driven_postpartum:=ifelse(icpc2%in%postpartum_conditions, 1, 0)]

# Investigate the selection
tmp <- dt_incidence[data_driven_postpartum==1 | literature_based_postpartum==1, ]

# Estimate the incidence relative
ggplot(data=tmp,  aes(x=time)) +
  geom_rect(xmin=0, xmax=12, ymin=0, ymax=1000, fill="red", alpha=0.2) +
  #geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_ribbon(aes(ymin=1000*(prediction - 1.96*se_prediction), ymax=1000*(prediction + 1.96*se_prediction)), alpha=0.5) +
  geom_line(aes(y= 1000 * prediction), size=1.2) +
  geom_point(aes(y=1000*incidence_rate), alpha=0.4) +
  scale_x_continuous("Time relative to birth (months)", expand=c(0, 0), breaks=seq(-24, 24, by=3)) +
  scale_y_continuous(paste0("Incidence per 1000 person-months"), expand=c(0, 0), n.breaks=6) +
  facet_wrap(~disease_label, scales="free_y") 
ggsave(filename="figures/incidence_postpartum_diagnosis.svg", height=50, width=70, unit="cm")


# Correct the literature-based approach
correct_literature_based <- c("A11",  "D06",  "X77", "X74", "P82",
                              "W82", "W84", "P12", "U71", "T70", "X78", "U71", "B90"
                              )

# Correct the literature based approach
dt_incidence[, literature_based_postpartum:=ifelse(icpc2%in%correct_literature_based, 0, literature_based_postpartum)]

# Select the pregnancy codes ---------------------------------------------------

# Investigate the selection
tmp <- dt_incidence[data_driven_pregnancy==1, ]

# Estimate the incidence rate
tmp[, incidence_rate:=diagnosis/exposure]

# Collect the ICPC-2 codes for pregnancy conditions
icpc2_pregnancy <- c("A04", "A06", "A23", "A27", "A29", "A74", # A76 removed posteriory
                     "A91", "A99", "B78", "B79", "B80",
                     "B81", "B82", "B83", "B84", "D01", "D03",
                     "D04", "D06", "D09", "D10", "D12", "D14", "D72",
                     "D89", "D96", "K04", "K07", "K29", "K73", "K78",
                     "K79", "K83", "K88", "K89", "L02", "L03", 
                     "L13", "L84", "N01", "N17", "N88", "N93", "P85",
                     "R06", "R24", "S02", "S19", "T01", "T10", "T11",
                     "T29", "T86", "T87", "T89", "U98", "U99",
                     "W03", "W05", "W71", "W76", "W84", "W85",
                     "X70", "X72", "X78" # "Z08", "Z10 Remove Health care system and social welfare problem
                     )

# Create the label
tmp[, disease_label := str_extract(disease, "[A-Za-z ]+")]

# Plot the diseases
plot_pregnancy <- ggplot(data=subset(tmp, icpc2%in%icpc2_pregnancy), aes(x=time)) +
  geom_rect(xmin=-9, xmax=0, ymin=0, ymax=1000, fill="red", alpha=0.2) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_ribbon(aes(ymin=1000*(prediction - 1.96*se_prediction), ymax=1000*(prediction + 1.96*se_prediction)), alpha=0.5) +
  geom_line(aes(y= 1000 * prediction), size=1.2) +
  geom_point(aes(y=1000*incidence_rate), alpha=0.4) +
  scale_x_continuous("Time relative to birth (months)", expand=c(0, 0), breaks=seq(-24, 24, by=3)) +
  scale_y_continuous(paste0("Incidence per 1000 person-months"), expand=c(0, 0), n.breaks=6) +
  facet_wrap(~disease_label, scales="free_y")
ggsave(plot_pregnancy, filename="figures/incidence_pregnancy_diagnosis.svg", height=50, width=70, unit="cm")

# Change the parameters for data driven pregnancy
dt_incidence[, data_driven_pregnancy := ifelse(icpc2%in% icpc2_pregnancy, 1, 0)]

# Select the postpartum codes --------------------------------------------------

# Note: we do not model all possible combinations
# The postpartum period is always included and the literature review is also always included


if (all(parameters)) {

        # Select diseases codes with expected pattern
        icpc2_pattern <- unique(dt_incidence$icpc2[dt_incidence$literature_based_postpartum==1|dt_incidence$data_driven_postpartum==1|dt_incidence$data_driven_pregnancy==1])
        
      } else if (pregnancy_period==TRUE & data_driven==FALSE) {
        
        # Select diseases codes with expected pattern
        icpc2_pattern <- unique(dt_incidence[dt_incidence$literature_based_postpartum==1|dt_incidence$data_driven_pregnancy==1]$icpc2)
        
      } else if (pregnancy_period==FALSE & data_driven==TRUE) {
    
        # Select diseases codes with expected pattern
        icpc2_pattern <- unique(dt_incidence$icpc2[(dt_incidence$literature_based_postpartum==1|dt_incidence$data_driven_postpartum==1)])
        
      } else if (pregnancy_period==FALSE & data_driven==FALSE) {
        
        # Select diseases codes with expected pattern
        icpc2_pattern <- unique(dt_incidence$icpc2[dt_incidence$literature_based_postpartum==1])
  
      }

# Select the expert-based codes
if (expert_selection) {
  expert_selection <- c("B82", "D17", "D91", "D95", "D98", "K77", "K85",
                        "K86", "K93", "K94", "K96", "L20", "L95", "N06",
                        "N93", "N94", "P03", "P08", "P74", "P76", "P98",
                        "T07", "T85", "T86", "U04", "U70", "W17", "W19",
                        "W70", "W81", "W94", "W95", "W96", "X04", "X15",
                        "X18", "X20", "X21", "X82", "X87", "X99")
  
  # Filter those that are in Emilie's selectio
  icpc2_pattern <- icpc2_pattern[icpc2_pattern %in% expert_selection]
}

# Create a new object with the postpartum codes
postpartum_codes <- conversion_codes[conversion_codes$icpc2 %in% icpc2_pattern]


## Save for Oystein a file with the respective disease codes -------------------

# Store the codes
write.csv(postpartum_codes, file="results/postpartum_disease_codes.csv")
pregnancy_codes <- conversion_codes[conversion_codes$icpc2 %in% icpc2_pregnancy]
write.csv(pregnancy_codes, file="results/pregnancy_disease_codes.csv")

### END ########################################################################