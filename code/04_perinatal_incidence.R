###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Estimate the incidence
# Load the conversion codes
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 28.11.2025
###

rm(list = ls())

library(data.table)
library(tidyverse)
library(zoo)
library(mgcv)

# Load the graphic template
source("functions/graphic_template.R")

# Load the paths
source("paths.R")

# Load the KUHR-data
load("data/kuhr_mothers_2006+.Rda")

# Load the conversion codes
load("data/conversion_codes.Rda")

# Load the birth grid
load("data/mfr_grid.Rda")

# Filter the first births
mfr_grid <- mfr_grid[livebirth==1 & parity5_m == 0, ]

# 1. Estimate incidence of kuhr consulations ===================================

for (i in seq_along(conversion_codes$icpc2)) {
  
  cat("Disease nr: ", i, "\n")
  
  # Select the conversion codes and the disease name
  icpc2 <- conversion_codes$icpc2[i]
  disease <- conversion_codes$disease[i]
  
  # Filter the kuhr data
  kuhr_icpc2 <- kuhr[diagnose==icpc2, ]
  
  # Merge the fertility grid and the kuhr data
  df <- merge(mfr_grid, kuhr_icpc2, by.x=c("id_mor_k2_", "year", "month"), by.y=c("lopenr_k2_", "year", "month"), all.x=T)
  
  # Create a binary indicator
  df[, diagnosis:=ifelse(disease!="", 1, 0)]
  
  # Create the grid
  incidence <- df[, .(exposure=.N, diagnosis=sum(diagnosis, na.rm=T)), by=.(time)]
  
  # Estimate the regression
  mod_result <- gam(diagnosis ~ s(time), data=incidence, offset=log(exposure), family="poisson")
  
  # Model predictions
  mod_prediction <- predict(mod_result, incidence, se.fit=T, type="response")
  incidence$prediction <- mod_prediction$fit
  incidence$se_prediction <- mod_prediction$se.fit
  incidence$disease <- disease
  incidence$icpc2 <- icpc2
  
  # Create the plot
  plot_incidence <- ggplot(data=incidence, aes(x=time)) +
    geom_vline(xintercept = 0, linetype="dashed") +
    # Smoothed data
    geom_ribbon(aes(ymin= 1000 * (prediction - 1.96*se_prediction), ymax=1000 *(prediction + 1.96*se_prediction)), alpha=0.2) +
    geom_line(aes(y=1000 * prediction)) +
    # Observed data
    geom_point(aes(y=1000 * diagnosis/exposure), alpha = 0.5) +
    scale_x_continuous("Time relative to birth (months)", expand=c(0, 0), breaks=seq(-24, 24, by=3)) +
    scale_y_continuous(paste0(disease, " (", icpc2, ") Incidence per 1000 person-months")) 
  ggsave(plot_incidence, filename=paste0("figures/incidence_diseases/incidence_", icpc2, ".svg"), height=12, width=20, unit="cm")
  
  # Save the incidence data
  saveRDS(incidence, file=paste0("results/incidence_diseases/incidence_", icpc2, ".Rds"))
    
}

# 2. Estimate incidence of death ===============================================

# Load the demographic data
load("data/demographics.Rda")

# Subset those with date of death
dt_death <- dt_demo[!is.na(dod)]

# Create month and year of death
dt_death[, yod:=year(dod)]
dt_death[, mod:=month(dod)]

# Merge mortality file
df_death <- merge(mfr_grid, dt_death, by.x=c("id_mor_k2_", "year", "month"), by.y=c("lopenr_k2_", "yod", "mod"), all.x=T)

# Create a binary indicator
df_death[, death:=ifelse(!is.na(dod), 1, 0)]

# Create the grid
maternal_mortality <- df_death[, .(exposure=.N, death=sum(death, na.rm=T)), by=time]

# Estimate the regression
mod_result <- gam(death ~ s(time), data=maternal_mortality, offset=log(exposure), family="poisson")

# Model predictions
mod_prediction <- predict(mod_result, maternal_mortality, se.fit=T, type="response")
maternal_mortality$prediction <- mod_prediction$fit
maternal_mortality$se_prediction <- mod_prediction$se.fit
maternal_mortality$disease <- "death"
maternal_mortality$icpc2 <- "death"

# Create the plot
plot_maternal_mortality <- ggplot(data=maternal_mortality, aes(x=time)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_ribbon(aes(ymin=(prediction - 1.96*se_prediction), ymax=(prediction + 1.96*se_prediction)), alpha=0.2) +
  geom_line(aes(y=prediction)) +
  geom_line(aes(y=death)) +
  geom_point(aes(y=death), size=2) +
  scale_x_continuous("Time relative to birth (months)", expand=c(0, 0), breaks=seq(-24, 24, by=3)) +
  scale_y_continuous(paste0("Total number of deaths (N)"), n.breaks=10)
ggsave(plot_maternal_mortality, filename=paste0("figures/maternal_mortality_rate.svg"), height=12, width=20, unit="cm")

# Save the maternal_mortality data
saveRDS(maternal_mortality, file=paste0("results/maternal_mortality_incidence.Rds"))


# 3. Sepsis data in NPR ========================================================

# Load the NPR sepsis data
npr_sepsis <- read_sas(list.files("./..", pattern=".sas", full.names = T))
npr_sepsis <- as.data.table(npr_sepsis)

# Syk1
vars_syk1 <- c(paste0("syk1_", 1297:1500), "lopenrx")
syk1 <- npr_sepsis[, ..vars_syk1]
syk1 <- melt(data=syk1, variable.name="year", id.vars="lopenrx", value.name="sepsis1")
syk1 <- syk1[sepsis1==1, ]
syk1[, datetime := as.numeric(str_remove(year, "syk\\d_"))]
syk1[, year := trunc(1900 + as.numeric(datetime)/12)]
syk1[, month := round(12 * (1900 + as.numeric(datetime)/12)-year)]
syk1[, datetime:=NULL]

# Syk2
vars_syk2 <- c(paste0("syk2_", 1297:1500), "lopenrx")
syk2 <- npr_sepsis[, ..vars_syk2]
syk2 <- melt(data=syk2, variable.name="year", id.vars="lopenrx", value.name="sepsis2")
syk2 <- syk2[sepsis2==1, ]
syk2[, datetime := as.numeric(str_remove(year, "syk\\d_"))]
syk2[, year := trunc(1900 + as.numeric(datetime)/12)]
syk2[, month := round(12 * (1900 - year + as.numeric(datetime)/12))]
syk2[, datetime:=NULL]

# Bind them together
npr_sepsis <- merge(syk1, syk2, by=c("lopenrx", "year", "month"),all.x=T, all.y=T)
rm(syk1, syk2)
# Save the sepsis data
save(npr_sepsis, file = "data/npr_sepsis.Rda")

# Merge the birth grid and the sepsis data
incidence_sepsis <- merge(mfr_grid, npr_sepsis, by.x=c("id_mor_k2_", "year", "month"), by.y=c("lopenrx", "year", "month"), all.x=T)

# Filter births after 2010
incidence_sepsis <- incidence_sepsis[faar>2010, ]

# Create bivaraite variables
incidence_sepsis[, sepsis1:=ifelse(is.na(sepsis1), 0, 1)]
incidence_sepsis[, sepsis2:=ifelse(is.na(sepsis2), 0, 1)]

# Plot the incidence
incidence_sepsis <- incidence_sepsis[, .(sepsis=sum(sepsis2), exposure=.N), by=.(month)] 

# Estimate the regression
mod_result <- gam(sepsis ~ s(month), data=incidence_sepsis, offset=log(exposure), family="poisson")

# Model predictions
mod_prediction <- predict(mod_result, incidence_sepsis, se.fit=T, type="response")
incidence_sepsis$prediction <- mod_prediction$fit
incidence_sepsis$se_prediction <- mod_prediction$se.fit
incidence_sepsis$disease <- "sepsis"
incidence_sepsis$icpc2 <- "NPR"

# Plot the incidence for the sepsis
ggplot(data=incidence_sepsis, aes(x=month)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  # Smoothed data
  geom_ribbon(aes(ymin= 1000 * (prediction - 1.96*se_prediction), ymax=1000 *(prediction + 1.96*se_prediction)), alpha=0.2) +
  geom_line(aes(y=1000 * prediction)) +
  # Observed data
  geom_point(aes(y=1000 * sepsis/exposure), alpha = 0.5) +
  scale_x_continuous("Time relative to birth (months)", expand=c(0, 0), breaks=seq(-24, 24, by=3)) +
  scale_y_continuous("Incidence sepsis (NPR) per 1000 person-months") 

### END ########################################################################