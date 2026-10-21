###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: compare the results
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 02.12.2025
###

rm(list=ls()); gc(TRUE)

options(scipen=999)

library(data.table)
library(ggplot2)
library(ggrepel)

# Load the graphic schema
source("functions/graphic_template.R")

# Load the parameters
source("parameters.R")

# Load the result data
incidence_files <- list.files(path="results", pattern="incidence_[A-Z]\\d{2}.Rds", full.names=T)
res <- lapply(incidence_files, readRDS)

# Bind the data
res <- rbindlist(res, fill=T)

# Estimate the incidence rate
res[, incidence_rate:=1000*diagnosis/exposure]

# 1. Analyze the distributon ===================================================

# Plot the incidence by time
ggplot(res, aes(x=time, y=incidence_rate, group=time, colour=time)) +
  geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(res$incidence_rate), fill="Pregnancy"), alpha=0.2) +
  geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(res$incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  geom_vline(xintercept=c(-9, 0)) +
  geom_boxplot(colour="black") +
  geom_jitter(size=0.5, alpha=0.5, width=0.5) +
  scale_x_continuous("Time relative to birth (months)", expand=c(0, 0), breaks=seq(-24, 24, by=3)) +
  scale_y_continuous("Incidence rate distribution", trans="log10", n.breaks=10, expand=c(0, 0)) +
  scale_colour_gradient2("Time", low="blue", high="red", midpoint=0, mid="grey") +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  theme(
    legend.key.width=unit(3, "cm")
  )
ggsave(filename="figures/incidence_period_distribution.svg", height=15, width=25, unit="cm")

# 2. Detect pregnancy and birth related problems ===============================

# Create three episodes: pre-pregnancy, pregnancy, postpartum
res[, period:=factor(fcase(time< -9, 1,
                    time>= -9&time<0, 2,
                    time>=0 & time <12, 3,
                    time>=12, 4), labels = c("Pre-pregnancy", "Pregnancy", "Postpartum (short)", "Postpartum (long)"), ordered=T)]

# Aggregate the incidence rate
res_per <- res[, .(incidence= mean(1000 * prediction), count = .N), by=.(period, icpc2, disease)]
res_per[, avg_incidence:=mean(incidence), by=icpc2]
  
# Plot the aggregated results
ggplot(data=subset(res_per, avg_incidence>1), aes(x=fct_reorder(disease, incidence), y=incidence, colour=period, shape=period)) + 
  geom_point(size=2) +
  coord_flip() +
  scale_x_discrete("Disease", expand=c(0, 0.2)) +
  scale_y_continuous("Incidence rate (consultations per 1000 person-months)", trans="log10", n.breaks=10, expand=c(0, 0))
ggsave(filename="figures/incidence_period_point.svg", height=15, width=15, unit="cm")

# Plot the aggregated results
ggplot(data=subset(res_per, avg_incidence>1), aes(x=fct_reorder(disease, incidence), y=incidence, fill=period)) + 
  geom_col(position=position_dodge()) +
  coord_flip() +
  scale_x_discrete("Disease", expand=c(0, 0.2)) +
  scale_y_continuous("Incidence rate (consultations per 1000 person-months)", n.breaks=10, expand=c(0, 0))
ggsave(filename="figures/incidence_period_bar.svg", height=15, width=15, unit="cm")

# 3. Estimate the risk ratios ==================================================

# Create wide data
res_per_wide <- dcast(res_per, formula="disease + icpc2 + avg_incidence ~ period", value.var = "incidence")

# Risk ratio relative to pre-pregnancy
res_per_wide[, rr_pregnancy:=round(Pregnancy/`Pre-pregnancy`, 3)]
res_per_wide[, rr_postpartum:=round(`Postpartum (short)`/`Pre-pregnancy`, 3)]
res_per_wide[, `Postpartum (short)`:=round(`Postpartum (short)`, 3)]
res_per_wide[, `Pre-pregnancy`:=round(`Pre-pregnancy`, 3)]
res_per_wide[, `Postpartum (long)`:=round(`Postpartum (long)`, 3)]
res_per_wide[, `Pregnancy`:=round(`Pregnancy`, 3)]

# Plot the risk ratios in postpartum
ggplot(data=subset(res_per_wide, rr_postpartum>=cutoff_datadriven & `Postpartum (short)` > 1), aes(x=fct_reorder(disease, rr_postpartum))) +
  geom_point(aes(y=rr_postpartum)) +
  coord_flip() +
  scale_x_discrete("Disease") +
  scale_y_continuous("Postpartum risk vs. pre-pregnancy (Relative risk ratio)", trans="log10", n.breaks=10)

ggsave(filename="figures/rr_postpartum.svg", height=15, width=25, unit="cm")

# Save the table
tab_rr_postpartum <- res_per_wide %>% 
  select(disease, icpc2, `Postpartum (short)`, `Pre-pregnancy`, rr_postpartum) %>% 
  filter(rr_postpartum>=1 & !is.na(rr_postpartum) & !is.infinite(rr_postpartum)) %>% 
  arrange(desc(rr_postpartum))
write.csv(tab_rr_postpartum, file="results/tab_rr_postpartum.csv")


## Pregnancy risk --------------------------------------------------------------

# Plot the risk ratios in pregnancy
ggplot(data=subset(res_per_wide, rr_pregnancy>=cutoff_datadriven & Pregnancy > 1), aes(x=fct_reorder(disease, rr_pregnancy))) +
  geom_point(aes(y=rr_pregnancy)) +
  coord_flip() +
  scale_x_discrete("Disease") +
  scale_y_continuous("pregnancy risk vs. pre-pregnancy (Relative risk ratio)", trans="log10", n.breaks=10)
ggsave(filename="figures/rr_pregnancy.svg", height=15, width=15, unit="cm")

# Save the table
tab_rr_pregnancy <- res_per_wide %>% 
  select(disease, icpc2, `Pre-pregnancy`, Pregnancy, rr_pregnancy) %>% 
  filter(rr_pregnancy>=1 & !is.na(rr_pregnancy) & !is.infinite(rr_pregnancy)) %>% 
  arrange(desc(rr_pregnancy))

write.csv(tab_rr_pregnancy, file="results/tab_rr_pregnancy.csv")

## Bind the tables for the data-driven approach together
tab_datadriven <- rbindlist(list(tab_rr_pregnancy, tab_rr_postpartum), fill = T)

# Filter those with an incidence higher than 1 per 1000 births
tab_datadriven[Pregnancy>1 | `Postpartum (short)`>1 ] %>% 
  pull(icpc2) %>% 
  sort()

# 4. Study the list by Vogel ===================================================

# List the ICPC2-codes
codes_postpartum <- c("D95", "X99", "S85", "W84", "X74", "X01",
                      "U70", "X74", "W82", "X74", "W15", "X04",
                      "P08", "X24", "P03", "P76", "P74", "P82", "P98", "W27",
                      "N94", "N99", "T86", "T85", "T70", "W94",
                      "B90", "A78", "U71", "W70", "B82", "W84", "X99",
                      "X99", "X78", "X87", "X77", "A78", "U71", "W70",
                      "X74", "A87", "X01", "X17", "A87", "X77", "X87", "X99",
                      "X78", "X87", "X99", "X78", "X87", "X01",
                      "X01", "D06", "X04", "X01", "D06", "A11",
                      "X04", "P08", "X24", "X04", "P08", "U04", "D17", "P12",
                      "A94", "X82", "X01", "U70", "U04", "D17", "P12", "X82",
                      "X01", "X04", "P08", "X24", "U04", "D17", "P12")

# Make a frequency table
table(codes_postpartum)

# Filter the diseases by those from vogel
res_vog <- res[icpc2%in% codes_postpartum, ]

# Create a clean label
res_vog[, disease:=str_extract(disease, "[A-Za-z ]+")]

# Plot the different diseases
ggplot(res_vog, aes(x=time, y=incidence_rate)) +
  geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(res_vog$incidence_rate), fill="Pregnancy"), alpha=0.2) +
  geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(res_vog$incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  geom_line(aes(), size=1.3) +
  #geom_point(aes(group=disease, colour=disease), size=2) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease incidence (per 1000 person-months)", trans="log10", expand=c(0, 0)) +
  facet_wrap(~disease) +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  guides(colour="none") +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_vogel_panel.svg", height=40, width=50, unit="cm")

# Plot the different codes
ggplot(res_vog, aes(x=time, y=incidence_rate)) +
  geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(res_vog$incidence_rate), fill="Pregnancy"), alpha=0.2) +
  geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(res_vog$incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  geom_line(aes(), size=1.5) +
  #geom_point(aes(group=disease, colour=disease), size=2) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease incidence (per 1000 person-months)", trans="log10", expand=c(0, 0)) +
  facet_wrap(~icpc2) +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  guides(colour="none") +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_vogel_icpc2_panel.svg", height=40, width=50, unit="cm")


## Aggregate the diseases ------------------------------------------------------

# Aggregate the diseases
res_vog_agg <- res_vog[, .(incidence_rate = sum(diagnosis)/unique(exposure), diagnosis=sum(diagnosis), exposure=unique(exposure)), by=.(time, period)]

# Estimate a pre-pregnancy trend model
mod_pre_pregnancy <- glm(diagnosis ~ time, offset=log(exposure), family="poisson", data=subset(res_vog_agg, period=="Pre-pregnancy"))

# Predict the results
mod_prediction <- predict(mod_pre_pregnancy, newdata=res_vog_agg, type="response")
res_vog_agg <- as.data.table(cbind(res_vog_agg, mod_prediction))

# Estimate 
ggplot(res_vog_agg, aes(x=time)) +
  geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(res_vog_agg$incidence_rate), fill="Pregnancy"), alpha=0.2) +
  geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(res_vog_agg$incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_text_repel(data=subset(res_vog_agg, time %in% seq(-24, 24, by=3)), aes(y=incidence_rate, label=round(incidence_rate, 3)), family="serif") +
  geom_vline(xintercept=0) +
  geom_line(aes(x=time, y=mod_prediction/exposure), linetype="dotted", size=1.3) +
 # geom_segment(aes(x=-9, xend=24, y=tmp_prepregnancy$avg_incidence, yend=tmp_prepregnancy$avg_incidence)) +
#  annotate(geom="text", x=18, y=tmp_prepregnancy$avg_incidence, label="Pre-pregnancy mean", vjust=-1, family="serif") +
  geom_line(aes(y=incidence_rate), size=1.3) +
  geom_point(aes(y=incidence_rate), size=2) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease incidence (per 1000 person-months)", expand=c(0, 0), n.breaks=10) +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  guides(colour="none") +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_vogel_combined.svg", height=15, width=25, unit="cm")

## Aggregate a reduced set of diseases -----------------------------------------

# Select diseases codes with expected pattern
source("disease_codes.R")

# Filter the diseases
res <- res[icpc2 %in% icpc2_pattern, ]

# Plot the different diseases
ggplot(subset(res, icpc2%in%icpc2_pattern), aes(x=time, y=incidence_rate)) +
  geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(res_vog$incidence_rate), fill="Pregnancy"), alpha=0.2) +
  geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(res_vog$incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  geom_line(aes(group=disease, colour=disease), size=1.3) +
  geom_point(aes(group=disease, colour=disease), size=2) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease incidence (per 1000 person-months)", trans="log10", expand=c(0, 0)) +
  #facet_wrap(~disease) +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  scale_colour_viridis_d("Diseases: ") +
  guides(fill="none") +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_selected.svg", height=40, width=50, unit="cm")

# Plot the different diseases
res[icpc2%in%icpc2_pattern, .(risk_ratio = 100 * (incidence_rate/first(incidence_rate)), time, disease), by=icpc2] %>% 
  filter(!is.na(risk_ratio) & !is.infinite(risk_ratio) ) %>% 
ggplot(aes(x=time, y=risk_ratio)) +
  geom_hline(yintercept=100) +
  geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(risk_ratio), fill="Pregnancy"), alpha=0.2) +
  geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(risk_ratio), fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  geom_line(aes(group=disease, colour=disease)) +
  geom_point(aes(group=disease, colour=disease)) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease risk relative to t-24 (t-24=100)", trans="log10", expand=c(0, 0), n.breaks=10) +
  #facet_wrap(~disease) +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  scale_colour_viridis_d("Diseases: ") +
  guides(fill="none") +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_relative_selected.svg", height=20, width=30, unit="cm")

# Plot the different diseases
res[icpc2%in%icpc2_pattern, .(risk_ratio = 100 * (incidence_rate/first(incidence_rate)), time, disease), by=icpc2] %>% 
  filter(!is.na(risk_ratio) & !is.infinite(risk_ratio) ) %>% 
  ggplot(aes(x=time, y=risk_ratio)) +
  geom_hline(yintercept=100) +
  geom_rect(xmin=-9, xmax=0, ymin=0, ymax=1000000, aes(fill="Pregnancy"), alpha=0.2) +
  geom_rect(xmin=0, xmax=12, ymin=0, ymax=1000000, aes(fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  geom_line(aes(group=disease, colour=disease)) +
  geom_point(aes(group=disease, colour=disease), size=1.5) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease risk relative to 100", trans="log10", expand=c(0, 0)) +
  facet_wrap(~disease, scales="free_y") +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  scale_colour_viridis_d("Diseases: ") +
  guides(colour="none", fill="none") +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_relative_selected_panel.svg", height=40, width=50, unit="cm")

# Plot the different diseases
ggplot(subset(res, icpc2%in%icpc2_pattern), aes(x=time, y=incidence_rate)) +
  geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(res_vog$incidence_rate), fill="Pregnancy"), alpha=0.2) +
  geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(res_vog$incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  geom_line(aes(group=disease, colour=disease), size=1.3) +
  geom_point(aes(group=disease, colour=disease), size=2) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease incidence (per 1000 person-months)", trans="log10", expand=c(0, 0)) +
  #facet_wrap(~disease) +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  scale_colour_viridis_d() +
  guides(fill="none") +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_selected.svg", height=40, width=50, unit="cm")

# Plot the different diseases
ggplot(subset(res, icpc2%in%icpc2_pattern), aes(x=time, y=incidence_rate)) +
  geom_rect(xmin=-9, xmax=0, ymin=0, ymax=1000, aes(fill="Pregnancy"), alpha=0.2) +
  geom_rect(xmin=0, xmax=12, ymin=0, ymax=1000, aes(fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  geom_line(aes(y=prediction*1000, group=disease, colour=disease), size=1.3) +
  geom_point(aes(y=incidence_rate, group=disease, colour=disease), size=1.5, alpha=0.5) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease incidence (per 1000 person-months)",  expand=c(0, 0)) +
  facet_wrap(~disease, scales="free_y") +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  guides(colour="none") +
  scale_colour_viridis_d() +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_selected_panel.svg", height=40, width=50, unit="cm")

# Aggregate the diseases
res_red_agg <- res_vog[icpc2 %in% icpc2_pattern, .(incidence_rate = sum(diagnosis)/unique(exposure), diagnosis=sum(diagnosis), exposure=unique(exposure)), by=.(time, period)]

# Estimate a pre-pregnancy trend model
mod_pre_pregnancy2 <- glm(diagnosis ~ time, offset=log(exposure), family="poisson", data=subset(res_red_agg, time<=-9))

# Predict the results
mod_prediction2 <- predict(mod_pre_pregnancy2, newdata=res_red_agg, type="response")
res_red_agg <- as.data.table(cbind(res_red_agg, mod_prediction2))
res_red_agg[, trend:=mod_prediction2/exposure]

# Estimate 
ggplot(res_red_agg, aes(x=time)) +
  geom_rect(aes(xmin=-9, xmax=0, ymin=0, ymax=max(res_vog_agg$incidence_rate), fill="Pregnancy"), alpha=0.2) +
  geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=max(res_vog_agg$incidence_rate), fill="Postpartum"), alpha=0.2) +
  geom_text_repel(data=subset(res_red_agg, time %in% seq(-24, 24, by=3)), aes(y=incidence_rate, label=round(incidence_rate, 3)), family="serif") +
  geom_vline(xintercept=0) +
  geom_line(aes(x=time, y=trend), linetype="dotted", size=1.3) +
  # geom_segment(aes(x=-9, xend=24, y=tmp_prepregnancy$avg_incidence, yend=tmp_prepregnancy$avg_incidence)) +
  #  annotate(geom="text", x=18, y=tmp_prepregnancy$avg_incidence, label="Pre-pregnancy mean", vjust=-1, family="serif") +
  geom_line(aes(y=incidence_rate), size=1.3) +
  geom_point(aes(y=incidence_rate), size=2) +
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=3), expand=c(0, 0)) +
  scale_y_continuous("Disease incidence (per 1000 person-months)", expand=c(0, 0), n.breaks=10, limits=c(0, max(res_vog_agg$incidence_rate))) +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  guides(colour="none") +
  theme(strip.text=element_text(size=8))
ggsave(filename="figures/incidence_reduced_combined.svg", height=15, width=25, unit="cm")


# Save the incidence for the Vogel et al. data
write.csv(res_vog, file="results/tab_incidence_vogel.csv")


## Plot the groups =============================================================

# Load the groupings
load("documentation/disease_groups.Rda")

# Merge the groupings
res_groups <- merge(res, disease_groups, by=c("icpc2", "disease"))

# Crate the disesae label 
res_groups[, disease_label:=str_extract(disease, "[A-Za-z ]+")]

# Clean the group name
res_groups[, group := str_replace(group, "_", " ")]

# Extract diseases
res_groups$disease <- str_extract(res_groups$disease, "[A-Z a-z]+")
res_groups$disease <- str_remove(res_groups$disease, " NOS")
res_groups$disease <- str_remove(res_groups$disease, " other")
res_groups$disease <- str_remove(res_groups$disease, " uncomplicated")
res_groups$disease <- str_replace(res_groups$disease, " other")


# Plot the disease groups
ggplot(subset(res_groups, group!=""), aes(x=time, y=incidence_rate)) +
  geom_rect(xmin=-9, xmax=0, ymin=0, ymax=1000, aes(fill="Pregnancy"), alpha=0.2) +
  geom_rect(xmin=0, xmax=12, ymin=0, ymax=1000, aes(fill="Postpartum"), alpha=0.2) +
  geom_vline(xintercept=0) +
  #geom_text(data= subset(res_groups, group!=""&time==2), aes(label = disease, colour = disease)) +
  geom_line(aes(y=prediction*1000, group=group, colour=group), size=1.5) +
  geom_point(aes(y=incidence_rate, group=group, colour=group), size=1, alpha=0.5) +
  #geom_text_repel(data=subset(res_groups, time==24), aes(=disease_label, colour=disease), family="serif", size=3.5, vjust="inward", hjust="left") + 
  scale_x_continuous("Time relative to birth (in months)", breaks=seq(-24, 24, by=6), expand=c(0, 0), limits=c(-24, 24)) +
  scale_y_continuous("Disease incidence (per 1000 person-months)",  expand=c(0, 0)) +
  facet_wrap(~disease, scales="free_y", ncol = 5, nrow =8) +
  scale_fill_manual("Period:", values=c("lightblue", "pink")) +
  scale_colour_viridis_d(" Disease group:") +
  guides(fill = "none") +
  theme(strip.text=element_text(size=8))
ggsave("figures/incidence_groups.svg", height=35, width=30, unit="cm")

### END #######################################################################