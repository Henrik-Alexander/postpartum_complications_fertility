###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Present the results
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 04.12.2025
###

library(data.table)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(broom)
library(ggrepel)
library(scales)
library(openxlsx)

# Load the funciton to create tables
source("functions/create_table.R")

# Load the conversion codes
load("data/conversion_codes.Rda")

# Load the incidence data
load("data/incidence_main_analysis.Rda")

# Load the functions
source("functions/helper.R")
 
# Load the graphic template
source("functions/graphic_template.R")

# Load the groups
source("code/icpc2_grouping.R")

# Load the data
load("documentation/disease_groups.Rda")

# Load the analysis data
load("data/df_analysis.Rda")

# 1. Short-term fertility ===================================================

# Load the binary model for any postpartum complication

# List regression files
reg_files <- list.files("results", full.names=T, pattern="lpm_res_.+short.Rds")
regression_models_short <- lapply(reg_files, function(models) {
  tmp <- readRDS(models)
  names(tmp) <- paste0("model", 1:4)
  tmp <- lapply(tmp, tidy, conf.int=T)
  tmp <- rbindlist(tmp, idcol="model")
  return(tmp)
  })

# Clean the names and assign to the regrssion models in the list
names(regression_models_short) <- str_remove(str_remove(reg_files, "results/lpm_res_"), "_?short.Rds")

# Bind the results together
regression_models_short <- rbindlist(regression_models_short, idcol="icpc2")

# Round the p-value
regression_models_short[, p.value:=round(p.value, 4)]

# Filter the disease term
regression_models_short <- regression_models_short[(str_detect(term, "single_condition|any_postnatal|any_disease") | term==icpc2), ]

# Select unique observations
regression_models_short <- unique(regression_models_short, by = c("icpc2", "model"))

# Merge the conversion codes to the model results for the accurate labelling
regression_models_short <- merge(regression_models_short, conversion_codes, all.x=T, by.x="icpc2", by.y="icpc2")

# Create a disaese label <- here could the prevalence be added to the data
regression_models_short[, disease_label:=ifelse(icpc2=="any", "Any complication", paste0(disease, " (", icpc2, ")"))]

# Classify into positive and negative associations
regression_models_short[, mean_association:=mean(estimate), by=disease_label]
regression_models_short[, max_p := max(p.value), by =disease_label]
regression_models_short[, direction:=ifelse(max_p>0.05, "Not significant", ifelse(mean_association>0, "Positive association", "Negative association"))]
regression_models_short[, significance:=factor(ifelse(p.value<0.05, 1, 0.8), labels=c("Not Significant", "Statistically significant"))]

# Merge the grouping to the models
regression_models_short <- merge(regression_models_short, disease_groups, by=c("icpc2", "disease"))

# Plot the regression coefficients
ggplot(regression_models_short, aes(x=fct_reorder(disease_label, mean_association), y=estimate, colour=model, shape=model)) +
  geom_hline(yintercept=0, colour = "grey") + 
  geom_point(aes(alpha=significance, group=model), position=position_dodge(width=1)) +
  geom_text(aes(label=round(estimate, 2), y=conf.low, group=model), position=position_dodge(width=1), hjust=1, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high, group=model), position=position_dodge(width=1), width=0.5) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_shape_discrete("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association", n.breaks=6, expand = c(0.5, 0)) +
  scale_alpha_discrete("P-value", range=c(0.4, 1)) +
  facet_wrap(~ direction, scales="free_y") +
  coord_flip() +
  theme(axis.title.y=element_blank(),
        panel.grid.minor.y = element_line(size=2, linetype="solid"))

ggsave(filename="figures/lpm_res_short.svg", height=30, width=30, unit="cm")

# Plot by condition group
ggplot(regression_models_short, aes(x=fct_reorder(disease_label, mean_association), y=estimate, colour=model)) +
  geom_hline(yintercept=0, colour = "grey") + 
  geom_point(aes(alpha=significance), position=position_dodge(width=1)) +
  geom_text(aes(label=round(estimate, 2), y=conf.low), position=position_dodge(width=1), hjust=1, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high), position=position_dodge(width=1), width=0.5) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association", expand = c(1, 0)) +
  scale_alpha_discrete("P-value", range=c(0.4, 1)) +
  facet_wrap( ~ group, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())
ggsave(filename="figures/lpm_res_groups_short.svg", height=30, width=40, unit="cm")

# Plot only the full model
regression_models_short[, direction:=ifelse(p.value>0.05, "Not significant", ifelse(estimate>0, "Positive association", "Negative association"))]
ggplot(subset(regression_models_short, model=="model4"), aes(x=fct_reorder(disease_label, estimate), y=estimate, colour=group)) +
  geom_hline(yintercept=0, colour = "grey") + 
  geom_point(aes(alpha=significance), position=position_dodge(width=1)) +
  geom_text(aes(label=round(estimate, 2), y=conf.high), hjust=-0.3, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high), position=position_dodge(width=1), width=0.2) +
  scale_colour_viridis_d("Disease group:") +
  scale_y_continuous("Association", expand = c(0.4, 0)) +
  scale_alpha_discrete("P-value", range=c(0.4, 1)) +
  facet_wrap( ~ direction, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())
ggsave(filename="figures/lpm_res_model4_short.svg", height=20, width=30, unit="cm")

# Merge the incidence to the regression results
regression_models_short <- merge(regression_models_short, incidence, by.x="icpc2", by.y="icpc2_pattern")

# Save the regression results
save(regression_models_short, file="results/regression_results.Rda")

# Plot the relationship between incidence and association
ggplot(subset(regression_models_short, disease_label != "Any complication" & model=="model4"), aes(x=postpartum_proportion, y=estimate)) +
  geom_hline(yintercept=0, colour = "grey") +
  geom_vline(xintercept = 0, colour = "grey") +
  geom_smooth(se=F, method="lm", formula="y~poly(x, 2)", size=0.8, col="grey") +
  geom_point(aes(colour=group)) +
  geom_text_repel(aes(label=disease_label, colour=group)) +
  geom_linerange(aes(ymin=conf.low, ymax=conf.high, colour=group)) +
  facet_wrap(~ model) +
  scale_x_continuous("Incidence", labels = scales::percent, expand=c(0.2, 0), n.breaks=10, trans="log10") +
  scale_y_continuous("Association", n.breaks=10)
ggsave(filename = "figures/incidence_association_plot_short.svg", width=20, height=15, unit="cm")

## 1.3 Joint model on short-term fertility ------------------------------------

# Load the results
models_joint_short <- readRDS("results/lpm_res_joint_short.Rds")

# Bind and plot
models_joint_short <- lapply(models_joint_short, tidy, conf.int=T)
models_joint_short <- rbindlist(models_joint_short, idcol="model")
models_joint_short[, model:=paste("Model", str_extract(model, "\\d"))]

# Fitler the disease codes
models_joint_short <- models_joint_short[term %in% icpc2_pattern, ]

# Merge the groups with the joint data
models_joint_short <- merge(models_joint_short, disease_groups, by.x="term", by.y="icpc2")

# Create a disaese label <- here could the prevalence be added to the data
models_joint_short[, disease_label:=ifelse(term=="any", "Any complication", disease)]
models_joint_short[, mean_association:=mean(estimate), by=disease_label]
models_joint_short[, direction:=ifelse(max(p.value)>0.05, "Not significant", ifelse(mean_association>0, "Positive association", "Negative association")), by=disease_label]
models_joint_short[, significance:=factor(ifelse(p.value<0.05, 1, 0.8), labels=c("Not Significant", "Statistically significant"))]
models_joint_short[, disease_cat := translate_icpc2_chapter(term)]
write.csv(models_joint_short, "results/model_joint_short_groups_direction.csv")

# Plot the regression coefficients
ggplot(subset(models_joint_short, model == "Model 1"), aes(x=fct_reorder(disease_label, mean_association), y=estimate)) +
  geom_hline(yintercept=0, colour = "grey") + 
  geom_point(aes(alpha=significance), position=position_dodge(width=1)) +
  geom_text(aes(label=round(estimate, 2), y = conf.low), position=position_dodge(width=1), hjust=1.1, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high), position=position_dodge(width=1), width=0.5) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_shape("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association", expand = c(0.125, 0)) +
  scale_x_discrete(expand =c(0.1, 0)) +
  scale_alpha_discrete("P-value", range=c(0.4, 1)) +
  facet_wrap(~ direction, scales="free_y", ncol = 1) + 
  coord_flip() +
  theme(axis.title.y=element_blank())
ggsave(filename="figures/lpm_joint_short.svg", height=20, width=15, unit="cm")

## 1.3. Disease groups on short-term fertility ---------------------------------

# Load the results
models_group_short <- readRDS("results/lpm_res_group_short.Rds")

# Bind and plot
models_group_short <- lapply(models_group_short, tidy, conf.int=T)
models_group_short <- rbindlist(models_group_short, idcol="model")
models_group_short[, model:=paste("Model", str_extract(model, "\\d"))]

# Fitler the disease codes
models_group_short <- models_group_short[term %in% unique(disease_groups$group), ]

# Create a disaese label <- here could the prevalence be added to the data
models_group_short[, mean_association:=mean(estimate), by=term]
models_group_short[, max_p:=max(p.value), by =term]
models_group_short[, direction:=ifelse(max_p>0.05, "Not significant", ifelse(mean_association>0, "Positive association", "Negative association"))]
models_group_short[, significance:=factor(ifelse(p.value<0.05, 1, 0.8), labels=c("Not Significant", "Statistically significant"))]
models_group_short[, term := str_replace(term, "_", " ")]

# Plot the regression coefficients
ggplot(models_group_short, aes(x=fct_reorder(term, mean_association), y=estimate, colour=model, shape=model)) +
  geom_hline(yintercept=0, colour = "grey") +
  geom_point(aes(alpha=significance, group=model), position=position_dodge(width=1), size=3) +
  geom_text(aes(label=round(estimate, 2), group=model, y=conf.high), position=position_dodge(width=1), hjust=-0.2, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high, group=model), position=position_dodge(width=1), width=0.2, size=1.2) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_shape("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association", n.breaks=8, expand=c(0.2, 0)) +
  scale_alpha_discrete("P-value", range=c(0.4, 1)) +
  facet_wrap(~ direction, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())
ggsave(filename = "figures/lpm_coeff_group_short.svg")

# 2. Long-term fertility outcomes =============================================

## 2.1. Any condition on long-term chance of 2nd child -------------------------

# Load the any models
any_postpartum_long <- list.files("results", full.names=T, pattern="lpm_res_any_long.Rds")
any_postpartum_long <- readRDS(any_postpartum_long)

latex_table(any_postpartum_long,
          ci=T,
          apply.coef = NULL,
          type="text", 
          label=paste0("tab:reg_any_long"),
          title=paste("Poisson regression of the impact of a single diagosis of any diagnosis in the mother during the pregnancy or the postpartum period on the total number of births 10 years laters. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          #covariate.labels = c(disease, rep("Age group", length(unique()) "Migration backgroun")
          out=paste0("tables/lpm_any_long.tex")
)

create_table(any_postpartum_long, out = "tables/any_long.xlsx")


## Regression models long ----------------------------------------

# List regression files
reg_files <- list.files("results", full.names=T, pattern="lpm_res_.+long.Rds")
reg_files <- reg_files[str_detect(reg_files, paste(icpc2_pattern, collapse="|"))]
regression_models_long <- lapply(reg_files, function(models) {
  tmp <- readRDS(models)
  names(tmp) <- paste0("model", 1:4)
  tmp <- lapply(tmp, tidy, conf.int=T)
  tmp <- rbindlist(tmp, idcol="model")
  return(tmp)
})

regression_models_long <- rbindlist(regression_models_long)

# Round the p-value
regression_models_long[, p.value:=round(p.value, 4)]

# Merge the conversion codes to the model results for the accurate labelling
regression_models_long <- merge(regression_models_long, conversion_codes, all.x=T, by.x="term", by.y="icpc2")

# Create a disaese label <- here could the prevalence be added to the data
regression_models_long[, disease_label:=ifelse(term=="any", "Any complication", paste0(disease, " (", term, ")"))]

# Filter only the regression variables
regression_models_long <- regression_models_long[disease!="", ]

# Save and load the regression results long
save(regression_models_long, file="results/regression_model_long.Rda")

# Classify into positive and negative associations
regression_models_long[, mean_association:=mean(estimate), by=term]
regression_models_long[, direction:=ifelse(max(p.value)>0.05, "Not significant", ifelse(mean_association>0, "Positive association", "Negative association")), by=term]
regression_models_long[, significance:=ifelse(p.value<0.05, "Statistically significant", "Not Significant")]
regression_models_long <- merge(regression_models_long, disease_groups, by.x = c("term", "disease", "condition_nor"), by.y=c("icpc2", "disease", 'condition_nor'))

# Plot the regression coefficients
ggplot(regression_models_long, aes(x=fct_reorder(disease_label, mean_association), y=estimate, colour=model)) +
  geom_hline(yintercept=0, colour = "grey") + 
  geom_point(aes(alpha=significance, group=model), position=position_dodge(width=1)) +
  geom_text(aes(label=round(estimate, 2), y=conf.low, group=model), position=position_dodge(width=1), hjust=1, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high, group=model), position=position_dodge(width=1), width=0.5) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association", expand = c(0.6, 0)) +
  scale_alpha_discrete("P-value", range=c(0.6, 1)) +
  facet_wrap(~ direction, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())
ggsave(filename="figures/lpm_res_long.svg", height=30, width=30, unit="cm")

regression_models_long[, direction:=ifelse(p.value>0.05, "Not significant", ifelse(estimate>0, "Positive association", "Negative association"))]
ggplot(subset(regression_models_long, model == "model4"), aes(x=fct_reorder(disease_label, estimate), y=estimate, colour = group)) +
  geom_hline(yintercept=0, colour = "grey") + 
  geom_point(aes(alpha=significance), size = 3) +
  geom_text(aes(label=round(estimate, 2), y=conf.low), hjust=1.2, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high), position=position_dodge(width=1), width=0.1, size = 1.5) +
  scale_colour_viridis_d("Diesease group:") +
  scale_y_continuous("Association", expand = c(0.5, 0)) +
  scale_alpha_discrete("P-value", range=c(0.6, 1)) +
  facet_wrap(~ direction, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())
ggsave(filename="figures/lpm_res_model4_long.svg", height=25, width=30, unit="cm")

## 2.3 Joint model on long-term fertility --------------------------------------

# Load the results
models_joint_long <- readRDS("results/lpm_res_joint_long.Rds")

# Bind and plot
models_joint_long <- lapply(models_joint_long, tidy, conf.int=T)
models_joint_long <- rbindlist(models_joint_long, idcol="model")
models_joint_long[, model:=paste("Model", str_extract(model, "\\d"))]

# Fitler the disease codes
models_joint_long <- models_joint_long[term %in% icpc2_pattern, ]

# Merge with disease names
models_joint_long <- merge(models_joint_long, conversion_codes, by.x="term", by.y="icpc2")

# Create a disaese label <- here could the prevalence be added to the data
models_joint_long[, disease_label:=ifelse(term=="any", "Any complication", paste0(disease, " (", term, ")"))]
models_joint_long[, mean_association:=round(mean(estimate), 3), by=disease_label]
models_joint_long[, direction:=ifelse(max(p.value)>0.05, "Not significant", ifelse(mean_association>0, "Positive association", "Negative association")), by=disease_label]
models_joint_long[, significance:=factor(ifelse(p.value<0.05, 1, 0.8), labels=c("Not Significant", "Statistically significant"))]
models_joint_long[, disease_cat := translate_icpc2_chapter(term)]

# Plot the regression coefficients
ggplot(models_joint_long, aes(x=fct_reorder(disease_label, mean_association), y=estimate, colour=model, shape=model)) +
  geom_hline(yintercept=0, colour = "grey", linetype = "dotted") + 
  geom_point(aes(alpha=significance), position=position_dodge(width=1)) +
  geom_text(aes(label=round(estimate, 2), y=conf.low), position=position_dodge(width=1), hjust=1, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high), position=position_dodge(width=1), width=0.5) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_shape("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association") +
  scale_alpha_discrete("P-value") +
  facet_wrap(~ direction, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())

# Save the result
write.csv(models_joint_long, file = "results/models_joint_long.csv")

## 2.4 Disease groups on long-term fertility -----------------------------------

# Load the results
models_group_long <- readRDS("results/lpm_res_group_long.Rds")

# Bind and plot
models_group_long <- lapply(models_group_long, tidy, conf.int=T)
models_group_long <- rbindlist(models_group_long, idcol="model")
models_group_long[, model:=paste("Model", str_extract(model, "\\d"))]

# Fitler the disease codes
models_group_long <- models_group_long[term %in% unique(disease_groups$group), ]

# Create a disaese label <- here could the prevalence be added to the data
models_group_long[, mean_association:=mean(estimate), by=term]
models_group_long[, direction:=ifelse(mean_association>0, "Positive association", "Negative association"), by=term]
models_group_long[, direction:=ifelse(max(p.value)>0.05, "Not significant", ifelse(mean_association>0, "Positive association", "Negative association")), by=term]

models_group_long[, significance:=factor(ifelse(p.value<0.05, 1, 0.8), labels=c("Not Significant", "Statistically significant"))]
models_group_long[, term := str_replace(term, "_", " ")]

# Plot the regression coefficients
ggplot(models_group_long, aes(x=fct_reorder2(term, model, mean_association), y=estimate, colour=model, shape=model)) +
  geom_hline(yintercept=0, colour = "grey") + 
  geom_point(aes(alpha=significance, group=model), position=position_dodge(width=1), size = 3) +
  geom_text(aes(label=round(estimate, 2), y=conf.low, group=model), position=position_dodge(width=1), hjust=1.2, family="serif") +
  geom_errorbar(aes( alpha=significance, ymin=conf.low, ymax=conf.high, group=model), position=position_dodge(width=1), width=0.2, size=1.3) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_shape("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association", expand = c(0.5, 0), n.breaks = 6) +
  scale_alpha_discrete("P-value") +
  facet_wrap(~ direction, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())
ggsave(filename = "figures/lpm_coeff_group_long.svg", height = 20, width = 25, unit = "cm")


ggplot(models_group_long, aes(x=fct_reorder(term, mean_association), y=estimate, colour=model, shape=model)) +
  geom_hline(yintercept=0) + 
  geom_point(aes(alpha=significance), position=position_dodge(width=1), size = 2) +
  geom_text(aes(label=round(estimate, 2), y=conf.low), position=position_dodge(width=1), hjust=1.2, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high), position=position_dodge(width=1), width=0.5) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_shape("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association", expand = c(0.15, 0), n.breaks = 6) +
  scale_alpha_discrete("P-value") +
  facet_wrap(~ direction, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())

# 3. Completed cohort fertility ================================================

## 3.1 Condition code on completed fertility -----------------------------------

# Load the completed cohort fertility results
models_cfr <- readRDS("results/poisson_cfr_long.Rds")

# Print the regression table
latex_table(models_cfr,
          ci=T,
          apply.coef=exp,
          type="latex", 
          label=paste0("tab:reg_any_diagnosis"),
          title=paste("Poisson regression of the impact of a single diagosis of any diagnosis in the mother during the pregnancy or the postpartum period on the total number of births 10 years laters. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          #covariate.labels = c(disease, rep("Age group", length(unique()) "Migration backgroun")
          out=paste0("tables/poisson_any_long.tex")
)

create_table(models_cfr, out = "results/poisson_regression.csv", exp = TRUE)

# Counterfactual simulation ===================================================

# Filter the data with long-term fertility observation
df_long <- df_analysis[complete_long==TRUE&year_firstbirth%in%2006:2011, ]

# Create the new birth outcome: birth interval < 10
df_long[, birth:=ifelse(birth_spacing<10&!is.na(birth_spacing), 1, 0)]

df_count <- df_long
df_count[, orig_complications:=any_postnatal]
df_count[, any_postnatal:=0]
df_count$prediction <- predict(models_cfr$mod3_cfr, df_long, type="response")

# The counterfactual impact
df_count[, .(cfr_observed=mean(n_births), cfr_counterfactual = mean(prediction))] 

# Assemble the parities
cbind("observed" = table(df_count$n_births), "counterfactual" = table(round(df_count$prediction)))

# Plot the risk ratios
coeff_cfr <- lapply(models_cfr, tidy)

# Bind the coefficients together
coeff_cfr <- rbindlist(coeff_cfr, idcol="model")
coeff_cfr[, p.value:=round(p.value, 3)]

# Plot the result
ggplot(subset(coeff_cfr, term=="any_postnatal"), aes(x=model, y=exp(estimate))) +
  geom_point() +
  geom_text(aes(label = round(exp(estimate), 3)), vjust = -1)

## 3.2 Group impact on completed fertility -------------------------------------

# Load the result data
coeff_cfr_group <- readRDS("results/lpm_res_group_cfr_long.Rds") 

# Bind and plot
coeff_cfr_group <- lapply(coeff_cfr_group, tidy, conf.int=TRUE)
coeff_cfr_group <- rbindlist(coeff_cfr_group, idcol="model")
coeff_cfr_group[, model:=paste("Model", str_extract(model, "\\d"))]

# Fitler the disease codes
coeff_cfr_group <- coeff_cfr_group[term %in% unique(disease_groups$group), ]

# Create a disaese label <- here could the prevalence be added to the data
coeff_cfr_group[, mean_association:=mean(estimate), by=term]
coeff_cfr_group[, direction:= ifelse(max(p.value)>0.05, "Not significant", ifelse(mean_association>0, "Positive association", "Negative association")), by=term]
coeff_cfr_group[, significance:=factor(ifelse(p.value<0.05, 1, 0.8), labels=c("Not Significant", "Statistically significant"))]

# Plot the regression coefficients
ggplot(coeff_cfr_group, aes(x=fct_reorder(term, mean_association), y=estimate, colour=model, shape=model)) +
  geom_hline(yintercept=0) + 
  geom_point(aes(alpha=significance), position=position_dodge(width=1)) +
  geom_text(aes(label=round(estimate, 2), alpha=significance), position=position_dodge(width=1), vjust=-0.5, family="serif") +
  geom_errorbar(aes(alpha=significance, ymin=conf.low, ymax=conf.high), position=position_dodge(width=1), width=0.2) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_shape("Specification", labels=c("Bivariate", "Age + Birthyear", "Socio-demographic", "Full model")) +
  scale_y_continuous("Association") +
  scale_alpha_discrete("P-value", range=c(0.4, 1)) +
  facet_wrap(~ direction, scales="free_y") + 
  coord_flip() +
  theme(axis.title.y=element_blank())
ggsave(filename = "figures/cfr_coeff_group_long.svg")


### Plot the grouped incidence pattern =========================================

# Load the disease groups
load("documentation/disease_groups.Rda")

# Load the incidence data
incidence_files <- list.files("results", pattern = "incidence_[A-Z]\\d{2}.Rds", full.names=T)

# Load the incidence files
incidence_data <- lapply(incidence_files, readRDS)

# Bind the data together
incidence_data <- rbindlist(incidence_data)

# Merge the disease groups with the incidence data
incidence_data <- merge(x=incidence_data, y=disease_groups, by = c("icpc2", "disease"))

# Clean the labels
incidence_data[, disease := str_replace(disease, " female.+other", "")]
incidence_data[, disease := str_replace(disease, "other", "")]
incidence_data[, disease := str_replace(disease, "symptom/comp.+", "symptom")]
incidence_data[, disease := str_replace(disease, " ", "\n")]
incidence_data[, disease := str_replace(disease, " NOS.?", "")]

# Plot the data
ggplot(data=subset(incidence_data, group != "Other"), aes(x=time, y = prediction*1000, group = disease, colour = disease)) +
  geom_vline(xintercept = 0) +
  geom_text_repel(data = subset(incidence_data, time == 0 & group != "Other"), aes(label = disease), family = "serif", size = 3) +
  geom_point(alpha = 0.3, size=0.5, aes(y=1000*diagnosis/exposure)) +
  geom_line(aes(y = prediction*1000)) +
  facet_wrap(~ group, scales = "free_y", ncol=3) +
  scale_color_viridis_d(option = "turbo") +
  scale_x_continuous("Time relative to birth (t=0)", expand =c(0, 0), labels = unit_format(unit = "", prefix = "t=")) +
  scale_y_continuous("Monthly incidence rate (consultations/1000 person-months)", expand = c(0, 0), n.breaks = 6) +
  guides(color="none") +
  theme(
    
  )
ggsave(filename = "figures/grouped_incidence_rates_final.svg", height = 15, width = 22, unit = "cm")


### END ########################################################################