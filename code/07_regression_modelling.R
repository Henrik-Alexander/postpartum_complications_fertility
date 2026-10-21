###
# Project: Perinatal complications and subsequent childbearing 
# Purpose: Regression modelling
# Load the conversion codes
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 03.12.2025
###

rm(list=ls()); gc(TRUE)

library(data.table)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(broom)
library(openxlsx)

# Load the analysis data
load("data/df_analysis.Rda")

# Select diseases codes with expected pattern
source("disease_codes.R")

# Load the conversion codes
load("data/conversion_codes.Rda")

# Load the helpder functions
source("functions/helper.R")

# Load the graphic template
source("functions/graphic_template.R")

# Create table funciton
source("functions/create_table.R")

# Define the covariates
covariates <- c("birth", "any_postnatal", "age_group", "year_firstbirth", "migrant_cat", "income_decile", "edu_level", "relationship", "delivery_mode", "any_pregnancy")

estimate_odds_ratio <- function(births, x) {
  estimate_odds <- function(p) p / (1-p)
  
  estimate_odds(mean(births[x==1])) / estimate_odds(mean(births[x==0])) 
  
}


# Functions ===================================================================

# Estimate the joint model
create_regression_equation <- function(main_predictor=icpc2_pattern, outcome="birth", controls=covariates[2:3]) {
  as.formula(paste(outcome, "~", paste(c(main_predictor, controls), collapse="+")))
}

# Descriptive data =============================================================


# Birth distribution in the whole population
table(df_analysis$birth)
round(100*prop.table(table(df_analysis$birth)), 2)


# By postnatal complication
table(postnatal = df_analysis$any_postnatal, birth = df_analysis$birth)
round(100*prop.table(table(postnatal = df_analysis$any_postnatal, birth = df_analysis$birth), margin = 1), 1)

# Estimate odds ratio
estimate_odds_ratio(df_analysis$birth, df_analysis$any_postnatal)

# Create summary table =========================================================

# Function to summarize categorical values
summarise_cat <- function(cat, varname) {
  frequency <- table(cat)
  proportion <- round(prop.table(frequency), 3)
  tmp <- as.data.frame(cbind(frequency, proportion))
  tmp$values <- rownames(tmp)
  tmp$variable <- varname
  return(tmp[, c("variable", "values", "frequency", "proportion")])
}

# Function to implement the summary to the man dataset
summarise_df <- function(variable, df=df_analysis) {
  summarise_cat(df[[variable]], variable)
}

# Summarize the dataset
tab_sum <- lapply(covariates, summarise_df)
tab_sum <- rbindlist(tab_sum)
write.csv(tab_sum, file="results/summary_table.csv")

# Summarize the dataset
tab_sum_compli <- lapply(covariates, summarise_df, df=df_analysis[any_postnatal==1, ])
tab_sum_compli <- rbindlist(tab_sum_compli)
tab_sum_health <- lapply(covariates, summarise_df, df=df_analysis[any_postnatal==0, ])
tab_sum_health <- rbindlist(tab_sum_health)
tab_sum_predictor <- merge(tab_sum_compli, tab_sum_health, by=c("variable", "values"), suffixes = c("_complications", "_healthy"))

write.csv(tab_sum, file="results/summary_table.csv")
write.csv(tab_sum_predictor, file="results/summary_table_complications.csv")


# Unfold the list of ICPC-2 codes to dummies ===================================

# Create dummies for every individal type
for (code in icpc2_pattern) {
  df_analysis[, (code) := sapply(diagnosis_postpartum, function(x) ifelse(code %in% x, 1, 0))]
}

# Estiamte the rowsums
df_analysis$any_postnatal <- ifelse(rowSums(df_analysis[, ..icpc2_pattern])>0, 1, 0)

## Summarize the postpartum disease prevalence
summary_postpartum_diseases <- lapply(icpc2_pattern , function(icpc2) {
  create_name <- function(icpc2) {
    paste0(conversion_codes$disease[conversion_codes$icpc2==icpc2], " (", icpc2, ")")
  }
  
  print(icpc2)
  tmp <- ifelse(sapply(df_analysis$diagnosis_postpartum, function(x) sum(x %in% icpc2))>0, 1, 0)
  summarise_cat(tmp, create_name(icpc2))
  
})

# Bind the diseases together
summary_postpartum_diseases <- rbindlist(summary_postpartum_diseases)
summary_postpartum_diseases <- summary_postpartum_diseases[values==1, ]
summary_postpartum_diseases[, proportion := round(proportion*100, 1)]
write.csv(summary_postpartum_diseases, file="results/summary_postpartum_diseases.csv")

# Plot the disease prevalence
ggplot(subset(summary_postpartum_diseases, values==1), aes(x=fct_reorder(variable, proportion), y=proportion)) +
  geom_point() +
  coord_flip()

# Write a table of the postpartum conditions
summary_postpartum_diseases[values==1, ] %>%  
  arrange(desc(proportion))


## Summarize the pregnancy incidence
summary_pregnancy_diseases <- lapply(icpc2_pregnancy , function(icpc2) {
  create_name <- function(icpc2) {
    paste0(conversion_codes$disease[conversion_codes$icpc2==icpc2], " (", icpc2, ")")
  }
  
  print(icpc2)
  tmp <- ifelse(sapply(df_analysis$diagnosis_pregnancy, function(x) sum(x %in% icpc2))>0, 1, 0)
  summarise_cat(tmp, create_name(icpc2))
  
})

# Bind the diseases together
summary_pregnancy_diseases <- rbindlist(summary_pregnancy_diseases)
write.csv(summary_pregnancy_diseases, file="results/summary_pregnancy_diseases.csv")

# Create the disease groupings =================================================

# Load the disease groups
source("code/icpc2_grouping.R")

# Create a vector for the groups
icpc2_groups <- unique(disease_groups$group)

# Create dummies for every individal type
for (group in icpc2_groups) {
  df_analysis[, (group) := sapply(diagnosis_postpartum, function(x) ifelse(any(x %in% disease_groups$icpc2[disease_groups$group==group]), 1, 0))]
}

# Summarise the groups
tab_groups <- lapply(icpc2_groups, function(group) summarise_cat(cat=df_analysis[[group]], varname=group))
tab_groups <- rbindlist(tab_groups)

# Plot the share of the disease groups
ggplot(subset(tab_groups, values==1), aes(x=fct_reorder(variable, proportion), y=proportion)) +
  geom_col() +
  geom_text(aes(label=paste0(proportion*100, "%")), vjust="top", col="white", family="serif", size=5) +
  scale_y_continuous("Share affected", labels=scales::percent, expand=c(0, 0)) +
  scale_x_discrete("Postpartum condition groups")
ggsave(filename="figures/share_disease_groups.svg")

# Estimate the incidence =======================================================

# Estimate the frequency of the conditions
postpartum_frequency <- apply(df_analysis[, ..icpc2_pattern], 2, sum)
postpartum_proportion  <- postpartum_frequency / nrow(df_analysis)

# Bind the columns together
incidence <- data.table(icpc2_pattern, postpartum_frequency, postpartum_proportion)

# Save the data
save(incidence, file="data/incidence_main_analysis.Rda")

# Merge with the conversion codes
incidence <- merge(incidence, conversion_codes, by.x="icpc2_pattern", by.y="icpc2", all.x=T)
incidence$disease[incidence$condition=="any"] <- "Any condition"

# Plot the incidence
ggplot(subset(incidence, postpartum_proportion>0.01), aes(x=fct_reorder(disease, postpartum_proportion), y=postpartum_proportion)) +
  geom_point() +
  coord_flip() +
  scale_y_continuous("Prevalence", label=scales::percent, n.breaks=10) +
  scale_x_discrete() +
  theme(axis.title.x=element_blank())
rm(incidence, incidence_list); gc(T)

# Model the experience of postpartum conditions ================================

# The different models
mod1_ppc <- lm(any_postnatal ~ age_group + year_firstbirth, data=df_analysis)
mod2_ppc <- lm(any_postnatal ~ age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship , data=df_analysis)
mod3_ppc <- lm(any_postnatal ~ age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship  + delivery_mode + any_pregnancy, data=df_analysis)

# Collect the models
models_ppc <- mget(ls(pattern="mod\\d_ppc"))
saveRDS(models_ppc, file="results/models_predicting_ppc.Rds")

# Create a latex table
stargazer(models_ppc,
          ci=T,
          type="latex", 
          label=paste0("tab:predicting_postpartum"),
          title=paste("Linear probability model of socio-demogrpahic and health predictors on having a post-partum complication. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          #covariate.labels = c(disease, rep("Age group", length(unique()) "Migration backgroun")
          out=paste0("tables/predicting_postpartum_condition.tex"))

# Create a list of factors
models_ppc <- lapply(models_ppc, tidy, conf.int=T)

# Bind the models to a data frame together
models_ppc <- rbindlist(models_ppc, idcol="model")

# Create the model name
models_ppc[, model:=paste("Model", str_extract(model, "\\d"))]

# Estimate the man association
models_ppc[, mean_association:=mean(estimate), by=term]

# Classify into positive and negative associations
models_ppc[, direction:=ifelse(mean_association>0, "Positive association", "Negative association"), by=term]

# Plot the regression coefficients
ggplot(models_ppc, aes(x=term, y=estimate, colour=model)) +
  geom_hline(yintercept=0) + 
  geom_point(aes(alpha=ifelse(p.value<0.05, 1, 0.8)), position=position_dodge(width=0.75)) +
  geom_text(aes(label=round(estimate, 2), y=conf.low), position=position_dodge(width=0.75), hjust=1, family="serif") +
  geom_errorbar(aes(alpha=ifelse(p.value<0.05, 1, 0.8), ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.75), width=0.5) +
  scale_colour_viridis_d("Specification", labels=c("Bivariate", "Age + Birthyear", "Full model")) +
  scale_y_continuous("Association") +
  #scale_alpha_manual(labels=c("Statistically significant", "Not Significant")) +
  facet_wrap(~ direction, scales="free") + 
  coord_flip() +
  theme(axis.title.y=element_blank())

# Bivariate associations =======================================================

# Share with postpartum complications
prop.table(table(df_analysis$any_postnatal))

# Plot the share of births with a subsequent child
prop.table(table(df_analysis$birth))

# Plot by birth
prop_table <- prop.table(table("birth"=df_analysis$birth, "postpartum complications" = df_analysis$any_postnatal), margin=2)

# Estimate the odds-ratio of having a birth
(prop_table[2, 2] / prop_table[2, 1]) / (prop_table[1, 2] / prop_table[1, 1])

# Look at the age distribution
hist(df_analysis$age_m)

# Plot the postpartum complications by age
ggplot(df_analysis, aes(x=age_m, group=any_postnatal, colour=as.factor(any_postnatal))) +
  geom_density()

# Look at the birth year distribution
hist(df_analysis$faar)

# Plot the waiting time
hist(df_analysis$birth_spacing, breaks=30)
abline(v=mean(df_analysis$birth_spacing), lwd=2, col="red")

# Plot the waiting time by complications
ggplot(data=df_analysis, aes(x=birth_spacing, group=factor(any_postnatal), fill=factor(any_postnatal))) +
  geom_density(alpha=0.3) +
  geom_vline(aes(xintercept = mean(birth_spacing, na.rm=T), group =factor(any_postnatal), colour=factor(any_postnatal)))

# 1. Model short-term fertility ===================================================

## 1.1 Model the birth propensity for all diseases combined ---------------------

# The different models
mod1 <- lm(birth ~ any_postnatal, data=df_analysis)
mod2 <- lm(birth ~ any_postnatal + age_group + year_firstbirth, data=df_analysis)
mod3 <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship, data=df_analysis)
mod4 <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship  + delivery_mode + any_pregnancy, data=df_analysis)

# Coolect the models
models <- list(mod1, mod2, mod3, mod4)

# Print the regression table
latex_table(models,
          ci=T,
          apply.coef = NULL,
          type="latex", 
          label=paste0("tab:reg_any_diagnosis"),
          title=paste("Linear regression of the impact of a single diagosis of any diagnosis in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 3 years later. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          out=paste0("tables/impact_birth_any_short.tex"))
saveRDS(models, file=paste0("results/lpm_res_any_short.Rds"))

# Export the full table
create_table(models, out = "results/full_main.xlsx")

## 1.2. Model the propensity for the individual disease --------------------------

lapply(icpc2_pattern, function(icpc2) {
  
  cat("Disease:", icpc2, "\n")
  
  # Extract the disease name
  disease <- conversion_codes$disease[conversion_codes$icpc2==icpc2]
  
  # Run the regression models
  tmp_mod1 <- lm(create_regression_equation(icpc2, "birth", NULL), data=df_analysis)
  tmp_mod2 <- lm(create_regression_equation(icpc2, "birth", covariates[3:3]), data=df_analysis)
  tmp_mod3 <- lm(create_regression_equation(icpc2, "birth", covariates[3:8]), data=df_analysis)
  tmp_mod4 <- lm(create_regression_equation(icpc2, "birth", covariates[3:10]), data=df_analysis)
  
  # Collect the regression models
  tmp_models <- list(tmp_mod1, tmp_mod2, tmp_mod3, tmp_mod4)
  
  # Print the regression table
  stargazer(tmp_models,
            ci=T,
            type="latex", 
            label=paste0("tab:reg_", icpc2),
            title=paste("Linear regression of the impact of a single diagosis of", disease, "in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 3 years later. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
            #covariate.labels = c(disease, rep("Age group", length(unique()) "Migration backgroun")
            out=paste0("tables/impact_birth_", icpc2, "short.tex")
            )
  # Save the model results
  saveRDS(tmp_models, file=paste0("results/lpm_res_", icpc2, "_short.Rds"))
  
})

## 1.3. Estimate the joint model -----------------------------------------------

# The different models
mod1_joint_short <- lm(create_regression_equation(icpc2_pattern, "birth", NULL), data=df_analysis)
mod2_joint_short <- lm(create_regression_equation(icpc2_pattern, "birth", covariates[3:3]), data=df_analysis)
mod3_joint_short <- lm(create_regression_equation(icpc2_pattern, "birth", covariates[3:8]), data=df_analysis)
mod4_joint_short <- lm(create_regression_equation(icpc2_pattern, "birth", covariates[3:10]), data=df_analysis)

# Collect the results
models_joint_short <- mget(ls(pattern="mod\\d_joint_short"))

# Save the joint model
saveRDS(models_joint_short, file="results/lpm_res_joint_short.Rds")

## 1.4 Estimate the group model ------------------------------------------------

# The different models
mod1_group_short <- lm(create_regression_equation(icpc2_groups, "birth", NULL), data=df_analysis)
mod2_group_short <- lm(create_regression_equation(icpc2_groups, "birth", covariates[3:3]), data=df_analysis)
mod3_group_short <- lm(create_regression_equation(icpc2_groups, "birth", covariates[3:8]), data=df_analysis)
mod4_group_short <- lm(create_regression_equation(icpc2_groups, "birth", covariates[3:10]), data=df_analysis)

# Collect the results
models_group_short <- mget(ls(pattern="mod\\d_group_short"))

# Save the group model
saveRDS(models_group_short, file="results/lpm_res_group_short.Rds")

# Save the group model
stargazer(models_group_short,
          ci=T, apply.coef = exp,
          type="latex", 
          label="tab:reg_group_short",
          title=paste("Linear regression of the impact of a single diagosis of disease groups in the mother during the postpartum period on the probability of having a subsequent birth 3 years later. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          #covariate.labels = c(disease, rep("Age group", length(unique()) "Migration backgroun")
          out=paste0("tables/lpm_group_short.tex")
)


# 2. Analyze the long-term fertility ===========================================

# Filter the data with long-term fertility observation
df_long <- df_analysis[complete_long==TRUE & year_firstbirth %in% 2006:2011, ]

# Create teh new birht outcome: birth interval < 10
df_long[, birth:=ifelse(birth_spacing<10&!is.na(birth_spacing), 1, 0)]

## 2.0 Describe the long-term data ---------------------------------------------

# Second birth propensity
prop.table(table(df_long$birth))

# Second birth propensity
prop_table <- prop.table(table(birth=df_long$birth, postpartum = df_long$any_postnatal), margin=2)

# Overall distribution of parities 10 years after the first birth
round(prop.table(table(df_long$n_births))*100, 2)

# Mean number of children 10 years later
tapply(df_long$n_births, df_long$any_postnatal, mean)

# Total number of births
tab_parity <- round(100*prop.table(table(birth=df_long$n_births, postpartum = df_long$any_postnatal), margin=2), 2)
tab_parity_odds <- apply(tab_parity, 2, function(x) x / (100-x))
odds_ratios_parity <- tab_parity_odds[, 2]/tab_parity_odds[, 1]

## 2.1 Model the long-term second birth propensity -----------------------------

# The different models
mod1_long <- lm(birth ~ any_postnatal, data=df_long)
mod2_long <- lm(birth ~ any_postnatal + age_group + year_firstbirth, data=df_long)
mod3_long <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship , data=df_long)
mod4_long <- lm(birth ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship  + delivery_mode + any_pregnancy, data=df_long)

# Coolect the models
models_long <- mget(ls(pattern = "mod\\d_long"))

# Print the regression table
stargazer(models_long,
          ci=T,
          type="latex", 
          label=paste0("tab:reg_any_diagnosis"),
          title=paste("Linear regression of the impact of a single diagosis of any diagnosis in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 10 years later. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          #covariate.labels = c("Any postnatal:", rep("Age group:", 3), "Migration background:"),
          out=paste0("tables/impact_birth_any_long.tex"))
saveRDS(models_long, file=paste0("results/lpm_res_any_long.Rds"))

## 2.2. Model the long-term probability for individual disesaes ---------------

lapply(icpc2_pattern, function(icpc2) {
  
  cat("Disease:", icpc2, "\n")
  
  # Extract the disease name
  disease <- conversion_codes$disease[conversion_codes$icpc2==icpc2]
  
  # Run the regression models
  tmp_mod1 <- lm(create_regression_equation(icpc2, "birth", NULL), data=df_long)
  tmp_mod2 <- lm(create_regression_equation(icpc2, "birth", covariates[3:3]), data=df_long)
  tmp_mod3 <- lm(create_regression_equation(icpc2, "birth", covariates[3:8]), data=df_long)
  tmp_mod4 <- lm(create_regression_equation(icpc2, "birth", covariates[3:10]), data=df_long)
  
  # Collect the regression models
  tmp_models <- list(tmp_mod1, tmp_mod2, tmp_mod3, tmp_mod4)
  
  # Print the regression table
  stargazer(tmp_models,
            ci=T, apply.coef = exp,
            type="latex", 
            label=paste0("tab:reg_", icpc2),
            title=paste("Logistic regression of the impact of a single diagosis of", disease, "in the mother during the pregnancy or the postpartum period on the probability of having a subsequent birth 3 years later. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
            #covariate.labels = c(disease, rep("Age group", length(unique()) "Migration backgroun")
            out=paste0("tables/impact_birth_", icpc2, "long.tex")
  )
  # Save the model results
  saveRDS(tmp_models, file=paste0("results/lpm_res_", icpc2, "_long.Rds"))
  
})


## 2.3 Estimate the joint long-term model --------------------------------------

# The different models
mod1_joint_long <- lm(create_regression_equation(icpc2_pattern, "birth", NULL), data=df_long)
mod2_joint_long <- lm(create_regression_equation(icpc2_pattern, "birth", covariates[3:3]), data=df_long)
mod3_joint_long <- lm(create_regression_equation(icpc2_pattern, "birth", covariates[3:8]), data=df_long)
mod4_joint_long <- lm(create_regression_equation(icpc2_pattern, "birth", covariates[3:10]), data=df_long)

# Collect the results
models_joint_long <- mget(ls(pattern="mod\\d_joint_long"))

# Save the joint model
saveRDS(models_joint_long, file="results/lpm_res_joint_long.Rds")

## 1.4 Estimate the group model ------------------------------------------------

# The different models
mod1_group_long <- lm(create_regression_equation(icpc2_groups, "birth", NULL), data=df_long)
mod2_group_long <- lm(create_regression_equation(icpc2_groups, "birth", covariates[3:3]), data=df_long)
mod3_group_long <- lm(create_regression_equation(icpc2_groups, "birth", covariates[3:8]), data=df_long)
mod4_group_long <- lm(create_regression_equation(icpc2_groups, "birth", covariates[3:10]), data=df_long)

# Collect the results
models_group_long <- mget(ls(pattern="mod\\d_group_long"))

# Save the group model
saveRDS(models_group_long, file="results/lpm_res_group_long.Rds")


# Save the group model
stargazer(models_group_long,
          ci=T,
          type="latex", 
          label="tab:reg_group_short",
          title=paste("Linear regression of the impact of a single diagosis of disease groups in the mother during the postpartum period on the probability of having a subsequent birth 10 years later. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          #covariate.labels = c(disease, rep("Age group", length(unique()) "Migration backgroun")
          out=paste0("tables/lpm_group_long.tex")
)

## 2.3 Model the long term number of total births ------------------------------

# The different models
mod1_cfr <- glm(n_births ~ any_postnatal, data=df_long, family="poisson")
mod2_cfr <- glm(n_births ~ any_postnatal + age_group + year_firstbirth, data=df_long, family="poisson")
mod3_cfr <- glm(n_births ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship , data=df_long, family="poisson")
mod4_cfr <- glm(n_births ~ any_postnatal + age_group + year_firstbirth + migrant_cat + income_decile + edu_level + relationship  + delivery_mode + any_pregnancy, data=df_long, family="poisson")

# Get the models
models_cfr <- mget(ls(pattern="mod\\d_cfr"))

# Save the completed fertility
saveRDS(models_cfr, file="results/poisson_cfr_long.Rds")

# Save the group model
stargazer(models_cfr,
          apply.coef=exp, 
          apply.ci=exp,
          type="latex", 
          label="tab:reg_group_short",
          title=paste("Poisson regression of the impact of a single diagosis of disease groups in the mother during the postpartum period on the completed fertility 10 years later. Model 1 is a binary regression, Model 2 accounts for the age of the mother at first birth, and Model 3 includes the complete list of control variables. Results are presented in odds-ratios."),
          #covariate.labels = c(disease, rep("Age group", length(unique()) "Migration backgroun")
          out=paste0("tables/poisson_any_long.tex")
)

create_table(models_cfr, out = "tables/poisson_any_long.xlsx", exp=TRUE)



## 2.4 Group the long-term total number of births by group ---------------------

# The different models
mod1_group_cfr_long <- glm(create_regression_equation(icpc2_groups, "n_births", NULL), data=df_long, family="poisson")
mod2_group_cfr_long <- glm(create_regression_equation(icpc2_groups, "n_births", covariates[3:3]), data=df_long, family = "poisson")
mod3_group_cfr_long <- glm(create_regression_equation(icpc2_groups, "n_births", covariates[3:8]), data=df_long, family = "poisson")
mod4_group_cfr_long <- glm(create_regression_equation(icpc2_groups, "n_births", covariates[3:10]), data=df_long, family = "poisson")

# Collect the results
models_group_long <- mget(ls(pattern="mod\\d_group_cfr_long"))

# Save the group model
saveRDS(models_group_long, file="results/lpm_res_group_cfr_long.Rds")

### END ########################################################################