***

# Postpartum complications and subsequent childbearing: A register-based analysis in Norway

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)

This repository contains the analysis code for the research project investigating the association between postpartum health complications in first-time mothers and subsequent childbearing, utilizing nationwide Norwegian registers.


## 📖 Abstract
This study investigates the association between postpartum health complications in first-time mothers and subsequent childbearing using Norwegian nationwide registers. For all first births in 2008-2017 (N = 276,377), we identified consultations within the first 12 months after birth that suggested postpartum complications. 

**Key Findings:**
- **Prevalence:** About 34.4% of mothers experienced a postpartum health complication.
- **Impact:** Mothers with complications had reduced and postponed subsequent childbearing.
- **Specifics:** Genital diseases (5.8%) and puerperal mastitis (5.6%) were most prevalent, while heart failure (<0.1%) was most strongly associated with a reduction in fertility.
- **Conclusion:** While individual impacts are significant, postpartum health complications have a small impact on the overall national fertility level.

## 👥 Authors
- **Henrik-Alexander Schubert**, D.Phil. (Max Planck Institute for Demographic Research)
- **Øystein Kravdal**, Ph.D. (Norwegian Institute of Public Health / University of Oslo)
- **Emilie Steentoft Dahlberg**, M.D. (Copenhagen University Hospital Hvidovre)

## 🔐 Data Privacy Note
Due to the sensitive nature of the Norwegian nationwide registers and GDPR compliance, the raw data used in this study are not included in this repository and cannot be shared publicly. 


## 📁 Project Structure

```text
.
├── META.R                      # Master script to run the entire pipeline
├── parameters.R                 # Global constants and model specifications
├── paths.R                      # Local directory and file path configurations
├── disease_codes.R              # Mapping of the 40 postpartum complications
├── README.md                    # Project documentation
├── functions/                   # Custom R utility functions
│   ├── create_table.R           # Publication-ready table generation
│   ├── graphic_template.R       # Visual styling and plot themes
│   └── helper.R                 # General utility functions
├── code/                        # Analysis pipeline scripts
│   ├── 00_load_data.R           # Data ingestion
│   ├── 01_prepare_mfr_grid.R    # Mother-Father-Child grid construction
│   ├── 02_prepare_kuhr.R        # KUHR register processing
│   ├── 03_prepare_covariates.R  # Control variable preparation
│   ├── 04_perinatal_incidence.R # Perinatal incidence analysis
│   ├── 05_postnatal_complications.R # Complication identification
│   ├── 06_prepare_analysis_data.R   # Final dataset merging
│   ├── 07_regression_modelling.R    # LPM and Poisson models
│   ├── 08_result_presentation.R     # Result formatting
│   ├── 09_robustness_checks.R       # Sensitivity analyses
│   ├── 10_export_results.R          # Final export of tables/figures
│   ├── event_history_analysis.R     # Longitudinal timing analysis
│   ├── icpc2_grouping.R             # ICPC-2 code grouping
│   ├── infections.R                 # Infection-specific filtering
│   ├── prepregnancy_conditions.R    # Pre-existing condition filtering
│   ├── regional_codes.R             # Geographic identifier handling
│   ├── severe_maternal_morbidity.R  # Severe morbidity analysis
│   └── sibling_identification.R     # Subsequent birth logic
├── figures/                     # Generated plots and visualizations
└── tables/                      # Exported regression and summary tables
```

## 🚀 How to Run the Analysis

1. **Clone the repository:**
   ```bash
   git clone https://github.com/Henrik-Alexander/postpartum_complications_fertility.git
   ```
2. **Configure Environment:** 
   Open `paths.R` and update the paths to point to your local data directories.
3. **Execute Pipeline:**
   Run the master script in R:
   ```r
   source("META.R")
   ```

## 🛠️ Technical Details
- **Data Source:** Norwegian nationwide registers (Primary care consultations).
- **Sample Size:** $N = 276,377$ first births (2008-2017).
- **Models Used:** 
  - Linear Probability Models (LPM)
  - Poisson Regression Models


## 📜 Citation
If you use this code or refer to this study in your work, please cite:

> Schubert, H-A., Kravdal, Ø., & Dahlberg, E. S. (2026). *Postpartum complications and subsequent childbearing: A register-based analysis in Norway*. MPIDR Working Paper.