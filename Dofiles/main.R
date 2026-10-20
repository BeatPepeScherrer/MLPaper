# Master Script for Running All Analyses ----

# setwd("L:/research_assistants/Pepe/MLPaper")

# 1. Dependencies ----

# Use packman package to concisely install and load necessary packages
if (!require("pacman")) {
  install.packages("pacman")
}

library(pacman)
p_load(Hmisc, ROSE, caret, ROCR,
       glmnet, xgboost, randomForest, dplyr,
       haven, cli, readxl, writexl, openxlsx,
       fastDummies, purrr, naniar, stats, tidyr,
       ggplot2, eeptools, ggspatial, labelled,
       sf, broom, ggcorrplot, viridisLite, rlang
)

# 2.  Load Custom Functions ----
source("Dofiles/dhs_functions.R")

# # 3. Clean and prepare DHS data for model estimation ----
# source("Dofiles/run_countries.R")

# # 4. Run all models and save performance metrics / importance scores / new data sets ----
# source("Dofiles/run_models.R")

# # 5. Prepare census data set ----
# source("Dofiles/census_merging.R")
# 
# 6. Create some of the Figures and Tables in the paper ----
source("Dofiles/figures_tables.R")

# # 7. Create Maps with out-of-sample predictions, regional- and district-level ----
source("Dofiles/short_model_mapping.R")
