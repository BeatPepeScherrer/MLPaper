#' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'  
#'  Description:  Function to run selected models for selected countries and store performance metrics in
#'                separate Excel files
#'
#'  Authors:      Kathrin Durizzo, Beat Scherrer               
#'  Date:         30.07.2024
#'  possible function inputs: 
#'  @param country : See column names of variables_overview
#'  @param model : any combination out of c("basic_logit", "logit", "lasso", "random_forest", "neural_network", "xgboost")
#'
#' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# clear workspace
rm(list = ls())

# Packages
library(Hmisc)
library(ROSE)
library(caret)
library(ROCR)
library(glmnet)
library(xgboost)
library(randomForest)
library(dplyr)
library(haven)
library(cli)
library(readxl)
library(openxlsx)
library(fastDummies)

# Source key functions
source("Dofiles/dhs_functions.R")

# Define key vars
countries <- c("Benin","Burkina.Faso","Burundi","Cameroon", "Chad",
               "DRC","Republic.of.the.Congo","Cote.dIvoire","Eswatini","Ethiopia","The.Gambia",
               "Ghana","Guinea","Kenya","Lesotho","Liberia","Madagascar", "Malawi",
               "Mali", "Mauritania", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal","South.Africa",
               "Sierra.Leone", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")
outcome_vars <- c("insurance","locationBirth","postnatalcheck","visitHF12m")
metrics_df <- NA
outcome_vars_without_insurance <- c("locationBirth","postnatalcheck","visitHF12m")
model_list <- c("basic_logit", "subset", "xgboost")

for (outcome_var in outcome_vars) {
  for (country in countries) {
    do.call(run_models, list(country = country, model_list = model_list, outcome_var = outcome_var, short_model = FALSE))
  }
}

# # Run pooled short models
# for (outcome_var in outcome_vars) {
#   do.call(run_models, list(country = "all", model_list = c("lasso", "random_forest", "neural_network", "xgboost"), outcome_var = outcome_var, short_model= TRUE))
# }


