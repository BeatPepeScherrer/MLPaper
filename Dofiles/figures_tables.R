#' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'  
#'  Description:  This script creates Tables and Figures for the Paper (e.g. Boxplots)
#'
#'  Authors:      Kathrin Durizzo, Beat Scherrer               
#'  Date:         30.07.2024
#'
#' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# clear workspace
rm(list = ls())

# Packages
library(tidyr)
library(ggplot2)
library(readxl)
library(dplyr)
library(openxlsx)

# Source helper scripts
source("Dofiles/dhs_functions.R")

# Define key variables
path_boxplots <- "Boxplots/data"
path_importance <- "varimp_plots/data"
path_boxplots_short <- "short_model/Boxplots/data"
path_importance_short <- "short_model/varimp_plots/data"

outcome_vars <- c("insurance","locationBirth","postnatalcheck", "visitHF12m")

short_model <- FALSE

countries <- c("Benin","Burkina.Faso","Burundi","Cameroon", "Chad",
               "DRC","Republic.of.the.Congo","Cote.dIvoire","Eswatini","Ethiopia","The.Gambia",
               "Ghana","Guinea","Kenya","Lesotho","Liberia","Madagascar", "Malawi",
               "Mali", "Mauritania", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal","South.Africa",
               "Sierra.Leone", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")

performance_df_merged <- NA
importance_df_merged <- NA

models <- c("importance_subset", "importance_general", "shap_importance")

# # Compute sample average of proportion insured
# files <- list.files("New_data_sets/insurance", pattern = ".RData$", full.names = TRUE)
# # Read all files and combine them into a single data frame
# for (country in 1:length(countries)) {
#   set <- as.data.frame(get(load(files[[country]]))[["insurance"]])
#   if (!exists("all_data")) {
#     all_data <- set
#   } else {
#     all_data <- rbind(all_data, set)
#   }
# }
# prop_insured <- sum(all_data)/nrow(all_data)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# I. Boxplots ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CALL THE FUNCTION: 
# for (outcome_var in outcome_vars) {
#   do.call(create_boxplots, list(outcome_var = outcome_var, short_model = FALSE))
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# II. Variable Importance Plots ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CALL FUNCTION: Create variable importance plots
# for (outcome_var in outcome_vars) {
#   for (model in c("shap_importance")) {
#     for (rank in c(FALSE)) {
#       do.call(create_importance_plot, list(path = path_importance, model = model, rank = rank, outcome_var = outcome_var, short_model = FALSE))
#     }
#   }
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# III. Scatterplots ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CALL THE FUNCTION:
# for (metric in c("Kappa", "Sensitivity", "Specificity", "Accuracy")) {
#   for (outcome_var in outcome_vars) {
#     do.call(create_scatterplots, list(outcome_var = outcome_var, metric = metric, short_model = FALSE))
#   }
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# IV. Merged Variable importance Table and Importance rank Figure ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
# Create merged varimp data table for each outcome
# for (outcome_var in outcome_vars) {
#   do.call(create_varimp_data_table, list(outcome_var = outcome_var, short_model = FALSE))
# }

# # Create figure that shows how many times variable importance of given variable was ranked in top 5
# short_model <- FALSE
# model_size <- ifelse(short_model == FALSE, "long", "short")
# outcome_var <- "postnatalcheck"
# for (country in countries) {
#   # List all relevant .xlsx files
#   if (short_model == TRUE) {
#     file <- list.files(paste0(path_importance_short,"_",outcome_var), pattern = paste0("*\\",country,"_short.xlsx$"), full.names = TRUE)
#   } else {
#     file <- list.files(paste0(path_importance,"_",outcome_var), pattern = paste0("*\\",country,".xlsx$"), full.names = TRUE)
#   }
#   # Read all files and combine them into a single data frame
#   importance_rank_df <- as.data.frame(lapply(file, read_excel))
#   # keep relevant cols only
#   importance_rank_df_gini <- importance_rank_df %>% dplyr::select(c(variables, shap_importance))
#   importance_rank_df_shap <- importance_rank_df %>% dplyr::select(c(variables, shap_importance))
#   # sort and filter for top 5, create variable "rank"
#   importance_rank_df_gini <- importance_rank_df_gini %>% dplyr::arrange(desc(shap_importance))
#   importance_rank_df_gini <- importance_rank_df_gini[1:5,]
#   importance_rank_df_gini$rank <- 1:5
#   importance_rank_df_gini$country <- country
#   importance_rank_df_shap <- importance_rank_df_shap %>% dplyr::arrange(desc(shap_importance))
#   importance_rank_df_shap <- importance_rank_df_shap[1:5,]   
#   importance_rank_df_shap$rank <- 1:5
#   importance_rank_df_shap$country <- country
#   if (country == first(countries)) {
#     importance_rank_df_shap_merged <- importance_rank_df_shap
#     importance_rank_df_gini_merged <- importance_rank_df_gini
#   } else {
#     importance_rank_df_shap_merged <- rbind(importance_rank_df_shap_merged, importance_rank_df_shap)
#     importance_rank_df_gini_merged <- rbind(importance_rank_df_gini_merged, importance_rank_df_gini)
#   }
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# V. Coefficient Sign Tables ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# # Create function to inspect coefficient signs
# coef_sign_df_merged <- NULL
# for (outcome_var in outcome_vars) {
#   for (country in countries) {
#     file <- list.files(paste0(path_importance,"_",outcome_var), pattern = paste0("importance.*\\",country,".xlsx$"), full.names = TRUE)
#     # Read all files and combine them into a single data frame
#     coef_sign_df <- as.data.frame(lapply(file, read_excel))
#     coef_sign_df <- coef_sign_df %>% dplyr::select(c(variables,Negative_subset))
#     names(coef_sign_df) <- c("variables",paste0("negative_",country,"_",outcome_var))
#     if (is.null(coef_sign_df_merged)) {
#       coef_sign_df_merged <- coef_sign_df
#     } else {
#       coef_sign_df_merged <- merge(coef_sign_df_merged, coef_sign_df, by = "variables", all = TRUE) 
#     }
#   }
#   coef_sign_df_merged$prop <- rowSums(coef_sign_df_merged[,-1])/(ncol(coef_sign_df_merged)-1)
#   write.xlsx(coef_sign_df_merged, paste0("coef_signs/coef_signs_",outcome_var,".xlsx"))
#   coef_sign_df_merged <- NULL
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# VI. Performance Table (main model specifications) ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# model_size <- ifelse(short_model == FALSE, "long", "short")
# for model_size in c("long", "short") {
#   short_model <- ifelse(model_size == "long", FALSE, TRUE)
#   for (outcome_var in outcome_vars) {
#     cli::cli_alert_info(paste0("Creating performance table for model type: ", model_size, ", outcome variable: ", outcome_var))
#     for (country in countries) {
#       # List all relevant .xlsx files
#       if (short_model == TRUE) {
#         file <- list.files(paste0(path_boxplots_short,"_",outcome_var), pattern = paste0("metrics.*\\",country,"_short.xlsx$"), full.names = TRUE)
#       } else {
#         file <- list.files(paste0(path_boxplots,"_",outcome_var), pattern = paste0("metrics.*\\",country,".xlsx$"), full.names = TRUE)
#       }
#       # Read all files and combine them into a single data frame
#       performance_df <- as.data.frame(lapply(file, read_excel))
#       performance_df$Country <- country
#       performance_df <- performance_df %>%
#         dplyr::select(all_of("Country"), everything())
#       if (country == first(countries)) {
#         performance_df_merged <- performance_df
#       } else {
#         performance_df_merged <- rbind(performance_df_merged, performance_df)
#       }
#     }

#     # Drop general model
#     if (short_model == FALSE & "logit_general_insurance" %in% colnames(performance_df_merged)) {
#       performance_df_merged <- performance_df_merged %>% dplyr::select(-c(logit_general_insurance))
#     }

#     # Filter for the relevant performance metrics
#     performance_df_merged <- performance_df_merged %>%
#       dplyr::filter(model_names %in% c("Accuracy", "Kappa", "Sensitivity", "Specificity"))

#     # Rename columns
#     if (short_model == FALSE) {
#       performance_df_merged <- performance_df_merged %>% dplyr::rename("Performance_Metric" = model_names,
#                                                                       "ML Model" = xgboost_cv_insurance,
#                                                                       "Classical Model" = logit_subset_insurance)
#     } else {
#       performance_df_merged <- performance_df_merged %>% dplyr::rename("Performance_Metric" = model_names,
#                                                                       "ML Model" = xgboost_cv_insurance,
#                                                                       "Classical Model" = logit_general_insurance)
#     }
#     # Rename some values
#     performance_df_merged <- performance_df_merged %>%
#       mutate(Performance_Metric = gsub("Kappa", "Cohen's Kappa", Performance_Metric))

#     # Reshape data to long format for ggplot
#     performance_df_merged <- performance_df_merged %>%
#       pivot_longer(cols = contains("Model"),
#                   names_to = "Model",
#                   values_to = "Value")

#     performance_df_merged$Model <- factor(performance_df_merged$Model, levels = c("Classical Model", "ML Model"))

#     # Order the model names
#     performance_df_merged$Performance_Metric <- factor(performance_df_merged$Performance_Metric,
#                                                       levels = c("Accuracy", "Sensitivity", "Specificity", "Cohen's Kappa"))

#     # Create final table
#     average_performance_df <- performance_df_merged %>%
#       group_by(Performance_Metric, Model) %>%
#       summarise(Average = mean(Value), sd = sd(Value))
#     average_performance_df_long <- average_performance_df %>%
#       pivot_wider(names_from = Model, values_from = c(Average, sd))
#     average_performance_df_long$Outcome <- outcome_var
#     # Outcome should be first var
#     average_performance_df_long <- average_performance_df_long %>%
#       select(all_of("Outcome"), everything())
#     if (outcome_var == first(outcome_vars)) {
#       average_performance_df_long_total <<- average_performance_df_long
#     } else {
#       average_performance_df_long_total <<- rbind(average_performance_df_long_total, average_performance_df_long)
#     }
#       cli::cli_alert_info(paste0("Creating performance table for model type: ", model_size, ", outcome variable: ", outcome_var, "DONE!"))
#   }
#   write.xlsx(average_performance_df_long_total, paste0("Tables_Figures/average_performance_table_",model_size,".xlsx"))
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# VI. Performance Table (appendix, all models) ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for (outcome_var in outcome_vars) {
  for (country in countries) {
    # List all relevant .xlsx files
    file <- list.files(paste0(path_boxplots,"_",outcome_var), pattern = paste0("metrics.*\\",country,".xlsx$"), full.names = TRUE)
    # Read all files and combine them into a single data frame
    performance_df <- as.data.frame(lapply(file, read_excel))
    performance_df$Country <- country
    performance_df <- performance_df %>%
      dplyr::select(all_of("Country"), everything())
    if (country == first(countries)) {
      performance_df_merged <- performance_df
    } else {
      performance_df_merged <- rbind(performance_df_merged, performance_df)
    }
  }
  
  # Drop general model
  if (short_model == FALSE & "logit_general_insurance" %in% colnames(performance_df_merged)) {
    performance_df_merged <- performance_df_merged %>% dplyr::select(-c(logit_general_insurance))
  }

  # Filter for the relevant performance metrics
  performance_df_merged <- performance_df_merged %>% 
    dplyr::filter(model_names %in% c("Accuracy", "Kappa", "Sensitivity", "Specificity"))
  
  # Rename columns
  performance_df_merged <- performance_df_merged %>% dplyr::rename("Performance_Metric" = model_names,
                                                                   "Lasso Model" = lasso_cv_insurance,
                                                                   "RF Model" = rf_cv_insurance,
                                                                   "Neural Net Model" = nnet_cv_insurance,
                                                                   "XGBoost Model" = xgboost_cv_insurance)
  # Rename some values
  performance_df_merged <- performance_df_merged %>%
    mutate(Performance_Metric = gsub("Kappa", "Cohen's Kappa", Performance_Metric))
  
  # Reshape data to long format for ggplot
  performance_df_merged <- performance_df_merged %>%
    pivot_longer(cols = contains("Model"), 
                 names_to = "Model", 
                 values_to = "Value")
  
  performance_df_merged$Model <- factor(performance_df_merged$Model, levels = c("Lasso Model", "RF Model", "Neural Net Model", "XGBoost Model"))
  
  # Order the model names
  performance_df_merged$Performance_Metric <- factor(performance_df_merged$Performance_Metric, 
                                                     levels = c("Accuracy", "Sensitivity", "Specificity", "Cohen's Kappa"))
  
  # Create final table
  average_performance_df <- performance_df_merged %>%
    group_by(Performance_Metric, Model) %>%
    summarise(Average = mean(Value), sd = sd(Value))
  average_performance_df_long <- average_performance_df %>%
    pivot_wider(names_from = Model, values_from = c(Average, sd))
  average_performance_df_long$Outcome <- outcome_var
  # Outcome should be first var
  average_performance_df_long <- average_performance_df_long %>%
    select(all_of("Outcome"), everything())
  if (outcome_var == first(outcome_vars)) {
    average_performance_df_long_total <<- average_performance_df_long
  } else {
    average_performance_df_long_total <<- rbind(average_performance_df_long_total, average_performance_df_long)
  }
}
write.xlsx(average_performance_df_long_total, paste0("Tables_Figures/average_performance_table_appendix.xlsx"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# VII. Correlation Plot ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# for (outcome_var in outcome_vars) {
#   do.call(create_corrplot, list(short_model = TRUE, outcome_var = outcome_var))
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# VIII. Summary stats Tables ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# map_types <- c("predicted_census_district", "predicted_dhs_region_long", "predicted_dhs_region_short", "average_dhs_region")
# outcome_vars <- c("insurance", "locationBirth", "postnatalcheck", "visitHF12m")
# for (map_type in map_types) {
#   for (outcome_var in outcome_vars) {
#     do.call(create_summary_stats, list(outcome_var = outcome_var, map_type = map_type))
#   }
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# IX. Wealth Quintile Plots ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# outcome_vars <- c("insurance")
# for (outcome_var in outcome_vars) {
#   do.call(create_wealth_quintile_plots, list(outcome_var = outcome_var, short_model = FALSE))
# }


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# X. Arrange maps ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# countries <- c("Benin","Burkina.Faso","Burundi","Cameroon", "Chad",
#                  "DRC","Republic.of.the.Congo","Cote.dIvoire","Eswatini","Ethiopia","The.Gambia",
#                  "Ghana","Guinea","Kenya","Lesotho","Liberia","Madagascar", "Malawi",
#                  "Mali", "Mauritania", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal","South.Africa",
#                  "Sierra.Leone", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")

# country_title_list <- list(
#   Burkina.Faso = "Burkina Faso",
#   Benin = "Benin",
#   Burundi = "Burundi",
#   Cameroon = "Cameroon",
#   DRC = "DRC",
#   Republic.of.the.Congo = "Republic of the Congo",
#   Cote.dIvoire = "Côte d'Ivoire",
#   Eswatini = "Eswatini",
#   Ethiopia = "Ethiopia",
#   The.Gambia = "The Gambia",
#   Ghana = "Ghana",
#   Guinea = "Guinea",
#   Kenya = "Kenya",
#   Liberia = "Liberia",
#   Lesotho = "Lesotho",
#   Madagascar = "Madagascar",
#   Mauritania = "Mauritania",
#   Mali = "Mali",
#   Malawi = "Malawi",
#   Nigeria = "Nigeria",
#   Niger = "Niger",
#   Namibia = "Namibia",
#   Rwanda = "Rwanda",
#   South.Africa = "South Africa",
#   Sierra.Leone = "Sierra Leone",
#   Senegal = "Senegal",
#   Chad = "Chad",
#   Togo = "Togo",
#   Tanzania = "Tanzania",
#   Uganda = "Uganda",
#   Zambia = "Zambia",
#   Zimbabwe = "Zimbabwe"
# )

# for (country in countries) {
#   country_title <- country_title_list[[country]]
#   do.call(arrange_maps, list(country = country, country_title = country_title))
# }