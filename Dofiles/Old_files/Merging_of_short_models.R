#############################################################################################################
#  
#  Description: Model
#
#  Authors:     Kathrin Durizzo               
#  Date:        14.01.2025
#
#############################################################################################################

#------------------------------------------------------------------------------------------------------------
#
#                                         Setup
#
#------------------------------------------------------------------------------------------------------------
options(warn = -1)

library(rgdal)
library(broom)
library(dplyr)
library(labelled)
library(ggspatial)
library(sf)
library(caret)
library(ROCR)
library(haven)
library(readxl)
library(openxlsx)

getwd()
setwd("L:/research_assistants/Pepe/MLPaper")

#------------------------------------------------------------------------------------------------------------
#
#                   I. Insurance
#
#------------------------------------------------------------------------------------------------------------
# List of file paths
file_paths <- c(
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Benin_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Burkina.Faso_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Burundi_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Cameroon_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Chad_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Republic.of.the.Congo_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_DRC_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Cote.dIvoire_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Ethiopia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_The.Gambia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Ghana_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Guinea_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Kenya_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Lesotho_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Madagascar_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Malawi_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Mali_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Namibia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Niger_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Nigeria_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Senegal_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Sierra.Leone_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Tanzania_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Togo_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Uganda_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Zambia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Zimbabwe_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Eswatini_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Liberia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Rwanda_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_final_insurance_Mauritania_short.RData"
)

# Initialize an empty list to store data
df_list <- list()

# Load each file and append the data frame to the list
for (file in file_paths) {
  load(file) # Each file should contain an object named `df_final`
  df_list[[length(df_list) + 1]] <- df_final
}

# Combine all data frames into one
df_combined_insurance <- bind_rows(df_list)

# Save the combined data frame for future use
save(df_combined_insurance, file = "X:/research_assistants/Pepe/MLPaper/short_model/final_data/insurance/df_combined_insurance.RData")



# Create the summary table
summary_table <- df_combined_insurance %>%
  group_by(NAME_0) %>%
  summarise(
    number_of_observations = n(),
    mean_insurance = mean(insurance, na.rm = TRUE),
    mean_predicted_class = mean(predicted_class, na.rm = TRUE)
  )

# Print the summary table
print(summary_table)

# Export the summary table to Excel
write.xlsx(summary_table, file = "country_summary_statistics.xlsx")

summary(df_combined_insurance$insurance)

#------------------------------------------------------------------------------------------------------------
#
#                   II. visitHF12m
#
#------------------------------------------------------------------------------------------------------------
# List of file paths
file_paths <- c(
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Benin_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Burkina.Faso_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Burundi_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Cameroon_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Chad_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Republic.of.the.Congo_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_DRC_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Cote.dIvoire_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Ethiopia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_The.Gambia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Ghana_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Guinea_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Kenya_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Lesotho_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Madagascar_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Malawi_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Mali_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Namibia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Niger_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Nigeria_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Senegal_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Sierra.Leone_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Tanzania_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Togo_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Uganda_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Zambia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Zimbabwe_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Eswatini_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Liberia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Rwanda_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_final_visitHF12m_Mauritania_short.RData"
)

# Initialize an empty list to store data
df_list <- list()

# Load each file and append the data frame to the list
for (file in file_paths) {
  load(file) # Each file should contain an object named `df_final`
  df_list[[length(df_list) + 1]] <- df_final
}

# Combine all data frames into one
df_combined_visitHF12m <- bind_rows(df_list)

# Save the combined data frame for future use
save(df_combined_visitHF12m, file = "X:/research_assistants/Pepe/MLPaper/short_model/final_data/visitHF12m/df_combined_visitHF12m.RData")



# Create the summary table
summary_table <- df_combined_visitHF12m %>%
  group_by(NAME_0) %>%
  summarise(
    number_of_observations = n(),
    mean_visitHF12m = mean(visitHF12m, na.rm = TRUE),
    mean_predicted_class = mean(predicted_class, na.rm = TRUE)
  )

# Print the summary table
print(summary_table)

# Export the summary table to Excel
write.xlsx(summary_table, file = "country_summary_statistics.xlsx")

summary(df_combined_visitHF12m$visitHF12m)



#------------------------------------------------------------------------------------------------------------
#
#                   III. locationBirth
#
#------------------------------------------------------------------------------------------------------------
# List of file paths
file_paths <- c(
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Benin_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Burkina.Faso_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Burundi_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Cameroon_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Chad_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Republic.of.the.Congo_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_DRC_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Cote.dIvoire_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Ethiopia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_The.Gambia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Ghana_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Guinea_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Kenya_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Lesotho_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Madagascar_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Malawi_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Mali_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Namibia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Niger_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Nigeria_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Senegal_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Sierra.Leone_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Tanzania_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Togo_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Uganda_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Zambia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Zimbabwe_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Eswatini_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Liberia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Rwanda_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_final_locationBirth_Mauritania_short.RData"
)

# Initialize an empty list to store data
df_list <- list()

# Load each file and append the data frame to the list
for (file in file_paths) {
  load(file) # Each file should contain an object named `df_final`
  df_list[[length(df_list) + 1]] <- df_final
}

# Combine all data frames into one
df_combined_locationBirth <- bind_rows(df_list)

# Save the combined data frame for future use
save(df_combined_locationBirth, file = "X:/research_assistants/Pepe/MLPaper/short_model/final_data/locationBirth/df_combined_locationBirth.RData")



# Create the summary table
summary_table <- df_combined_locationBirth %>%
  group_by(NAME_0) %>%
  summarise(
    number_of_observations = n(),
    mean_locationBirth = mean(locationBirth, na.rm = TRUE),
    mean_predicted_class = mean(predicted_class, na.rm = TRUE)
  )

# Print the summary table
print(summary_table)

# Export the summary table to Excel
write.xlsx(summary_table, file = "country_summary_statistics.xlsx")

summary(df_combined_locationBirth$locationBirth)





#------------------------------------------------------------------------------------------------------------
#
#                   IV. postnatalcheck
#
#------------------------------------------------------------------------------------------------------------
# List of file paths
file_paths <- c(
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Benin_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Burkina.Faso_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Burundi_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Cameroon_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Chad_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Republic.of.the.Congo_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_DRC_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Cote.dIvoire_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Ethiopia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_The.Gambia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Ghana_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Guinea_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Kenya_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Lesotho_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Madagascar_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Malawi_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Mali_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Namibia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Niger_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Nigeria_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Senegal_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Sierra.Leone_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Tanzania_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Togo_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Uganda_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Zambia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Zimbabwe_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Eswatini_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Liberia_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Rwanda_short.RData",
  "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_final_postnatalcheck_Mauritania_short.RData"
)

# Initialize an empty list to store data
df_list <- list()

# Load each file and append the data frame to the list
for (file in file_paths) {
  load(file) # Each file should contain an object named `df_final`
  df_list[[length(df_list) + 1]] <- df_final
}

# Combine all data frames into one
df_combined_postnatalcheck <- bind_rows(df_list)

# Save the combined data frame for future use
save(df_combined_postnatalcheck, file = "X:/research_assistants/Pepe/MLPaper/short_model/final_data/postnatalcheck/df_combined_postnatalcheck.RData")



# Create the summary table
summary_table <- df_combined_postnatalcheck %>%
  group_by(NAME_0) %>%
  summarise(
    number_of_observations = n(),
    mean_postnatalcheck = mean(postnatalcheck, na.rm = TRUE),
    mean_predicted_class = mean(predicted_class, na.rm = TRUE)
  )

# Print the summary table
print(summary_table)

# Export the summary table to Excel
write.xlsx(summary_table, file = "country_summary_statistics.xlsx")

summary(df_combined_postnatalcheck$postnatalcheck)
