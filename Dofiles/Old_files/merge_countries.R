# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# VII. Merge countries and finalize ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Source scripts
source("L:/research_assistants/Pepe/MLPaper/Dofiles/run_countries.R")

# Specify the folder path containing .RData files
folder_path <- "L:/research_assistants/Pepe/MLPaper/New_data_sets"
# Get a list of .RData files in the folder
file_paths <- list.files(path = folder_path, pattern = "\\.RData$", full.names = TRUE)
# Create an empty list to store the data
data_list <- list()
# Load each .RData file into the list
for (i in seq_along(file_paths)) {
  # Load the file into a temporary environment to avoid overwriting
  temp_env <- new.env()
  load(file_paths[i], envir = temp_env)
  # Assuming each .RData file contains a single data frame
  obj_name <- ls(temp_env)[1]
  data_list[[paste0(obj_name, "_", i)]] <- temp_env[[obj_name]]
  # Append country name to each df
  data_list[[i]]$country <- countries[i]
}
# Merge the data frames
# Row-bind data frames (assuming they have the same columns)
merged_data <- bind_rows(data_list)

# For first analysis: Exclude all region and language dummies
merged_data <- merged_data %>% dplyr::select(-c(contains("region_")))
merged_data <- merged_data %>% dplyr::select(-c(contains("language_")))

# Drop all obs where insurance = NA
merged_data <- merged_data[!is.na(merged_data$insurance),]
# Remove columns with only NA values
merged_data <- merged_data %>%
  select_if(~ !all(is.na(.)))
# Drop variables if more than 10% missings
merged_data <- merged_data %>% select(where(~ mean(is.na(.)) <= 0.10))
# Drop variables with too little variation
merged_data_num <- merged_data %>%
  dplyr::select(where(~ !is.na(stats::sd(., na.rm = TRUE)) && stats::sd(., na.rm = TRUE) > 0.001))
merged_data <- bind_cols(merged_data_num, merged_data[["country"]])
colnames(merged_data)[ncol(merged_data)] <- "country"

# Put "insurance" as first column 
merged_data <- merged_data %>%
  select(all_of("insurance"), everything())
# Exclude all missing values
merged_data <- na.omit(merged_data)
# Read in overview
overview = read.xlsx("Variables_overview.xlsx", sheet = 2)
overview <- overview[-c(1:4),]
for (country in countries) {
  for (i in 1:nrow(overview)) {
    overview[i, which(colnames(overview) == country)] <- ifelse(all(is.na(merged_data[which(merged_data$country == country),which(colnames(merged_data) == overview$Name[i])])), 0, 1)
  }
}

# Last step: Make country dummies and drop character-type country
# generate dummies for categorical variables
merged_data <- dummy_cols(merged_data, select_columns = c("country"))
merged_data <- merged_data %>% dplyr::select(-c(country))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# VIII. Save final data set and overview Table ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$
# export data to .RData
save(merged_data, file=paste0("L:/research_assistants/Pepe/MLPaper/merged_data.RData"))
# Export overview to .xlsx
write.xlsx(overview, paste0("L:/research_assistants/Pepe/MLPaper/merged_overview.xlsx"))



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# IX. Analyze missing data ----
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Use gg_miss_var to visualize missing data per variable
# for (country in names(table(merged_data$NAME_0))) {
#   missing_plot <- gg_miss_var(merged_data[merged_data$NAME_0 == country,], show_pct = TRUE)
#   # Customize the plot with ggplot2
#   missing_plot <- missing_plot +
#     ggplot2::labs(title = "Missing Values by Country and Variable", 
#                   x = "Variables", 
#                   y = "Percentage of Missing Values") +
#     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
#     ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0, 0.1))) +
#     ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)), breaks = seq(0, 100, by = 10))
#   ggplot2::ggsave(paste0("missing_plot_",country,".png"), plot = missing_plot, width = 15, height = 11)
# }
# 
# df_insurance <- df[ , -68:-71]  # excludes Other primary outcomes since we won't use them in the model (Visited_HF ....)
# df_insurance <- df_insurance[ which(!is.na(df_insurance$insurance)),]   # excludes missing values for insurance
# df_insurance <- df_insurance[!names(df_insurance) %in% c("female", "education_NA", "hv001", "hv022", "hv021", "NAME_0", "region", "country", "LATNUM", "LONGNUM", "hv_wt", "year")] # exclude variables we do not need for the model
# df_insurance <- df_insurance[!names(df_insurance) %in% c("slept","water_interrupted","share_toilet","soap", "child_mosquitonet","female_spend_health_care","female_spend_earnings", "female_spend_health_car", "tested_HIV")] # exclude variables we do not have any variation after dropping missing values in next step or where we have too many missing values
# df_insurance <- na.omit(df_insurance) # exclude all missing values


