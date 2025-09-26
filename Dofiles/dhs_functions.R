#############################################################################################################
#  
#'  Description:  This file contains all relevant functions used to run the file run_countries.R
#'  Author:       Beat Scherrer             
#'  Date:         17.07.2024
#
#############################################################################################################

# 0. Packages ----
library(haven)
library(dplyr)
library(purrr)

# 1. Function for reading data ----
read_data <- function(input_path_IR, input_path_MR, input_path_PR) {
  # Read Individual Women's Data and prepare for merging
  data_IR <- read_dta(paste0("DataRaw/DHS/",input_path_IR))
  data_IR$merge001 <- data_IR$v001
  data_IR$merge002 <- data_IR$v002
  data_IR$merge003 <- data_IR$v003
  # Read Men's Data and prepare for merging
  data_MR <- read_dta(paste0("DataRaw/DHS/",input_path_MR))
  data_MR$merge001 <- data_MR$mv001
  data_MR$merge002 <- data_MR$mv002
  data_MR$merge003 <- data_MR$mv003
  # Read Household Listing Data and prepare for merging
  data_PR <- read_dta(paste0("DataRaw/DHS/",input_path_PR))
  data_PR$merge001 <- data_PR$hv001
  data_PR$merge002 <- data_PR$hv002
  data_PR$merge003 <- data_PR$hvidx
  # Merge the data sets
  data <- merge(data_PR, data_MR, by = c("merge001", "merge002", "merge003"), all=TRUE)
  data <- merge(data, data_IR, by = c("merge001", "merge002", "merge003"), all=TRUE)
  #Remove redundant data sets from workspace
  rm("data_IR", "data_MR", "data_PR")
  return(data)
}

# 2. Functions for creating variables ----
recode_binary <- function(data, old_name,
                          old_name_two = old_name,
                          old_name_three = old_name,
                          old_name_four = old_name,
                          old_name_five = old_name,
                          old_name_six = old_name,
                          new_name, true_values, na_values = c()) {
  na_positions <- is.na(data[[old_name]]) & 
    is.na(data[[old_name_two]]) &
    is.na(data[[old_name_three]]) &
    is.na(data[[old_name_four]]) &
    is.na(data[[old_name_four]]) &
    is.na(data[[old_name_six]])
  data[[new_name]] <- ifelse(data[[old_name]] %in% true_values |
                               data[[old_name_two]] %in% true_values | 
                               data[[old_name_three]] %in% true_values |
                               data[[old_name_four]] %in% true_values |
                               data[[old_name_five]] %in% true_values |
                               data[[old_name_six]] %in% true_values, 1,
                             ifelse(data[[old_name]] %in% na_values |
                                      data[[old_name_two]] %in% na_values | 
                                      data[[old_name_three]] %in% na_values |
                                      data[[old_name_four]] %in% na_values |
                                      data[[old_name_five]] %in% na_values |
                                      data[[old_name_six]] %in% na_values,NA,0))
  data[[new_name]][na_positions] <- NA
  return(data)
}

# Define function to create summary statistics
summarize_df <- function(df) {
  # Separate numeric, integer, and factor columns
  numeric_cols <- df %>% select_if(~ is.numeric(.) || is.integer(.))
  factor_cols <- df %>% select_if(is.factor)
  # One-hot encode factor columns
  if (ncol(factor_cols) > 0) {
    one_hot_encoded <- model.matrix(~ . - 1, data = factor_cols) %>% 
      as.data.frame() %>%
      setNames(gsub(" ", "_", names(.))) # Remove spaces in column names
    # Combine numeric and one-hot encoded factor columns
    numeric_cols <- bind_cols(numeric_cols, one_hot_encoded)
  }
  # Calculate statistics and round to two decimal places
  summary_df <- numeric_cols %>%
    map_dfr(~ list(
      n = sum(!is.na(.x)),
      mean = round(mean(.x, na.rm = TRUE), 2),
      min = round(min(.x, na.rm = TRUE), 2),
      max = round(max(.x, na.rm = TRUE), 2),
      sd = round(sd(.x, na.rm = TRUE), 2),
      na_share = round(mean(is.na(.x)), 2)
    ), .id = "variable")
  return(summary_df)
}

# Capitalize first letter of string
cap_first <- function(strings) {
  # A helper function to capitalize the first letter of a single string
  capitalize_one <- function(string) {
    if (nchar(string) == 0) return(string) # Handle empty string
    first_letter <- toupper(substr(string, 1, 1))
    rest <- substr(string, 2, nchar(string))
    paste0(first_letter, rest)
  }
  
  # Apply the helper function to each element in the character vector
  sapply(strings, capitalize_one, USE.NAMES = FALSE)
}

# 3. Define Function that creates the final data set for a given country ----
# Loop over each country to create the final data sets
run_countries <- function(country, country_code, IR_path, MR_path, PR_path, outcome_var, output_path) {
  # Read in data
  cli::cli_alert_info("Reading in data for {country} ...")
  data = read_data(IR_path, MR_path, PR_path)
  cli::cli_alert_info("Reading in data for {country} DONE")
  # Define empty df
  df <- data.frame(matrix(nrow = nrow(data),ncol = 0))
  # Define geo data path
  geo_path <- list.files(path = "Data/Geolocation/", pattern = paste0(".*",country_code,"GE.*_data.*"), full.names = TRUE)
  
  # 2.1. Create and add new variables, general recoding and special cases ----
  # Add hv001
  df$hv001 <- data$hv001
  
  #Sample Weight
  data$hv_wt = data$hv005 / 1000000
  df <- cbind(df,data["hv_wt"])
  
  #generate HHID
  data$HHID <- paste(data$merge001, data$merge002)
  df <- cbind(df,data["HHID"])
  
  #generate year
  data$year <- "2017_2018"
  df <- cbind(df,data["year"])
  
  # Generate a variable for age categories
  data$age <- data$hv105
  data$age_cat <- NA
  data$age_cat[data$age>=0 & data$age<= 14] <- "0_14"
  data$age_cat[data$age>=15 & data$age<= 24] <- "15_24"
  data$age_cat[data$age>=25 & data$age<= 29] <- "25_29"
  data$age_cat[data$age>=30 & data$age<= 34] <- "30_34"
  data$age_cat[data$age>=35 & data$age<= 39] <- "35_39"
  data$age_cat[data$age>=40 & data$age<= 44] <- "40_44"
  data$age_cat[data$age>=45 & data$age<= 49] <- "45_49"
  data$age_cat[data$age>=50 & data$age<= 54] <- "50_54"
  data$age_cat[data$age>=55 & data$age<= 59] <- "55_59"
  data$age_cat[data$age>=60 ] <- "59+"
  data$age_cat <- as.factor(data$age_cat)
  df <- cbind(df,data["age"],data["age_cat"])
  
  # Age categories for HH head
  data$age_HH_head <- data$hv220
  data$age_HH_head_cat <- NA
  data$age_HH_head_cat[data$age_HH_head>=0 & data$age_HH_head<= 14] <- "0_14"
  data$age_HH_head_cat[data$age_HH_head>=15 & data$age_HH_head<= 24] <- "15_24"
  data$age_HH_head_cat[data$age_HH_head>=25 & data$age_HH_head<= 29] <- "25_29"
  data$age_HH_head_cat[data$age_HH_head>=30 & data$age_HH_head<= 34] <- "30_34"
  data$age_HH_head_cat[data$age_HH_head>=35 & data$age_HH_head<= 39] <- "35_39"
  data$age_HH_head_cat[data$age_HH_head>=40 & data$age_HH_head<= 44] <- "40_44"
  data$age_HH_head_cat[data$age_HH_head>=45 & data$age_HH_head<= 49] <- "45_49"
  data$age_HH_head_cat[data$age_HH_head>=50 & data$age_HH_head<= 54] <- "50_54"
  data$age_HH_head_cat[data$age_HH_head>=55 & data$age_HH_head<= 59] <- "55_59"
  data$age_HH_head_cat[data$age_HH_head>=60 ] <- "59+"
  data$age_HH_head_cat <- as.factor(data$age_HH_head_cat)
  df <- cbind(df,data["age_HH_head"],data["age_HH_head_cat"])
  
  # 2.2. Recode to binaries ----
  cli::cli_alert_info("Recoding binaries...")
  for (recode_info in recode_list) {
    args_list <- append(list(data = data),recode_info)
    if (!(recode_info$old_name %in% colnames(data)))
    {
      warning(paste0("The variable ",recode_info$old_name," is not in the data set! Skipping..."))
      next
    }
    data <- do.call(recode_binary,args_list)
    # Append variable to final data frame
    df <- cbind(df,data[recode_info$new_name])
  }
  cli::cli_alert_info("Recoding binaries DONE")
  
  # 2.3. Standardize numeric variables ----
  cli::cli_alert_info("Standardizing numerical variables...")
  for (entry in list_scale) {
    if (!(entry$old_name %in% colnames(data)))
    {
      warning(paste0("The variable",entry$old_name,"is not in the data set! Skipping..."))
      next
    }
    if (96 %in% data[[entry$old_name]]) {
      data[[entry$old_name]][data[[entry$old_name]] == 96] <- NA
    }
    if (99 %in% data[[entry$old_name]]) {
      data[[entry$old_name]][data[[entry$old_name]] == 99] <- NA
    }
    if (998 %in% data[[entry$old_name]]) {
      data[[entry$old_name]][data[[entry$old_name]] == 998] <- NA
    }
    if (997 %in% data[[entry$old_name]]) {
      data[[entry$old_name]][data[[entry$old_name]] == 997] <- NA
    }
    if (996 %in% data[[entry$old_name]]) {
      data[[entry$old_name]][data[[entry$old_name]] == 996] <- NA
    }
    if (999 %in% data[[entry$old_name]]) {
      data[[entry$old_name]][data[[entry$old_name]] == 999] <- NA
    }
    data[entry$new_name] <- as.data.frame(scale(data[entry$old_name]))
    df <- cbind(df,data[entry$new_name])
  }
  cli::cli_alert_info("Standardizing numerical variables DONE")
  
  # 2.4. Factorize categorical variables ----
  cli::cli_alert_info("factorizing categorical variables...")
  # Region
  if ("hv024" %in% colnames(data)) {
    data$region <- factor(data$hv024,
                          levels = attr(data$hv024, "labels"),
                          labels = cap_first(names(attr(data$hv024, "labels"))))
    df <- cbind(df,data["region"])
  } else {
    warning("The variable hv024 is not in the data set! Skipping...")
  }
  
  # Wealth index
  if ("hv270" %in% colnames(data)) {
    data$wealthindex <- data$hv270
    data$wealthindex <- factor(data$wealthindex,
                               levels = c(1,2,3,4,5),
                               labels = c("1_Quintile" ,
                                          "2_Quintile",
                                          "3_Quintile",
                                          "4_Quintile",
                                          "5_Quintile"))
    df <- cbind(df,data["wealthindex"])
  } else {
    warning("The variable hv270 is not in the data set! Skipping...")
  }
  
  # Native language
  if ("hv045c" %in% colnames(data)) {
    data$language <- factor(data$hv045c,
                            levels = attr(data$hv045c, "labels"),
                            labels = cap_first(names(attr(data$hv045c, "labels"))))
    df <- cbind(df,data["language"])
  } else {
    warning("The variable hv045c is not in the data set! Skipping...")
  }
  
  # religion
  if ("v130" %in% colnames(data)) {
    data$v131[data$v131 == 96] <- NA
    data$religion <- factor(data$v130,
                            levels = attr(data$v130, "labels"),
                            labels = cap_first(names(attr(data$v130, "labels"))))
    df <- cbind(df,data["religion"])
  } else {
    warning("The variable v130 is not in the data set! Skipping...")
  }
  
  # ethnicity
  if ("v131" %in% colnames(data)) {
    data$v131[data$v131 == 96] <- NA
    data$ethnicity <- factor(data$v131,
                             levels = attr(data$v131, "labels"),
                             labels = cap_first(names(attr(data$v131, "labels"))))
    df <- cbind(df,data["ethnicity"])
  } else {
    warning("The variable v131 is not in the data set! Skipping...")
  }
  
  cli::cli_alert_info("factorizing categorical variables DONE")
  
  
  data <- recode_binary(data = data, old_name = "hv115",old_name_two = "v501", old_name_three = "mv501", new_name = "married", true_values = c(1,2))
  table(data$married_HH, useNA = "always")
  
  # 2.4. Aggregate variables on HH level ----
  cli::cli_alert_info("Aggregating variables on household level ...")
  # child involved in child_labor within HH
  if ("child_labor" %in% colnames(data)) {
    data$child_labor_HH <- ave(data$child_labor, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
    data$child_labor_HH[data$child_labor_HH >=1] <- 1
    df <- cbind(df,data["child_labor_HH"])
  } else {
    warning("The variable child_labor is not in the data set! Skipping...")
  }
  
  # child involved in child_labor: carrying heavy loads in HH
  if ("child_labor_carry" %in% colnames(data)) {
    data$child_labor_carry_HH <- ave(data$child_labor_carry, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
    data$child_labor_carry_HH[data$child_labor_carry_HH >=1] <- 1
    df <- cbind(df,data["child_labor_carry_HH"])
  } else {
    warning("The variable sh165 is not in the data set! Skipping...")
  }
  
  # child involved in child labor: unsafe work in HH
  if ("child_labor_unsafe" %in% colnames(data)) {
    data$child_labor_unsafe_HH <- ave(data$child_labor_unsafe, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
    data$child_labor_unsafe_HH[data$child_labor_unsafe_HH >=1] <- 1
    df <- cbind(df,data["child_labor_unsafe_HH"])
  } else {
    warning("The variable sh166 is not in the data set! Skipping...")
  }
  
  # child involved in child labor: number of hours worked in HH
  if ("sh164" %in% colnames(data)) {
    data$child_labor_hours_HH <- scale(ave(data$sh164, data$HHID, FUN= function(x) sum(x, na.rm = TRUE)))
    data$child_labor_hours_HH[data$child_labor_hours_HH >=1] <- 1
    #df <- cbind(df,data["child_labor_hours_HH"])
  } else {
    warning("The variable sh166 is not in the data set! Skipping...")
  }
  
  # # married aggregated on HH level
  # if ("married" %in% colnames(data)) {
  #   data$married_HH <- ave(data$married, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
  #   data$married_HH[data$married_HH!= 0 & data$married_HH!=1] <- NA
  #   df <- cbind(df,data["married_HH"])
  # } else {
  #   warning("The variable married is not in the data set! Skipping...")
  # }
  
  # Generate variable for education levels
  if ("hv106" %in% colnames(data)) {
    data$education <- NA 
    data$education[data$hv106 == 0] <- 0
    data$education[data$hv106 == 1] <- 1
    data$education[data$hv106 == 2] <- 2
    data$education[data$hv106 == 3] <- 3	
    data$education_HH <- ave(data$education, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
    data$education_HH[data$education_HH!= 0 & data$education_HH!=1 & data$education_HH!=2 & data$education_HH!=3] <- NA
    # Factorize and add labels
    data$education <- factor(data$education,
                             levels = c(0,1,2,3),
                             labels = c("no_edu", "primary_edu", "secondary_edu", "tertiary_edu")) 
    data$education_HH <- factor(data$education_HH,
                                levels = c(0,1,2,3),
                                labels = c("no_edu", "primary_edu", "secondary_edu", "tertiary_edu")) 
    df <- cbind(df, data["education"], data["education_HH"])
  } else {
    warning("The variable hv106 (education) is not in the data set! Skipping...")
  }
  
  ### Variables from Women's survey ###
  # # media aggregated on HH level
  # if ("media" %in% colnames(data)) {
  #   data$media_HH <- ave(data$media, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
  #   data$media_HH[data$media_HH!=0 & data$media_HH!=1] <- NA
  #   df <- cbind(df,data["media_HH"])
  # } else {
  #   warning("The variable media is not in the data set! Skipping...")
  # }
  
  # # fertility aggregated on HH level
  # if ("fertility" %in% colnames(data)) {
  #   data$fertility_HH <- ave(data$fertility, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
  #   data$fertility_HH[data$fertility_HH!=0 & data$fertility_HH!=1] <- NA
  #   df <- cbind(df,data["fertility_HH"])
  # } else {
  #   warning("The variable fertility is not in the data set! Skipping...")
  # }
  cli::cli_alert_info("Aggregating variables on household level DONE")
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # III. Drop variables, observations, and create final insurance data set  ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # 3.1. Create final insurance data set  ----
  df_insurance <- df
  
  # 3.2. Drop observations ----
  cli::cli_alert_info("Dropping obs...")
  # Drop observations aged below 15 and above 59
  df_insurance <- df_insurance[ which(df_insurance$age>=15 & df_insurance$age<=49),]
  # Only look at females
  df_insurance <- df_insurance[ which(df_insurance$female==1),]
  # Drop all obs where insurance = NA
  df_insurance <- df_insurance %>% dplyr::filter(!is.na(get(outcome_var)))
  cli::cli_alert_info("Dropping obs DONE")
  
  # 3.3. Drop variables ----
  cli::cli_alert_info("Dropping vars...")
  # Remove columns with only NA values
  df_insurance <- df_insurance %>%
    select_if(~ !all(is.na(.)))
  # Drop useless variables for prediction
  df_insurance <- df_insurance %>%
    dplyr::select(-c(hv_wt, age, age_HH_head, year, HHID))
  # Drop variables with more than 10% missing values
  df_insurance <- df_insurance %>%
    dplyr::select(where(~ sum(!is.na(.)) > 0 && mean(is.na(.)) <= 0.1))
  # Drop variables with too little variation
  df_insurance <- df_insurance %>%
    dplyr::select(
      # Dynamically deselect variables only if they exist
      -c("hv001", "region"),
      where(
        ~ (is.numeric(.) && !is.na(stats::sd(., na.rm = TRUE)) && stats::sd(., na.rm = TRUE) > 0.001) ||
          (is.factor(.) && length(unique(na.omit(.))) > 1)
      )
    )
  cli::cli_alert_info("Dropping vars DONE")
  
  # Put primary outcome as first column 
  df_insurance <- df_insurance %>%
    select(all_of(outcome_var), everything())
  
  # 3.4. Exclude all missing values ----
  df_insurance <- na.omit(df_insurance)
  
  # generate dummies for categorical variables (this has to follow the overview table such that the categoricals are included in there)
  cli::cli_alert_info("dummify categoricals...")
  df_insurance <- dummy_cols(df_insurance, select_columns = c("age_cat", "age_HH_head_cat", "education","education_HH", "wealthindex", "language", "religion", "ethnicity"))
  myvars <- names(df_insurance) %in% c("age_cat", "age_HH_head_cat", "education","education_HH", "wealthindex", "language", "religion", "ethnicity")
  df_insurance <- df_insurance[!myvars]
  cli::cli_alert_info("dummify categoricals DONE")
  
  # Adapt column names for later model training
  names(df_insurance) <- gsub(" ", "_", names(df_insurance))
  names(df_insurance) <- gsub("-", "_", names(df_insurance))
  names(df_insurance) <- gsub(",", "", names(df_insurance))
  names(df_insurance) <- gsub("&", "", names(df_insurance))
  names(df_insurance) <- gsub("/", "_", names(df_insurance))
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # V. Add Geolocation ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # if (purrr::is_empty(geo_path)) {
  #   cli::cli_alert_warning("There is no geo data available for the country code {country_code}.")
  # }
  # else {
  #   cli::cli_alert_info("Merging DHS data with geo data ...")
  #   df_geolocation <- read_dta(geo_path)
  #   df_geolocation$hv001 <- df_geolocation$DHSCLUST
  #   df_geolocation <- df_geolocation [,c("hv001", "LATNUM", "LONGNUM")]
  #   
  #   #merge with geolocation
  #   df_insurance <- merge(df_insurance, df_geolocation, by = c("hv001"), all=TRUE)
  # }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # VI. Save countries ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  cli::cli_alert_info("Saving Data ...")
  
  # export data to .RData
  save(df_insurance, file=paste0(output_path,country,".RData"))
  
  # Call function to create summary statistics and export to xlsx
  df_summary <- summarize_df(df_insurance)
  write.xlsx(df_summary, paste0(output_path,country,"_summary_stats.xlsx"))
  cli::cli_alert_info("Saving Data DONE")
  
}

# 4. Function for running the models -----
run_models <- function(country, model_list, outcome_var, short_model = FALSE) {
  # Load data
  if (country == "all") {
    df_insurance <- get(load(file.path(getwd(), "short_model", "final_data", outcome_var, paste0("df_final_",outcome_var,"_",country,"_short.RData"))))  # Replace with the exact object name if known
  } else {
    df_insurance <- get(load(file.path(getwd(), "New_data_sets", outcome_var, paste0(country,".RData"))))  # Replace with the exact object name if known
  }
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  #                   I. Create model for insurance coverage ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  cli::cli_alert_info("Prepare Data for Train and Test for country: {country}.")
  # Reduce data to subset of key variables for estimation of short model
  if (short_model == TRUE) {
    df_insurance <- df_insurance %>%
      dplyr::select(all_of(intersect(c(outcome_var, "married", "age_cat_15_24", 
                                       "age_cat_25_29", "age_cat_30_34", "age_cat_35_39", 
                                       "age_cat_40_44", "age_cat_45_49", "education_no_edu", 
                                       "education_primary_edu", "education_secondary_edu", 
                                       "education_tertiary_edu", "currently_working", 
                                       "rural", "sondaughterhome", "electricity", "toilet", 
                                       "region", "hv001", "insurance"), 
                                     colnames(df_insurance))))
    # create df for prediction
    if ("hv001" %in% colnames(df_insurance)) {
      df_prediction <- df_insurance %>%
        dplyr::select(-c(hv001))
    } else {
      df_prediction <- df_insurance
    }
  } else {
    df_prediction <- df_insurance
    ## I.I. Drop certain variables from model estimation ----
    
    # Quick fix: if too many vars, drop ethnicities
    if (ncol(df_prediction) > 300) {
      df_prediction <- df_prediction %>% dplyr::select(-c(contains("ethnicity")))
    }
    
    # Drop other primary outcomes
    variables_to_drop <- intersect(setdiff(outcome_vars_without_insurance, outcome_var), names(df_prediction))
    df_prediction <- df_prediction %>% dplyr::select(-all_of(variables_to_drop))
    
    # Drop csection
    if ("csection" %in% colnames(df_prediction)) {
      df_prediction <- df_prediction %>% dplyr::select(-c(csection)
      )}
    
    # Drop hv001
    df_prediction <- df_prediction %>% dplyr::select(-c(hv001)
    )
  }
  
  if (country != "all") {
    # Dummify region
    df_prediction <- dummy_cols(df_prediction, select_columns = c("region"))
    myvars <- names(df_prediction) %in% c("region")
    df_prediction <- df_prediction[!myvars]    
  }
  
  # quick fix: omit all NAs for pooled df
  if (country == "all") {
    df_prediction <- na.omit(df_prediction)
  }
  
  # Rename invalid column names
  names(df_prediction) <- gsub("ã", "a", names(df_prediction))
  names(df_prediction) <- gsub("\\(|\\)", "", names(df_prediction))
  names(df_prediction) <- gsub("'", "", names(df_prediction))
  names(df_prediction) <- gsub("©", "", names(df_prediction))
  names(df_prediction) <- gsub("?", "", names(df_prediction))
  names(df_prediction) <- gsub("\\+", "", names(df_prediction))
  names(df_prediction) <- gsub("é", "e", names(df_prediction))
  
  # Adapt column names for later model training
  names(df_prediction) <- gsub(" ", "_", names(df_prediction))
  names(df_prediction) <- gsub("-", "_", names(df_prediction))
  names(df_prediction) <- gsub(",", "", names(df_prediction))
  names(df_prediction) <- gsub("&", "", names(df_prediction))
  names(df_prediction) <- gsub("/", "_", names(df_prediction))
  
  #splitting
  set.seed(1)   # set the seed to make it reproducible
  smp_size_insurance <- floor(0.66 * nrow(df_prediction)) # use 1/3 and 2/3 split
  train_ind_insurance <- sample(seq_len(nrow(df_prediction)), size = smp_size_insurance)
  test_insurance <- df_prediction[-train_ind_insurance,]
  train_insurance <<- df_prediction[train_ind_insurance,]
  
  # Dynamically create the formula based on the input variable
  formula_str1 <- sprintf("%s ~ .", outcome_var)
  formula_str2 <- sprintf("as.factor(%s) ~ .", outcome_var)
  formula <- as.formula(formula_str1)
  formula_model <- as.formula(formula_str2)
  
  # Oversampling. We perform oversampling to a little over 20% iff dataset is unbalanced to either side (threshold is 20%)
  table(train_insurance[outcome_var], useNA = "always")
  share_ones <- nrow(train_insurance[train_insurance[outcome_var] == 1,])/nrow(train_insurance)
  if (share_ones < 0.2 | share_ones > 0.8) {
    train_insurance <<- ROSE::ovun.sample(formula = formula, data = train_insurance, method = "over", p = 0.2, seed = 1)$data
  }
  table(train_insurance[outcome_var], useNA = "always")
  
  # Prepare data for LASSO model
  shrinkage_test_insurance <<- test_insurance[ ,-1] # Removes outcome_var
  x_test <<- model.matrix(~ ., data=shrinkage_test_insurance)
  shrinkage_train_insurance <<- train_insurance[ ,-1] # Removes outcome_var
  x_train <<- model.matrix(~ ., data=shrinkage_train_insurance)
  train_dummy_x._insurance <<- data.matrix(x_train)
  train_dummy_y._insurance <<- as.double(as.matrix(train_insurance[, 1])) # Only outcome_var
  test_dummy_x._insurance <<- data.matrix(x_test) # Removes outcome_var
  
  # Define repeated CV
  train_control <- trainControl(method="repeatedcv", number=5, repeats=3, savePredictions="final")
  cli::cli_alert_info("Prepare Data for Train and Test for country: {country}. DONE")
  
  # Create df to store variable importances
  importance_df <- as.data.frame(matrix(nrow = ncol(train_insurance)))
  importance_df[,1] <- colnames(train_insurance)
  colnames(importance_df) <- "variables"
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # II. General Logit Model ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if ("basic_logit" %in% model_list) {
    
    model_name <- "basic_logit"
    cli::cli_alert_info("Train Model: {model_name}")
    
    # Start time
    start_time <- Sys.time()
    
    # Train Logit model
    logit_general_insurance <- glm(formula = formula_model, data=df_prediction, family="binomial")
    cli::cli_alert_info("Test")
    # End time
    end_time <- Sys.time()
    
    # Calculate elapsed time
    elapsed_time <- difftime(end_time, start_time, units = "secs")
    
    # Produce performance metrics for later plotting
    predict_general_insurance <- predict(logit_general_insurance, df_prediction)
    Perf_general_insurance <- ROCR::prediction(as.numeric(predict_general_insurance), as.numeric(df_prediction[[outcome_var]]))
    AUC_general_insurance <- performance(Perf_general_insurance , "auc") 
    ROC_general_insurance <- performance(Perf_general_insurance, "tpr", "fpr")
    RP_general_insurance <- performance(Perf_general_insurance, "prec","rec")
    
    # Create confusion matrix
    pred_dummy <-  ifelse(predict_general_insurance> 0.5 , 1,0) 
    cm_general_insurance <- confusionMatrix(as.factor(pred_dummy),as.factor(df_prediction[[outcome_var]]), positive = "1") 
    
    # Create df with most important performance metrics
    metrics_general1 <- as.data.frame(cm_general_insurance$overall)
    metrics_general2 <- as.data.frame(cm_general_insurance$byClass)
    metrics_general1 <- rename(metrics_general1 , c("logit_general_insurance" = "cm_general_insurance$overall"))
    metrics_general2 <- rename(metrics_general2 , c("logit_general_insurance" = "cm_general_insurance$byClass"))
    metrics_general <- as.data.frame(rbind(metrics_general1, metrics_general2))
    
    # Append metrics to final metrics file
    if (!is.null(nrow(metrics_df))) {
      metrics_df <- cbind(metrics_df, metrics_general)
    } else{
      metrics_df <- metrics_general
    }
    
    # Create variable importance score vector
    importance <- as.data.frame(varImp(logit_general_insurance))
    negative <- as.data.frame(logit_general_insurance$coefficients[-1])
    negative <- na.omit(negative)
    for (row in 1:nrow(negative)) {
      importance[row,2] <- ifelse(negative[row,1] < 0, 1, 0)
    }
    names(importance) <- c("importance_general", "Negative_general")
    importance$variables <- rownames(importance)
    # Normalize it to 100
    importance$importance_general <- (importance$importance_general / max(importance$importance_general)) * 100
    # Sort
    importance_general <- importance %>% arrange(desc(importance))
    
    # Append to df
    importance_df <- importance_df %>% dplyr::left_join(as.data.frame(importance_general), by = "variables")
    
    cli::cli_alert_info("Train Model: {model_name} DONE. Time taken: {elapsed_time} seconds")
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # III. Crossvalidated Logit Model ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if ("logit" %in% model_list) {
    
    model_name <- "cv_logit"
    cli::cli_alert_info("Train Model: {model_name} ...")
    
    set.seed(1)
    # Start time
    start_time <- Sys.time()
    logit_insurance_cv  <- train(as.factor(insurance)~.,
                                 data=train_insurance, trControl=train_control, method="glm", family="binomial")
    # End time
    end_time <- Sys.time()
    
    # Calculate elapsed time
    elapsed_time <- difftime(end_time, start_time, units = "mins")
    
    # Produce performance metrics for later plotting
    predict_logit_insurance_cv <- predict(logit_insurance_cv, test_insurance, type="prob")
    Perf_logit_insurance_cv <- prediction(as.numeric(predict_logit_insurance_cv[[2]]), as.numeric(test_insurance$insurance))
    AUC_logit_insurance_cv  <- performance(Perf_logit_insurance_cv , "auc") 
    ROC_logit_insurance_cv  <- performance(Perf_logit_insurance_cv , "tpr", "fpr")
    RP_logit_insurance_cv  <- performance(Perf_logit_insurance_cv , "prec","rec")
    
    # Create confusion matrix
    pred_dummy <-  ifelse(predict_logit_insurance_cv[[2]]> 0.5 , 1,0) 
    cm_logit_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance$insurance), positive = "1")
    
    # Create df with most important performance metrics
    metrics_logit1 <- as.data.frame(cm_logit_insurance_cv$overall)
    metrics_logit2 <- as.data.frame(cm_logit_insurance_cv$byClass)
    metrics_logit1 <- rename(metrics_logit1 , c("logit_cv_insurance" = "cm_logit_insurance_cv$overall"))
    metrics_logit2 <- rename(metrics_logit2 , c("logit_cv_insurance" = "cm_logit_insurance_cv$byClass"))
    metrics_logit <<- as.data.frame(rbind(metrics_logit1, metrics_logit2))
    
    # Append metrics to final metrics file
    if (!is.null(nrow(metrics_df))) {
      metrics_df <- cbind(metrics_df, metrics_logit)
    } else{
      metrics_df <- metrics_logit
    }
    
    # List the variable importance in descending order
    importance <- as.data.frame(varImp(logit_insurance_cv)$importance)
    importance_logit <- importance %>% arrange(desc(importance))
    names(importance_logit) <- "importance_logit"
    importance_logit$variables <- rownames(importance_logit)
    importance_df <- importance_df %>% dplyr::left_join(as.data.frame(importance_logit), by = "variables")
    
    cli::cli_alert_info("Train Model: {model_name} DONE. Time taken: {elapsed_time} minutes")
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # IV. Lasso ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if ("lasso" %in% model_list) {
    model_name <- "lasso"
    cli::cli_alert_info("Training Model: {model_name} ...")
    
    # Track time
    start_time <- Sys.time()
    
    # Set seed for reproduceability
    set.seed(1)
    
    # Use 5-fold CV procedure to determine optimal lambda for regularization 
    lambda_dummy_TZ <- cv.glmnet(train_dummy_x._insurance, train_dummy_y._insurance, family='binomial', alpha=1,  standardize=TRUE, nfolds=5, type.measure = "class")
    lambda_dummy_TZ$lambda.1se
    lambda_dummy_TZ$lambda.min
    
    # train the logit model with regularization, use lambda.1se to get even sparser model
    lasso_insurance_cv  <- train(form = formula_model, 
                                 data=train_insurance, trControl=train_control,  method="glmnet", 
                                 tuneGrid= expand.grid(.alpha=1, .lambda= lambda_dummy_TZ$lambda.1se))
    
    # End time
    end_time <- Sys.time()
    # Calculate elapsed time
    elapsed_time <- difftime(end_time, start_time, units = "mins")
    
    # Produce performance metrics for later plotting
    predict_lasso_insurance_cv <- predict(lasso_insurance_cv, test_insurance,type="prob")
    Perf_lasso_insurance_cv <- prediction(as.numeric(predict_lasso_insurance_cv[[2]]), as.numeric(test_insurance[[outcome_var]]))
    AUC_lasso_insurance_cv  <- performance(Perf_lasso_insurance_cv , "auc") 
    ROC_lasso_insurance_cv  <- performance(Perf_lasso_insurance_cv , "tpr", "fpr")
    RP_lasso_insurance_cv  <- performance(Perf_lasso_insurance_cv , "prec","rec")
    
    # Create confusion matrix
    pred_dummy <-  ifelse(predict_lasso_insurance_cv[[2]]> 0.5 , 1,0) 
    cm_lasso_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance[[outcome_var]]), positive = "1") 
    
    # Create df with most important performance metrics
    metrics_lasso1 <- as.data.frame(cm_lasso_insurance_cv$overall)
    metrics_lasso2 <- as.data.frame(cm_lasso_insurance_cv$byClass)
    metrics_lasso1 <- rename(metrics_lasso1 , c("lasso_cv_insurance" = "cm_lasso_insurance_cv$overall"))
    metrics_lasso2 <- rename(metrics_lasso2 , c("lasso_cv_insurance" = "cm_lasso_insurance_cv$byClass"))
    metrics_lasso <- as.data.frame(rbind(metrics_lasso1, metrics_lasso2))
    
    # Append metrics to final metrics file
    if (!is.null(nrow(metrics_df))) {
      metrics_df <- cbind(metrics_df, metrics_lasso)
    } else{
      metrics_df <- metrics_lasso
    }
    
    # List the variable importance in descending order
    importance <- as.data.frame(varImp(lasso_insurance_cv)$importance)
    importance_lasso <- importance %>% arrange(desc(importance))
    names(importance_lasso) <- "importance_lasso"
    importance_lasso$variables <- rownames(importance_lasso)
    importance_df <- importance_df %>% dplyr::left_join(as.data.frame(importance_lasso), by = "variables")
    
    # Extract coefficients from the caret trained model
    lasso_best_lambda <- lasso_insurance_cv$bestTune$.lambda
    final_model <- lasso_insurance_cv$finalModel
    coef_final_model <- coef(final_model, s = lasso_best_lambda)
    
    cli::cli_alert_info("Training Model: {model_name} DONE. Time taken: {elapsed_time} minutes.")
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # V. Random Forest ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if ("random_forest" %in% model_list) {
    model_name <- "random_forest"
    cli::cli_alert_info("Tuning Model: {model_name} ...")
    
    # Track time
    start_time <- Sys.time()
    
    # Define grid for number of trees (ntrees) within forest and minimum number of obs in terminal nodes (nodesize)
    mtry_grid <- expand.grid(mtry = c(5, 10))
    nodesize_grid <- c(1,3)
    ntree_grid <- c(100,300)
    
    # Initialize a dataframe to store results
    results <- data.frame(mtry = integer(), ntree = integer(), nodesize = integer(), 
                          Accuracy = double())
    
    for (ntree in ntree_grid) {
      for (nodesize in nodesize_grid) {
        # Run the RF model for each hyperparameter combination
        rf_tune <- train(form = formula_model,
                         data = train_insurance,
                         method = "rf",
                         tuneGrid = mtry_grid,
                         ntree = ntree,
                         nodesize = nodesize,
                         trControl = train_control)
        
        # Extract the best mtry
        best_mtry <- rf_tune$bestTune$mtry
        
        # Extract the best accuracy
        best_accuracy <- max(rf_tune$results$Accuracy)
        
        # Store the results
        results <- rbind(results, data.frame(mtry = best_mtry, ntree = ntree, 
                                             nodesize = nodesize, Accuracy = best_accuracy))
      }
    }
    
    # End time
    end_time <- Sys.time()
    # Calculate elapsed time
    elapsed_time_tuning <- difftime(end_time, start_time, units = "mins")
    
    # Identify the best combination of parameters
    bestParams <- results[which.max(results$Accuracy), ]
    
    cli::cli_alert_info("Tuning Model: {model_name} DONE. Time taken: {elapsed_time_tuning} minutes.")
    
    cli::cli_alert_info("Training Model: {model_name} ...")
    
    # Track time
    start_time <- Sys.time()
    
    # Train the final model with the best hyperparameters
    rf_insurance_cv <- train(form = formula_model,
                             data = train_insurance,
                             tuneGrid = expand.grid(mtry = bestParams$mtry), 
                             ntree = bestParams$ntree,
                             nodesize = bestParams$nodesize,
                             trControl = train_control,
                             method = "rf")
    
    # Produce performance metrics for later plotting
    predict_rf_insurance_cv <- predict(rf_insurance_cv, test_insurance, type="prob")
    Perf_rf_insurance_cv <- prediction(as.numeric(predict_rf_insurance_cv[[2]]), as.numeric(test_insurance[[outcome_var]]))
    AUC_rf_insurance_cv  <- performance(Perf_rf_insurance_cv , "auc") 
    ROC_rf_insurance_cv  <- performance(Perf_rf_insurance_cv , "tpr", "fpr")
    RP_rf_insurance_cv  <- performance(Perf_rf_insurance_cv , "prec","rec")
    
    # Create confusion matrix
    pred_dummy <-  ifelse(predict_rf_insurance_cv[[2]]> 0.5 , 1,0) 
    cm_rf_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance[[outcome_var]]), positive = "1") 
    
    # Create df with most important performance metrics
    metrics_rf1 <- as.data.frame(cm_rf_insurance_cv$overall)
    metrics_rf2 <- as.data.frame(cm_rf_insurance_cv$byClass)
    metrics_rf1 <- rename(metrics_rf1 , c("rf_cv_insurance" = "cm_rf_insurance_cv$overall"))
    metrics_rf2 <- rename(metrics_rf2 , c("rf_cv_insurance" = "cm_rf_insurance_cv$byClass"))
    metrics_random_forest <- as.data.frame(rbind(metrics_rf1, metrics_rf2))
    
    # Append metrics to final metrics file
    if (!is.null(nrow(metrics_df))) {
      metrics_df <- cbind(metrics_df, metrics_random_forest)
    } else{
      metrics_df <- metrics_random_forest
    }
    
    # List the variable importance in descending order
    importance <- as.data.frame(varImp(rf_insurance_cv)$importance)
    importance_random_forest <- importance %>% arrange(desc(importance))
    names(importance_random_forest) <- "importance_rf"
    importance_random_forest$variables <- rownames(importance_random_forest)
    importance_df <- importance_df %>% dplyr::left_join(as.data.frame(importance_random_forest), by = "variables")
    
    # End time
    end_time <- Sys.time()
    # Calculate elapsed time
    elapsed_time_training <- difftime(end_time, start_time, units = "mins")
    
    cli::cli_alert_info("Training Model: {model_name} DONE. Time taken: {elapsed_time_training} minutes.")
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # VI. Neural Network ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if ("neural_network" %in% model_list) {
    model_name <- "neural_network"
    cli::cli_alert_info("Training Model: {model_name} ...")
    
    # Track time
    start_time <- Sys.time()
    
    set.seed(1)
    nnetGrid <-  expand.grid(size = seq(from = 1, to = 3, by = 1), decay = seq(from = 0.3, to = 0.5, by = 0.1)) #best tune is size 1 and decay = 0.5
    #nnet_insurance_cv <-train(as.factor(insurance)~.,
    #                         data=train_insurance, method='nnet', trControl = train_control, tuneGrid=expand.grid(size=5, decay=0.1)) ##nnet or neuralnet
    nnet_insurance_cv <-train(form = formula_model,
                              data=train_insurance, method='nnet',  ##nnet or neuralnet
                              trControl = train_control, tuneGrid=nnetGrid,
                              verbosity = 0,
                              verbose = FALSE)
    
    # End time
    end_time <- Sys.time()
    # Calculate elapsed time
    elapsed_time <- difftime(end_time, start_time, units = "mins")
    
    # Produce performance metrics for later plotting
    predict_nnet_insurance_cv <- predict(nnet_insurance_cv,test_insurance, type="prob") 
    Perf_nnet_insurance_cv <- prediction(as.numeric(predict_nnet_insurance_cv[[2]]), as.numeric(test_insurance[[outcome_var]]))
    AUC_nnet_insurance_cv  <- performance(Perf_nnet_insurance_cv, "auc") 
    ROC_nnet_insurance_cv  <- performance(Perf_nnet_insurance_cv, "tpr", "fpr")
    RP_nnet_insurance_cv  <- performance(Perf_nnet_insurance_cv, "prec","rec")
    
    # Create confusion matrix
    pred_dummy <-  ifelse(predict_nnet_insurance_cv[[2]]> 0.5 , 1,0)
    cm_nnet_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance[[outcome_var]]), positive = "1") 
    
    # Create df with most important performance metrics
    metrics_nnet1 <- as.data.frame(cm_nnet_insurance_cv$overall)
    metrics_nnet2 <- as.data.frame(cm_nnet_insurance_cv$byClass)
    metrics_nnet1 <- rename(metrics_nnet1 , c("nnet_cv_insurance" = "cm_nnet_insurance_cv$overall"))
    metrics_nnet2 <- rename(metrics_nnet2 , c("nnet_cv_insurance" = "cm_nnet_insurance_cv$byClass"))
    metrics_neural_net <- as.data.frame(rbind(metrics_nnet1, metrics_nnet2))
    
    # Append metrics to final metrics file
    if (!is.null(nrow(metrics_df))) {
      metrics_df <- cbind(metrics_df, metrics_neural_net)
    } else{
      metrics_df <- metrics_neural_net
    }
    
    # List the variable importance in descending order
    importance <- as.data.frame(varImp(nnet_insurance_cv)$importance)
    importance_neural_net <- importance %>% arrange(desc(importance))
    names(importance_neural_net) <- "importance_nnet"
    importance_neural_net$variables <- rownames(importance_neural_net)
    importance_df <- importance_df %>% dplyr::left_join(as.data.frame(importance_neural_net), by = "variables")
    
    cli::cli_alert_info("Training Model: {model_name} DONE. Time taken: {elapsed_time} minutes.")
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # VII. XGBoost ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if ("xgboost" %in% model_list) {
    model_name <- "xgboost"
    cli::cli_alert_info("Training Model: {model_name} ...")
    
    set.seed(1)
    # Define grid
    tune_grid <- expand.grid(nrounds=c(300, 500), # number of trees, default is 100
                             max_depth = c(2,3,4,5,6), # maximum tree depth, default is 6
                             eta = c(0.02, 0.05, 0.1), #learning rate, default is 0.3
                             gamma = c(0.01), # Used for tuning of Regularization, default: 0
                             colsample_bytree = c(0.75), # Column sampling, default: 1
                             subsample = c(0.50), # Row sampling, default: 1
                             min_child_weight = c(0)) #  Minimum leaf weight, default: 1
    
    # Track time
    start_time <- Sys.time()
    
    # # Start tuning the model
    xgb_tune <- caret::train(
      x = train_dummy_x._insurance,
      y = as.factor(train_dummy_y._insurance),
      trControl = train_control,
      tuneGrid = tune_grid,
      method = "xgbTree",
      verbose = TRUE,
      verbosity = 0
    )
    
    # Final grid
    final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds, # number of trees, default is 100
                              max_depth = xgb_tune$bestTune$max_depth, # maximum tree depth, default is 6
                              eta = xgb_tune$bestTune$eta, #learning rate, default is 0.3
                              gamma = c(0.01), # Used for tuning of Regularization, default: 0
                              colsample_bytree = c(0.75), # Column sampling, default: 1
                              subsample = c(0.50), # Row sampling, default: 1
                              min_child_weight = c(0)) #  Minimum leaf weight, default: 1
    
    # Run final model
    xgboost_insurance_cv <- caret::train(form = formula_model,
                                         data = train_insurance,
                                         trControl = train_control,
                                         tuneGrid = final_grid,
                                         method = "xgbTree",
                                         verbose = TRUE
    )
    
    # End time
    end_time <- Sys.time()
    # Calculate elapsed time
    elapsed_time <- difftime(end_time, start_time, units = "mins")
    
    # Produce performance metrics for later plotting
    predict_xgboost_insurance_cv <- predict(xgboost_insurance_cv,test_insurance, type="prob") 
    Perf_xgboost_insurance_cv <- prediction(as.numeric(predict_xgboost_insurance_cv[[2]]), as.numeric(test_insurance[[outcome_var]]))
    AUC_xgboost_insurance_cv  <- performance(Perf_xgboost_insurance_cv, "auc") 
    ROC_xgboost_insurance_cv  <- performance(Perf_xgboost_insurance_cv, "tpr", "fpr")
    RP_xgboost_insurance_cv  <- performance(Perf_xgboost_insurance_cv, "prec","rec")
    
    # Create confusion matrix
    pred_dummy <-  ifelse(predict_xgboost_insurance_cv[[2]]> 0.5 , 1,0)
    cm_xgboost_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance[[outcome_var]]), positive = "1") 
    
    # List the variable importance in descending order
    importance <- as.data.frame(varImp(xgboost_insurance_cv)$importance)
    importance_xgboost <- importance %>% arrange(desc(importance))
    names(importance_xgboost) <- "importance_xgboost"
    importance_xgboost$variables <- rownames(importance_xgboost)
    # get shapley values
    shap_values <- xgb.plot.shap(data = as.matrix(train_insurance[,-1]), features = colnames(train_insurance)[-1], model = xgboost_insurance_cv$finalModel, top_n = NULL, plot = FALSE)
    shap_importance <- colMeans(abs(shap_values$shap_contrib))
    shap_importance <- sort(shap_importance, decreasing = TRUE)
    shap_importance <- as.data.frame(shap_importance)
    shap_importance$variables <- rownames(shap_importance)
    importance_xgboost <- importance_xgboost %>% dplyr::left_join(shap_importance, by = "variables")
    importance_df <- importance_df %>% dplyr::left_join(as.data.frame(importance_xgboost), by = "variables")
    
    # Create df with most important performance metrics
    metrics_xgboost1 <- as.data.frame(cm_xgboost_insurance_cv$overall)
    metrics_xgboost2 <- as.data.frame(cm_xgboost_insurance_cv$byClass)
    metrics_xgboost1 <- rename(metrics_xgboost1 , c("xgboost_cv_insurance" = "cm_xgboost_insurance_cv$overall"))
    metrics_xgboost2 <- rename(metrics_xgboost2 , c("xgboost_cv_insurance" = "cm_xgboost_insurance_cv$byClass"))
    metrics_xgboost <- as.data.frame(rbind(metrics_xgboost1, metrics_xgboost2))
    
    # Append metrics to final metrics file
    if (!is.null(nrow(metrics_df))) {
      metrics_df <- cbind(metrics_df, metrics_xgboost)
    } else{
      metrics_df <- metrics_xgboost
    }
    
    
    cli::cli_alert_info("Training Model: {model_name} DONE. Time taken: {elapsed_time} minutes.")
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # VIII. Additional: General Logit Model with subset of variables ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if ("subset" %in% model_list) {
    
    
    # List of variables of interest
    vars <- c(outcome_var, "married", "currently_working", 
              "rural", "smokes", "media_HH", 
              "female_HH_head")
    
    # Select only the available variables in the dataframe
    df_prediction_subset <- df_prediction %>%
      dplyr::select(any_of(vars), contains("age_cat"), contains("education"), 
                    contains("wealthindex"), contains("age_HH_head_cat"))
    
    model_name <- "basic_subset"
    cli::cli_alert_info("Train Model: {model_name}")
    
    # Start time
    start_time <- Sys.time()
    
    # Train Logit model
    logit_subset_insurance  <- glm(formula = formula_model, data=df_prediction_subset, family="binomial")
    
    # End time
    end_time <- Sys.time()
    
    # Calculate elapsed time
    elapsed_time <- difftime(end_time, start_time, units = "secs")
    
    # Produce performance metrics for later plotting
    predict_subset_insurance <- predict(logit_subset_insurance, df_prediction_subset)
    Perf_subset_insurance <- ROCR::prediction(as.numeric(predict_subset_insurance), as.numeric(df_prediction_subset[[outcome_var]]))
    AUC_subset_insurance <- performance(Perf_subset_insurance , "auc") 
    ROC_subset_insurance <- performance(Perf_subset_insurance, "tpr", "fpr")
    RP_subset_insurance <- performance(Perf_subset_insurance, "prec","rec")
    
    # Create confusion matrix
    pred_dummy <-  ifelse(predict_subset_insurance> 0.5 , 1,0) 
    cm_subset_insurance <- confusionMatrix(as.factor(pred_dummy),as.factor(df_prediction_subset[[outcome_var]]), positive = "1") 
    
    # Create df with most important performance metrics
    metrics_subset1 <- as.data.frame(cm_subset_insurance$overall)
    metrics_subset2 <- as.data.frame(cm_subset_insurance$byClass)
    metrics_subset1 <- rename(metrics_subset1 , c("logit_subset_insurance" = "cm_subset_insurance$overall"))
    metrics_subset2 <- rename(metrics_subset2 , c("logit_subset_insurance" = "cm_subset_insurance$byClass"))
    metrics_subset <- as.data.frame(rbind(metrics_subset1, metrics_subset2))
    
    if (!is.null(nrow(metrics_df))) {
      metrics_df <- cbind(metrics_df, metrics_subset)
    } else{
      metrics_df <- metrics_subset
    }
    
    # Create variable importance score vector
    importance <- as.data.frame(varImp(logit_subset_insurance))
    # Create dummy for whether coefficient is negative
    negative <- as.data.frame(logit_subset_insurance$coefficients[-1])
    negative <- na.omit(negative)
    for (row in 1:nrow(negative)) {
      importance[row,2] <- ifelse(negative[row,1] < 0, 1, 0)
    }
    names(importance) <- c("importance_subset", "Negative_subset")
    importance$variables <- rownames(importance)
    # Normalize it to 100
    importance$importance_subset <- (importance$importance_subset / max(importance$importance_subset)) * 100
    # sort
    importance_subset <- importance %>% arrange(desc(importance))
    # Append to df
    importance_df <- importance_df %>% dplyr::left_join(as.data.frame(importance_subset), by = "variables")
    
    cli::cli_alert_info("Train Model: {model_name} DONE. Time taken: {elapsed_time} seconds")
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #
  # IV. Output ----
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if (!("overview_only" %in% model_list)) {
    
    model_output_name <- paste(model_list, collapse = "_")
    
    # Export the performance metrics to .xlsx
    cli::cli_alert_info("Writing output for Model: {model_list}, Country: {country} ...")
    #Performance metrics
    metrics_df$model_names <- rownames(metrics_df)
    # Put "model_names" as first column
    metrics_df <- metrics_df %>%
      select(all_of("model_names"), everything())
    # write to .xlsx
    if (short_model == TRUE) {
      write_xlsx(metrics_df, path = paste0("short_model/Boxplots/data_",outcome_var,"/metrics_",model_output_name,"_",country,"_short.xlsx"))
    } else {
      write_xlsx(metrics_df, path = paste0("Boxplots/data_",outcome_var,"/metrics_",model_output_name,"_",country,".xlsx"))
    }
    # Importance scores
    if (short_model == TRUE) {
      write_xlsx(importance_df, path = paste0("short_model/Varimp_plots/data_",outcome_var,"/importance_",model_output_name,"_",country,"_short.xlsx"))
    } else {
      write_xlsx(importance_df, path = paste0("Varimp_plots/data_",outcome_var,"/importance_",model_output_name,"_",country,".xlsx"))
    }
    
    cli::cli_alert_info("Writing output for Model: {model_list}, Country: {country} DONE.")
    
    # Calculate predicted probs, append to df and save df as final_df
    df_final <- df_insurance
    predict_xgboost_insurance_cv <- predict(xgboost_insurance_cv, df_prediction, type="prob")
    Perf_xgboost_insurance_cv <- prediction(as.numeric(predict_xgboost_insurance_cv[[2]]), as.numeric(df_prediction[[1]]))
    pred_dummy <-  ifelse(predict_xgboost_insurance_cv[[2]]> 0.5 , 1,0)
    df_final$predicted_class <- pred_dummy
    df_final$NAME_0 <- country
    if (short_model == TRUE) {
      save(df_final, file=paste0("short_model/final_data/",outcome_var,"/df_final_",outcome_var,"_",country,"_short.RData"))
    } else {
      save(df_final, file=paste0("final_data/",outcome_var,"/df_final_",outcome_var,"_",country,".RData"))
    }
  }

  # Create variables overview files
  # 3.5. Crete overview table and do some specific adjustments ----
  cli::cli_alert_info("create overview...")
  # Create variables overview
  overview = read.xlsx("variable_names.xlsx")
  for (i in 1:nrow(overview)) {
    overview[i, which(colnames(overview) == country)] <-  ifelse(overview$variables[i] %in% colnames(df_insurance), 1, 0)
    # Overwrite insurance with 0 if it is the primary outcome
    if (outcome_var == "insurance") {
      overview[which(overview$variables == "insurance"), which(colnames(overview) == country)] <- 0
    }
  }

  # Exclude names that contain "religion", "language", or "ethnicity"
  exclude_strings <- c("religion", "language", "ethnicity")
  names_to_exclude <- sapply(exclude_strings, function(x) grepl(x, overview$variables))
  combined_exclusion <- Reduce(`|`, names_to_exclude)
  # Update the overview vector
  overview[!overview$variables %in% colnames(df_prediction) & !combined_exclusion, ] <- 0
  if (country == first(countries)) {
    merged_overview <<- overview
  } else {
    merged_overview[country] <<- overview[country]
  }

  # Write to xlsx if last iteration step is done
  if (country == last(countries)) {
    #Exclude vars
    columns_to_drop <- c("anemia", "hemoglobin", "trips", "tetanus",
                         "checkup_delivery", "drugs_parasites",
                         "injected_tuberculosis", "csection")

    merged_overview <- merged_overview %>%
      dplyr::filter(!variables %in% columns_to_drop)
    # drop rows with missings
    merged_overview <- merged_overview %>%
      dplyr::filter(variables != "")
    # Identify rows with only zeros
    zero_rows <- apply(merged_overview[, -1], 1, function(row) all(row == 0))
    if (any(zero_rows, na.rm = TRUE)) {
      cli::cli_h2("Dropped Rows for {outcome_var}:")
      cli::cli_alert_info(paste(merged_overview$variables[zero_rows], collapse = ", "))
    } else {
      cli_alert_info("No rows with only zeros were found.")
    }
    # Keep rows without all zeros
    merged_overview <- merged_overview[!zero_rows, ]
    # drop na rows
    merged_overview <- merged_overview[!is.na(merged_overview$variables),]
    if (short_model == TRUE) {
      write.xlsx(merged_overview, paste0("short_model/merged_overview_", outcome_var, ".xlsx"))
    } else {
      write.xlsx(merged_overview, paste0("merged_overview_", outcome_var, ".xlsx"))
    }
    cli::cli_alert_info("create overview DONE")
  }
}

# 5. Functions for creating figures and tables ----
create_importance_plot <- function(path = path_importance, model = model, rank = FALSE, outcome_var = outcome_var, short_model = FALSE) {
  cli::cli_alert_info("Creating importance plots for outcome {outcome_var} and model {model}...")
  # Read in the performance excel files for each country
  for (country in countries) {
    # List all relevant .xlsx files
    if (short_model == TRUE) {
      file <- list.files(paste0(path_importance_short,"_",outcome_var), pattern = paste0("importance.*\\",country,"_short.xlsx$"), full.names = TRUE)
    } else {
      file <- list.files(paste0(path_importance,"_",outcome_var), pattern = paste0("importance.*\\",country,".xlsx$"), full.names = TRUE)
    }
    # Read all files and combine them into a single data frame
    importance_df <- lapply(file, read_excel)
    importance_df[[1]]$Country <- country
    importance_df <- do.call(rbind, importance_df)
    importance_df <- importance_df %>%
      dplyr::select(all_of("Country"), everything())
    if (length(importance_df_merged) <= 1) {
      importance_df_merged <- importance_df
    } else {
      importance_df_merged <- rbind(importance_df_merged, importance_df)
    }
  }
  
  # Filter out outcome variable
  importance_df_merged <- importance_df_merged %>% dplyr::filter(variables != outcome_var)
  # Add vector of zeros for xgboost
  importance_df_merged$Negative_xgboost <- 0
  
  # Rename variables for nicer illustration
  # Rename specific names
  importance_df_merged <- importance_df_merged %>%
    mutate(variables = recode(variables,
                              "member_all_HH" = "Number of HH members",
                              "member_child_under_5" = "Number of HH members under 5",
                              "member_women_HH" = "Number of women in HH",
                              "read_newspaper" = "Never read newspapers",
                              "education_HH_tertiary_edu" = "HH Education: Tertiary education",
                              "media" = "Exposure to mass media",
                              "education_tertiary_edu" = "Education: Tertiary education",
                              "education_primary_edu" = "Education: Primary education",
                              "education_no_edu" = "Education: No education",
                              "education_secondary_edu" = "Education: Secondary education",
                              "radio" = "Has radio",
                              "education_HH_secondary_edu" = "HH Education: Secondary education",
                              "sondaughterhome" = "Has children at home",
                              "age_HH_head_cat_45_49" = "Age HH head: 45-49",
                              "age_cat_25_29" = "Age: 25-29",
                              "age_cat_15_24" = "Age: 15-24",
                              "age_HH_head_cat_35_39" = "Age HH head: 35-39",
                              "age_HH_head_cat_40_44" = "Age HH head: 40-44",
                              "education_HH_no_edu" = "HH Education: No education",
                              "media_HH" = "HH Exposure to mass media",
                              "education_HH_primary_edu" = "HH Education: Primary education",
                              "age_HH_head_cat_55_59" = "Age HH head: 55-59",
                              "age_HH_head_cat_50_54" = "Age HH head: 50-54",
                              "age_HH_head_cat_59" = "Age HH head: 59",
                              "sondaughterdied" = "Child died",
                              "age_HH_head_cat_30_34" = "Age HH head: 30-34",
                              "age_HH_head_cat_25_29" = "Age HH head: 25-29",
                              "mobile" = "Mobile phone",
                              "member_men_HH" = "Number of men in HH",
                              "wealthindex_1_Quintile" = "Wealthindex: 1st Quintile",
                              "wealthindex_2_Quintile" = "Wealthindex: 2nd Quintile",
                              "wealthindex_3_Quintile" = "Wealthindex: 3rd Quintile",
                              "wealthindex_4_Quintile" = "Wealthindex: 4th Quintile",
                              "wealthindex_5_Quintile" = "Wealthindex: 5th Quintile",
                              "birth_last_3y" = "Gave birth in last 3 years",
                              "age_cat_30_34" = "Age: 30-34",
                              "age_cat_35_39" = "Age: 35-39",
                              "age_cat_40_44" = "Age: 40-44",
                              "contraceptive_health_care" = "Contraceptive requires HF access"
    ))
  
  # Add star to new variables
  if (short_model == FALSE) {
    importance_df_sub <- importance_df_merged %>% dplyr::filter(!is.na(importance_subset))
  } else {
    importance_df_sub <- importance_df_merged %>% dplyr::filter(!is.na(importance_general))
  }
  
  importance_df_merged <- importance_df_merged %>%
    mutate(variables = ifelse(variables %in% importance_df_sub$variables | variables %in% c("Wealthindex: 5th Quintile", "Education: Tertiary education", 
                                                                                            "age_cat_45_49", "age_cat_50_54", "age_cat_55_59", "age_cat_59",
                                                                                            "age_HH_head_cat_0_14", "age_HH_head_cat_59", "education_HH_tertiary_edu",
                                                                                            "age_cat_0_14"),
                              variables, paste0(variables, "*")))
  
  # General renaming
  importance_df_merged <- importance_df_merged %>%
    dplyr::mutate(variables = gsub("_", " ", variables))
  importance_df_merged$variables <- cap_first(importance_df_merged$variables)
  
  # Create separate dfs
  z_score <- qnorm(1 - 0.05 / 2)
  if (short_model == FALSE) {
    model_title <- ifelse(model == "importance_xgboost", "ML Model",
                          ifelse(model == "importance_subset", "Classical Model", "Logit with full set of variables"))
    negative_str <- ifelse(model == "importance_subset", "Negative_subset",
                           ifelse(model == "importance_general", "Negative_general", "Negative_xgboost"))
  } else {
    model_title <- ifelse(model == "importance_xgboost", "ML Model", "Classical Model")
    negative_str <- ifelse(model == "importance_general", "Negative_general", "Negative_xgboost")
  }
  
  importance_general_df <- importance_df_merged %>%
    dplyr::select(c(Country, variables, all_of(model), contains(negative_str))) %>%
    filter(!is.na(get(model)))
  # Rename negative dummy 
  colnames(importance_general_df)[4] <- "Negative"
  
  if (rank == TRUE) {
    importance_general_df <- importance_general_df %>% 
      group_by(Country) %>%
      arrange(desc(get(model))) %>%
      mutate(rank = row_number()) %>%
      ungroup() %>%
      group_by(variables) %>%
      filter(n_distinct(Country) == n_distinct(importance_general_df$Country)) %>%
      distinct(Country, variables, .keep_all = TRUE) %>%
      summarise(mean_rank = mean(rank, na.rm = TRUE),
                se = sd(rank, na.rm = TRUE) / sqrt(sum(!is.na(rank))),  # Handle NA in SE calculation
                Sign = as.factor(ifelse(mean(Negative) >= 0.5, "Negative", "Positive"))) %>%
      mutate(ci_lower = mean_rank - z_score * se,
             ci_upper = mean_rank + z_score * se) %>%
      arrange(mean_rank) %>%
      slice_head(n = 15)
  } else {
    importance_general_df <- importance_general_df %>% 
      group_by(variables) %>%
      filter(n_distinct(Country) == n_distinct(importance_general_df$Country)) %>%
      distinct(Country, variables, .keep_all = TRUE) %>%
      summarise(mean_value = mean(get(model), na.rm = TRUE),
                se = sd(get(model), na.rm = TRUE) / sqrt(sum(!is.na(get(model)))), # Handle NA in SE calculation
                Sign = as.factor(ifelse(mean(Negative) >= 0.5, "Negative", "Positive"))) %>% 
      mutate(ci_lower = mean_value - z_score * se,
             ci_upper = mean_value + z_score * se) %>%
      dplyr::arrange(desc(mean_value)) %>%
      slice_head(n = 15)
  }
  
  # GGPLOT: Create the variable importance plot
  if (rank == FALSE) {
    imp_plot <- ggplot(importance_general_df, aes(x = mean_value, y = reorder(variables, mean_value), fill = Sign)) +
      geom_bar(stat = "identity", width = 0.7) +    # Horizontal bars
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +  # Error bars for CI
      labs(x = element_blank(),
           y = element_blank(),
           title = model_title) +
      # subtitle = "Normalized average variable importance score (Max = 100)") +
      theme_minimal() +
      xlim(0,100) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(linewidth = 0.3),
            panel.grid.minor.x = element_line(linewidth = 0.3)) +
      guides(fill = "none")
    
    # Add conditional fill scale based on model type
    if (model == "importance_xgboost") {
      imp_plot <- imp_plot + scale_fill_grey(start = 0.25, end = 0.5)
    } else {
      imp_plot <- imp_plot + scale_fill_grey(start = 0.5, end = 0.75)
    }
    if (short_model == TRUE) {
      saved_plot <- ggsave(paste0("short_model/Varimp_plots/variable_plot_",model,"_",outcome_var,"_short.png"),plot = imp_plot, width = 8, height = 5, dpi = 1000)
    } else {
      saved_plot <- ggsave(paste0("Varimp_plots/variable_plot_",model,"_",outcome_var,".png"),plot = imp_plot, width = 8, height = 5, dpi = 1000)
    }
    cli::cli_alert_info("Creating importance plots for outcome {outcome_var} and model {model} DONE")
    return(saved_plot)
  } else {
    imp_plot_rank <- ggplot(importance_general_df, aes(x = mean_rank, y = reorder(variables, mean_rank, decreasing = TRUE), fill = Sign)) +
      geom_bar(stat = "identity", width = 0.7) +    # Horizontal bars
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +  # Error bars for CI
      labs(x = element_blank(),
           y = element_blank(),
           title = model_title) +
      # subtitle = "Average variable rank to predict insurance status") +
      scale_fill_grey(start = 0.3, end = 0.9) +  # Shades of grey for fill
      theme_minimal() +
      xlim(0,80) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(linewidth = 0.3),
            panel.grid.minor.x = element_line(linewidth = 0.3)) +
      # Conditionally remove legend
      guides(fill = "none")
    if (short_model == TRUE) {
      saved_plot <- ggsave(paste0("short_model/Varimp_plots/variable_rank_plot_",model,"_",outcome_var,"_short.png"),plot = imp_plot_rank, width = 8, height = 5, dpi = 1000)
    } else {
      saved_plot <- ggsave(paste0("Varimp_plots/variable_rank_plot_",model,"_",outcome_var,".png"),plot = imp_plot_rank, width = 8, height = 5, dpi = 1000)
    }
    cli::cli_alert_info("Creating importance plots for outcome {outcome_var} and model {model} DONE")
    return(saved_plot)
  }
}

create_boxplots <- function(outcome_var, short_model = FALSE) {
  cli::cli_alert_info("Creating boxplots for outcome {outcome_var}...")
  for (country in countries) {
    # List all relevant .xlsx files
    if (short_model == TRUE) {
      file <- list.files(paste0(path_boxplots_short,"_",outcome_var), pattern = paste0("metrics.*\\",country,"_short.xlsx$"), full.names = TRUE)
    } else {
      file <- list.files(paste0(path_boxplots,"_",outcome_var), pattern = paste0("metrics.*\\",country,".xlsx$"), full.names = TRUE)
    }
    # Read all files and combine them into a single data frame
    performance_df <- as.data.frame(lapply(file, read_excel))
    performance_df$Country <- country
    performance_df <- performance_df %>%
      dplyr::select(all_of("Country"), everything())
    if (length(performance_df_merged) <= 1) {
      performance_df_merged <- performance_df
    } else {
      performance_df_merged <- rbind(performance_df_merged, performance_df)
    }
  }
  
  # Drop general model
  if (short_model == FALSE) {
    performance_df_merged <- performance_df_merged %>% dplyr::select(-c(logit_general_insurance))
  }
  
  # Filter for the relevant performance metrics
  performance_df_merged <- performance_df_merged %>% 
    dplyr::filter(model_names %in% c("Accuracy", "Kappa", "Sensitivity", "Specificity"))
  
  # Rename columns
  if (short_model == FALSE) {
    performance_df_merged <- performance_df_merged %>% dplyr::rename("Performance_Metric" = model_names,
                                                                     "ML Model" = xgboost_cv_insurance,
                                                                     "Classical Model" = logit_subset_insurance)
  } else {
    performance_df_merged <- performance_df_merged %>% dplyr::rename("Performance_Metric" = model_names,
                                                                     "ML Model" = xgboost_cv_insurance,
                                                                     "Classical Model" = logit_general_insurance)
  }
  # Rename some values
  performance_df_merged <- performance_df_merged %>%
    mutate(Performance_Metric = gsub("Kappa", "Cohen's Kappa", Performance_Metric))
  
  # Reshape data to long format for ggplot
  performance_df_merged <- performance_df_merged %>%
    pivot_longer(cols = contains("Model"), 
                 names_to = "Model", 
                 values_to = "Value")
  
  performance_df_merged$Model <- factor(performance_df_merged$Model, levels = c("Classical Model", "ML Model"))
  
  # Order the model names
  performance_df_merged$Performance_Metric <- factor(performance_df_merged$Performance_Metric, 
                                                     levels = c("Accuracy", "Sensitivity", "Specificity", "Cohen's Kappa"))
  
  # Create boxplots
  boxplot <- ggplot(performance_df_merged, aes(x = Model, y = Value, fill = Model)) +
    stat_boxplot(geom ='errorbar', width = 0.6) +
    geom_boxplot() +
    geom_point() +
    labs(x = "Models", 
         y = "Performance") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 1)) + # Keeps y-axis limits but does not clip data
    facet_wrap(~ Performance_Metric, scales = "free", nrow = 1) +
    scale_fill_grey(start = 0.3, end = 0.8) + 
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
  # Export boxplot to png
  if (short_model == TRUE) {
    saved_plot <- ggsave(paste0("short_model/Boxplots/boxplots_2_models_",outcome_var,"_short.png"), plot = boxplot, width = 7, height = 5, dpi = 1000)
  } else {
    saved_plot <- ggsave(paste0("Boxplots/boxplots_3_models_",outcome_var,".png"), plot = boxplot, width = 7, height = 5, dpi = 1000)
  }
  cli::cli_alert_info("Creating boxplots for outcome {outcome_var} DONE")
  return(saved_plot)
}


create_scatterplots <- function(outcome_var, metric, short_model = FALSE) {
  cli::cli_alert_info("Creating scatterplot for outcome {outcome_var} and metric {metric}...")
  country_codes_countries <- list(country_codes = c("BJ","BF","BU","TD","CM","CD","CIG", "CI","SZ","ET","GM",
                                                    "GH","GN","KE","LS","LB","MD","MW","ML","MR","NM","NI","NG", 
                                                    "RW","SN","SL","TZ","TG","UG","ZM","ZW"),
                                  countries = countries)
  
  for (i in 1:length(country_codes_countries[[1]])) {
    # List all relevant .xlsx files
    if (short_model == TRUE) {
      file <- list.files(paste0(path_boxplots_short,"_",outcome_var), pattern = paste0("metrics.*\\",country_codes_countries[[2]][[i]],"_short.xlsx$"), full.names = TRUE)
    } else {
      file <- list.files(paste0(path_boxplots,"_",outcome_var), pattern = paste0("metrics.*\\",country_codes_countries[[2]][[i]],".xlsx$"), full.names = TRUE)
    }
    # Read all files and combine them into a single data frame
    performance_df <- as.data.frame(lapply(file, read_excel))
    performance_df$Country <- country_codes_countries[[2]][[i]]
    performance_df$Country_Code <- country_codes_countries[[1]][[i]]
    performance_df <- performance_df %>%
      dplyr::select(all_of("Country"), everything())
    if (length(performance_df_merged) <= 1) {
      performance_df_merged <- performance_df
    } else {
      performance_df_merged <- rbind(performance_df_merged, performance_df)
    }
  }
  
  # Drop general model
  if (short_model == FALSE) {
    performance_df_merged <- performance_df_merged %>% dplyr::select(-c(logit_general_insurance))
  }
  
  # Filter for the relevant performance metrics
  performance_df_merged <- performance_df_merged %>% 
    dplyr::filter(model_names %in% c(metric))
  
  # rename cols
  if (short_model == FALSE) {
    performance_df_merged <- performance_df_merged %>% dplyr::rename("Performance_Metric" = model_names,
                                                                     "ML_Model" = xgboost_cv_insurance,
                                                                     "Classical_Model" = logit_subset_insurance,
                                                                     "Country_Code" = Country_Code)
  } else {
    performance_df_merged <- performance_df_merged %>% dplyr::rename("Performance_Metric" = model_names,
                                                                     "ML_Model" = xgboost_cv_insurance,
                                                                     "Classical_Model" = logit_general_insurance,
                                                                     "Country_Code" = Country_Code)
  }
  
  # Rename some values
  if (metric == "Kappa") {
    performance_df_merged <- performance_df_merged %>%
      mutate(Performance_Metric = gsub("Kappa", "Cohen's Kappa", Performance_Metric))
  }
  
  # Create scatterplots
  scatterplot <- ggplot(performance_df_merged, aes(x = ML_Model, y = Classical_Model)) +
    geom_point() +  
    geom_text(
      aes(label = ifelse(ML_Model < Classical_Model, Country_Code, "")),  
      vjust = -0.5, hjust = -0.4, color = "darkgrey", size = 3
    ) +
    labs(x = "ML Model", 
         y = "Classical Model") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), expand = c(0.01, 0.01)) +
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey80"), 
      axis.text = element_text(color = "black"),
      axis.ticks = element_line(color = "black")
    )
  
  # Export scatterplot to png
  if (short_model == TRUE) {
    saved_plot <- ggsave(paste0("short_model/Scatterplots/scatter_2_models_",outcome_var,"_",metric,"_short.png"), plot = scatterplot, width = 7, height = 5, dpi = 500)
  } else {
    saved_plot <- ggsave(paste0("Scatterplots/scatter_2_models_",outcome_var,"_",metric,".png"), plot = scatterplot, width = 7, height = 5, dpi = 500)
  }
  cli::cli_alert_info("Creating scatterplot for outcome {outcome_var} and metric {metric} DONE.")
  return(saved_plot)
}

# Function to create variable importance data table
create_varimp_data_table <- function(outcome_var, short_model = FALSE) {
  merged_importance <- NA
  importance_path <- paste0("Varimp_plots/data_",outcome_var)
  importance_path_short <- paste0("short_model/Varimp_plots/data_",outcome_var)
  full_path <- file.path(getwd(), importance_path)
  full_path_short <- file.path(getwd(), importance_path_short)
  
  for (country in countries) {
    # List all relevant .xlsx files
    if (short_model == TRUE) {
      file <- list.files(full_path_short, pattern = paste0(country,"\\.xlsx$"), full.names = TRUE)
    } else {
      file <- list.files(full_path, pattern = paste0(country,"\\.xlsx$"), full.names = TRUE)
    }
    # Read all files and combine them into a single data frame
    importance <- as.data.frame(lapply(file, read_excel))
    # drop all negative dummy cols
    importance <- importance %>% dplyr::select(c(variables, shap_importance))
    colnames(importance) <- c("variables", paste0("XGBoost_",country))
    if (length(merged_importance) <= 1) {
      merged_importance <- importance
    } else {
      merged_importance <- full_join(merged_importance, importance, by = "variables")
    }
  }
  
  # Sort by column names
  merged_importance <- merged_importance[, order(colnames(merged_importance))]
  # Drop first row
  merged_importance <- merged_importance[-1,]
  # Put variables as first column
  merged_importance <- merged_importance %>%
    select(all_of("variables"), everything())
  
  # Write to excel
  if (short_model == TRUE) {
    write.xlsx(merged_importance, paste0("short_model/merged_importance_",outcome_var,".xlsx"))
  } else {
    write.xlsx(merged_importance, paste0("merged_importance_",outcome_var,".xlsx"))
  }
}

# Function to create correlation plots, one for whole inputted set and another for only top correlated ones
create_corrplot <- function(short_model = FALSE, outcome_var = outcome_var) {
  # Create a full dataset with all countries
  if (short_model == FALSE) {
    input_path <- paste0("New_data_sets/", outcome_var,"/")
  } else {
    input_path <- paste0("short_model/final_data/", outcome_var, "/")
  }
  
  # Initialize an empty list to store data frames
  country_dfs <- list()
  for (country in countries) {
    if(short_model == FALSE) {
      country_df <- get(load(paste0(input_path, country, ".RData")))
    } else {
      country_df <- get(load(paste0(input_path, "df_final_", outcome_var, "_", country, "_short.RData")))
    }
    # drop country-specific variables and vars with no variance
    if (short_model == FALSE) {
      country_df <- country_df %>% dplyr::select(-c(contains("language"), contains("ethnicity"), contains("religion"), region, female, slept))
    } else {
      country_df <- country_df %>% dplyr::select(-c(hv001, predicted_class, NAME_0, region))
    }
    country_dfs[[country]] <- country_df
  }
  # Merge all data frames, handling different column structures
  country_df_merged <- as.data.frame(bind_rows(country_dfs))
  
  # Create correlation matrix
  cor_matrix_df <- country_df_merged %>% as.matrix()
  
  # Compute Pearson correlation matrix
  cor_matrix <- cor_matrix_df %>% 
    cor(use = "pairwise.complete.obs")
  
  # Create correlation plots
  corplot <- ggcorrplot(cor_matrix, type = "upper",
                        outline.col = "white",
                        ggtheme = ggplot2::theme_minimal(),
                        colors = c("#E46726", "white", "#6D9EC1"),  # Red for negative, blue for positive
                        show.diag = TRUE,
                        lab = FALSE,
                        lab_size = 1) +
    theme(axis.text.x = element_text(size = 3),  # Adjust x-axis text size
          axis.text.y = element_text(size = 3))  # Adjust y-axis text size
  corplot
  # Save
  if (short_model == FALSE) {
    ggsave("Tables_Figures/corplot.png", plot = corplot)
  } else {
    ggsave("short_model/corplot.png", plot = corplot)
  }
  
  cor_matrix_filtered <- cor_matrix
  cor_matrix_filtered[abs(cor_matrix_filtered) <= 0.5] <- NA
  
  # Create a copy of the matrix without the diagonal for filtering
  cor_matrix_no_diag <- cor_matrix_filtered
  diag(cor_matrix_no_diag) <- NA  # Remove diagonal values (1s)
  
  # Identify rows and columns where at least one value (excluding diagonal) is > 0.5
  rows_to_keep <- apply(abs(cor_matrix_no_diag), 1, function(x) any(!is.na(x)))
  cols_to_keep <- apply(abs(cor_matrix_no_diag), 2, function(x) any(!is.na(x)))
  
  # Subset the matrix to only keep significant correlations
  cor_matrix_reduced <- cor_matrix_filtered[rows_to_keep, cols_to_keep]
  
  # Create correlation plots
  corplot_reduced <- ggcorrplot(cor_matrix_reduced, type = "upper",
                                outline.col = "white",
                                ggtheme = ggplot2::theme_minimal(),
                                colors = c("#E46726", "white", "#6D9EC1"),  # Red for negative, blue for positive
                                show.diag = TRUE,
                                lab = TRUE,
                                lab_size = 1.75) +
    theme(axis.text.x = element_text(size = 6),  # Adjust x-axis text size
          axis.text.y = element_text(size = 6))  # Adjust y-axis text size
  corplot_reduced
  # Save
  if (short_model == FALSE) {
    ggsave("Tables_Figures/corplot_reduced.png", plot = corplot_reduced)
  } else {
    ggsave("short_model/corplot_reduced.png", plot = corplot_reduced)
  }
  
  if (short_model == TRUE) {
    # Extra: Save pooled df to newdatasets
    save(country_df_merged, file = paste0("short_model/final_data/", outcome_var, "/df_final_",outcome_var,"_all_short.RData"))    
  }
}

# Create function to make the out-of-sample predictions and create district- and region-level maps
make_OOB_predictions_and_maps <- function(outcome_var = outcome_var, map_type = "all") {
  file_names <- list.files(path = "DataRaw/IPUMS/", full.names = TRUE)
  countries <- c("Benin","Burkina.Faso","Burundi","Cameroon", "Chad",
                 "DRC","Republic.of.the.Congo","Cote.dIvoire","Eswatini","Ethiopia","The.Gambia",
                 "Ghana","Guinea","Kenya","Lesotho","Liberia","Madagascar", "Malawi",
                 "Mali", "Mauritania", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal","South.Africa",
                 "Sierra.Leone", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")
  # 0. SETUP -----
  # read in GADM data
  mADM<- readRDS(file = "DataRaw/GADM/mADM.rds")
  mADMdata <- st_as_sf(mADM) %>% st_transform(4269)
  
  for (country in countries) {
    country_for_filter <- ifelse(country == "Burkina.Faso", "Burkina Faso", 
                                 ifelse(country == "The.Gambia", "Gambia", 
                                        ifelse(country == "DRC", "Democratic Republic of the Congo",
                                               ifelse(country == "Sierra.Leone", "Sierra Leone",
                                                      ifelse(country == "South.Africa", "South Africa", 
                                                             ifelse(country == "Cote.dIvoire", "Côte d'Ivoire", 
                                                                    ifelse(country == "Republic.of.the.Congo", "Republic of Congo",
                                                                           ifelse(country == "Eswatini", "Swaziland", country))))))))
    geo_path <- list.files(path = "Data/Geolocation/", pattern = paste0(".*",country_list[[country]][["country_code"]],"GE.*_data.*"), full.names = TRUE)
    ipums_path <- list.files(path = "DataRaw/IPUMS/", pattern = paste0("_*",tolower(country_list[[country]][["country_code"]])), full.names = TRUE)
    # Read in geo data
    if (is_empty(geo_path)) {
      cli::cli_alert_info("Skipping, no geodata found for {country}.")
      next  
    }
    df_geolocation <- read_dta(geo_path)

    # Read in short data set
    if (map_type == "predicted_dhs_region_long") {
      load(paste0("final_data/",outcome_var,"/df_final_",outcome_var,"_",country,".RData"))
    } else {
      load(paste0("short_model/final_data/",outcome_var,"/df_final_",outcome_var,"_",country,"_short.RData"))
    }
    
    # Drop insurance if it is not outcome
    if (outcome_var != "insurance" & "insurance" %in% colnames(df_final)) {
      df_final <- df_final %>% dplyr::select(-c(insurance))
    }
    #change naming for census matching
    colnames(df_final)[colnames(df_final) == "currently_working"] <- "working"
    colnames(df_final)[colnames(df_final) == "sondaughterhome"] <- "kidsathome"
    
    # Add geolocation to the DHS data
    df_geolocation$hv001 <- df_geolocation$DHSCLUST
    df_geolocation <- df_geolocation[,c("hv001", "LATNUM", "LONGNUM")]
    
    # Create final data
    df <- merge(df_final, df_geolocation, by = c("hv001"), all=TRUE)
    # Convert to sf objects
    df <- st_as_sf(df, coords = c("LONGNUM", "LATNUM"), crs = 4269)
    
    ## Load world_geolev2 shapefile (not available for Botswana)
    shp <- mADM
    shp <- sf::st_as_sf(shp)
    shp <- shp %>% dplyr::filter(NAME_0 == country_for_filter)
    
    shp <- st_make_valid(shp)
    
    shp <- shp %>%
      dplyr::mutate(id = row_number())
    
    # overlay DHS and mADM for each country separately - otherwise it crashes
    if (!(country_for_filter %in% mADMdata$NAME_0)) {
      cli::cli_alert_danger("Skipping for country {country}, no madmdata found")
      next
    }
    
    # Extract all boundary points while keeping the feature ID
    shp_df <- shp %>%  # Create a unique ID for each feature
      st_cast("MULTIPOINT") %>%  # Ensure polygons are expanded
      st_coordinates() %>%
      as.data.frame() %>%
      rename(long = X, lat = Y, id = L1) %>%
      left_join(shp, by = "id")  # Reattach attribute
    
    mADMdata_filtered <- mADMdata %>% dplyr::filter(NAME_0 == country_for_filter)
    data <- st_intersection(df, mADMdata_filtered)
    
    
    ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 1. Create country maps of average outcomes of xgboost predictions with census data, district level ----
    ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if (map_type == "all" | map_type == "predicted_census_district") {
      cli::cli_alert_info("Creating Predicted {outcome_var} Coverage Census map for {country} on district level...")
      # Example vector of filenames
      file_names <- list.files(path = "DataRaw/IPUMS/", full.names = TRUE)
      pattern <- tolower(country_list[[country]][["country_code"]])
      # Skip country if there does not exist geospatial data for it
      if (!any(grepl(pattern, file_names))) {
        cli::cli_alert_danger("Skipping for country {country}, no geodata (ipums) found")
        next
      } else {
        ## 1.1 Read in census and ipums data ----
        # Load census data
        load("DataRaw/Census/Census_clean_female_15_49.RData")
        # add ID to Census data
        census$ID <- 1:nrow(census)
        
        ## Load world_geolev2 shapefile (not available for Botswana)
        shp_file <- list.files(path = ipums_path, pattern = ".*\\.shp$", full.names = TRUE)
        shp <- sf::st_read(dsn = shp_file, stringsAsFactors = F)
        
        # 1.2. Clean up data and create final set ----
        # Drop insurance if it is not outcome
        if (outcome_var != "insurance" & "insurance" %in% colnames(df_final)) {
          df_final <- df_final %>% dplyr::select(-c(insurance))
        }
        
        # factorize all vars
        df_final[] <- lapply(df_final, as.factor)
        
        # Exclude country name
        df_final <- df_final %>% dplyr::select(-c(NAME_0, region, hv001, predicted_class))
        
        smp_size_insurance <- floor(0.66 * nrow(df_final))
        set.seed(1)   ## set the seed to make your partition reproducible
        train_ind_insurance <- sample(seq_len(nrow(df_final)), size = smp_size_insurance)
        train_insurance <- df_final[train_ind_insurance, ]
        test_insurance <- df_final[-train_ind_insurance, ]
        
        ## model
        set.seed(1)
        tune_grid <- expand.grid(nrounds=c(100,200,300,400), max_depth = c(3:7),
                                 eta = c(0.05, 1),
                                 gamma = c(0.01),
                                 colsample_bytree = c(0.75),
                                 subsample = c(0.50),
                                 min_child_weight = c(0))
        
        # set parameters for models
        train_control <- trainControl(method="repeatedcv", number=5, repeats=5, savePredictions="final")
        
        # Run final model
        formula_str <- sprintf("as.factor(%s) ~ .", outcome_var)
        formula_model <- as.formula(formula_str)
        # Drop vars with less than 2 levels
        train_insurance <- train_insurance %>%
          select_if(~ length(unique(.)) >= 2)
        cli::cli_alert_info("Training XGBoost...")
        xgboost_insurance_cv <- caret::train(form = formula_model,
                                             data = train_insurance,
                                             trControl = train_control,
                                             tuneGrid = tune_grid,
                                             method = "xgbTree",
                                             tuneLength = 10
        )
        cli::cli_alert_info("Training XGBoost DONE.")
        
        ## Out-of-sample Census
        census$age_cat_15_24 <- as.factor(census$age_cat_15_24)
        census$age_cat_25_29 <- as.factor(census$age_cat_25_29)
        census$age_cat_30_34 <- as.factor(census$age_cat_30_34)
        census$age_cat_35_39 <- as.factor(census$age_cat_35_39)
        census$age_cat_40_44 <- as.factor(census$age_cat_40_44)
        census$age_cat_45_49 <- as.factor(census$age_cat_45_49)
        census$education_no_edu <- as.factor(census$education_no_edu)
        census$education_primary_edu <- as.factor(census$education_primary_edu)
        census$education_secondary_edu <- as.factor(census$education_secondary_edu)
        census$education_tertiary_edu <- as.factor(census$education_tertiary_edu)
        
        predict_xgboost_census <- predict(xgboost_insurance_cv, census, type="prob")
        summary(predict_xgboost_census$`1`)
        predict_xgboost_census$ID <- 1:nrow(predict_xgboost_census)
        Census_xgboost_with_prediction <- merge(census, predict_xgboost_census, by="ID") 
        Census_xgboost_with_prediction[[outcome_var]] <- Census_xgboost_with_prediction$`1`
        
        # Save data file with prediction results
        save(Census_xgboost_with_prediction, file=paste0("Data/Census_xgboost_with_prediction_",country,"_",outcome_var,".RData"))
        
        #===
        # this part above should be included in short model generation in the same way as you do it for the DHS model
        #===
        load(paste0("Data/Census_xgboost_with_prediction_",country,"_",outcome_var,".RData"))
        
        Census_xgboost_with_prediction$average_district_insurance <- ave(as.numeric(Census_xgboost_with_prediction[[outcome_var]]),Census_xgboost_with_prediction$geolev2, FUN= function(x) mean(x, na.rm = TRUE))
        Census_xgboost_with_prediction$average_district_insurance <- Census_xgboost_with_prediction$average_district_insurance*100
        Census_xgboost_with_prediction <- Census_xgboost_with_prediction[!duplicated(Census_xgboost_with_prediction$geolev2), ]
        Census_xgboost_with_prediction <- rename(Census_xgboost_with_prediction , c("GEOLEVEL2" = "geolev2"))
        Census_xgboost_with_prediction$GEOLEVEL2 <- as.character(Census_xgboost_with_prediction$GEOLEVEL2)
        
        # Prepare shapefile dataframe
        shp <- shp %>%
          dplyr::mutate(id = row_number())
        shp <- st_make_valid(shp)
        
        # Extract all boundary points while keeping the feature ID
        shp_df <- shp %>%  # Create a unique ID for each feature
          st_cast("MULTIPOINT") %>%  # Ensure polygons are expanded
          st_coordinates() %>%
          as.data.frame() %>%
          rename(long = X, lat = Y, id = L1) %>%
          left_join(shp, by = "id")  # Reattach attribute
        
        ## merge shapefile with census data by district level
        merged_df <- shp_df %>%
          left_join(Census_xgboost_with_prediction, by="GEOLEVEL2")
        
        ## Map
        # Set limits manually
        if (outcome_var == "insurance" & country != "Rwanda" & country != "Ghana") {
          min = 0
          max = 40
        } else if (outcome_var == "locationBirth") {
          min = 0
          max = 100
        } else if (outcome_var == "postnatalcheck") {
          min = 0
          max = 100
        } else if (outcome_var == "visitHF12m") {
          min = 0
          max = 100
        } else if (outcome_var == "insurance" & country %in% c("Ghana", "Rwanda")) {
          min = 50
          max = 100
        }
        
        map_insurance<-ggplot() + 
          # Overlay with regional boundaries and fill 
          geom_polygon(data = merged_df, aes(x = long, y = lat, group = ADMIN_NAME, fill=average_district_insurance), colour="transparent", size=0.02)+
          annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
          annotation_scale(location = "br") + coord_sf(crs = 4326) +
          scale_fill_gradient(
            low = "firebrick3",
            high = "gold1",
            na.value = "white",
            guide = "colourbar",
            aesthetics = "fill",
            limits = c(min, max)
          ) +
          labs(y="Latitude", x = "Longtitude", fill="Percentage (%)") +
          # ggtitle(paste0("Predicted ",outcome_var," Census ",cap_first(country)))+
          theme(legend.position="right")
        map_insurance
        ggsave(map_insurance, file=paste0("Maps/Census/map_",outcome_var,"_",country,".png"))
        cli::cli_alert_info("Creating Predicted {outcome_var} Coverage Census map for {country} on district level DONE.")
      }
    }
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 2. Create DHS country maps of average outcomes of xgboost predictions, region level, short model ----
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if (map_type == "all" | map_type == "predicted_dhs_region_short") {
      cli::cli_alert_info("Creating Predicted {outcome_var} Coverage DHS map for {country} on region level...")
      ## create region averages
      data$average_insurance_region <- ave(as.numeric(data$predicted_class), data$GID_1, FUN= function(x) mean(x, na.rm = TRUE))
      summary(data$average_insurance_region)
      data$average_insurance_region <- data$average_insurance_region*100
      data_region <- data[!duplicated(data$GID_1), ]
      data_region <- data_region[,c("average_insurance_region", "GID_1")]
      
      ## Merge data and shapefile
      data_region<-shp_df %>%
        dplyr::left_join(data_region, by="GID_1")
      
      ## Map INSURANCE
      # Set limits manually
      if (outcome_var == "insurance" & country != "Rwanda" & country != "Ghana") {
        min = 0
        max = 40
      } else if (outcome_var == "locationBirth") {
        min = 0
        max = 100
      } else if (outcome_var == "postnatalcheck") {
        min = 0
        max = 100
      } else if (outcome_var == "visitHF12m") {
        min = 0
        max = 100
      } else if (outcome_var == "insurance" & country %in% c("Ghana", "Rwanda")) {
        min = 50
        max = 100
      }
      
      map_insurance <- ggplot() + 
        geom_polygon(data = data_region, aes(x = long, y = lat, group = NAME_2, fill = average_insurance_region), 
                     colour = "transparent", size = 0.02) +
        annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
        annotation_scale(location = "br") + 
        coord_sf(crs = 4326) +
        scale_fill_gradient(
          low = "firebrick3",
          high = "gold1",
          na.value = "grey",  # NA color is grey
          guide = "colourbar",
          aesthetics = "fill",  # Set scale range from 0 to 100
          limits = c(min, max),
          oob = scales::squish  # Values outside the range are squished to the nearest limit
        ) +
        labs(y = "Latitude", x = "Longitude", fill = "Percentage (%)") +
        # ggtitle(paste0("Predicted ",outcome_var," DHS ", cap_first(country)),
        #         subtitle = "Limited set of variables used as predictors") +
        theme(legend.position = "right") 
      map_insurance
      ggsave(map_insurance, file=paste0("Maps/DHS/predicted_short/map_",outcome_var,"_DHS_region_",country,".png"))
      cli::cli_alert_info("Creating Predicted {outcome_var} Coverage DHS map for {country} on region level DONE.")
    }
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 3. Create DHS country maps of average outcomes of xgboost predictions, region level, long model ----
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if (map_type == "all" | map_type == "predicted_dhs_region_long") {
      cli::cli_alert_info("Creating Predicted {outcome_var} Coverage DHS map for {country} on region level...")
      ## create region averages
      data$average_insurance_region <- ave(as.numeric(data$predicted_class), data$GID_1, FUN= function(x) mean(x, na.rm = TRUE))
      summary(data$average_insurance_region)
      data$average_insurance_region <- data$average_insurance_region*100
      data_region <- data[!duplicated(data$GID_1), ]
      data_region <- data_region[,c("average_insurance_region", "GID_1")]
      
      ## Merge data and shapefile
      data_region<-shp_df %>%
        dplyr::left_join(data_region, by="GID_1")
      
      # Set limits manually
      if (outcome_var == "insurance" & country != "Rwanda" & country != "Ghana") {
        min = 0
        max = 40
      } else if (outcome_var == "locationBirth") {
        min = 0
        max = 100
      } else if (outcome_var == "postnatalcheck") {
        min = 0
        max = 100
      } else if (outcome_var == "visitHF12m") {
        min = 0
        max = 100
      } else if (outcome_var == "insurance" & country %in% c("Ghana", "Rwanda")) {
        min = 50
        max = 100
      }
      
      ## Map INSURANCE
      map_insurance <- ggplot() + 
        geom_polygon(data = data_region, aes(x = long, y = lat, group = NAME_2, fill = average_insurance_region), 
                     colour = "transparent", size = 0.02) +
        annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
        annotation_scale(location = "br") + 
        coord_sf(crs = 4326) +
        scale_fill_gradient(
          low = "firebrick3",
          high = "gold1",
          na.value = "grey",  # NA color is grey
          guide = "colourbar",
          limits = c(min,max),
          aesthetics = "fill",  # Set scale range from 0 to 100
          oob = scales::squish  # Values outside the range are squished to the nearest limit
        ) +
        labs(y = "Latitude", x = "Longitude", fill = "Percentage (%)") +
        # ggtitle(paste0("Predicted ",outcome_var," DHS ", cap_first(country)),
        #         subtitle = "Full set of variables used as predictors") +
        theme(legend.position = "right") 
      map_insurance
      ggsave(map_insurance, file=paste0("Maps/DHS/predicted_long/map_",outcome_var,"_DHS_region_",country,".png"))
      cli::cli_alert_info("Creating Predicted {outcome_var} Coverage DHS map for {country} on region level DONE.")
    }

    
    ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 4. Create country maps of average outcomes of DHS data, region level ----
    ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if (map_type == "all" | map_type == "average_dhs_region") {
      cli::cli_alert_info("Creating Average {outcome_var} Coverage DHS map for {country} on regional level...")
      ## create region averages
      data$average_insurance_region <- ave(as.numeric(data[[outcome_var]]), data$GID_1, FUN= function(x) mean(x, na.rm = TRUE))
      summary(data$average_insurance_region)
      data$average_insurance_region <- data$average_insurance_region*100
      data_region <- data[!duplicated(data$GID_1), ]
      data_region <- data_region[,c("average_insurance_region", "GID_1")]
      
      ## Merge data and shapefile
      data_region<-shp_df%>%
        left_join(data_region, by="GID_1")
      
      # Set limits manually
      if (outcome_var == "insurance" & country != "Rwanda" & country != "Ghana") {
        min = 0
        max = 40
      } else if (outcome_var == "locationBirth") {
        min = 0
        max = 100
      } else if (outcome_var == "postnatalcheck") {
        min = 0
        max = 100
      } else if (outcome_var == "visitHF12m") {
        min = 0
        max = 100
      } else if (outcome_var == "insurance" & country %in% c("Ghana", "Rwanda")) {
        min = 50
        max = 100
      }
      
      ## Map INSURANCE
      map_insurance <- ggplot() + 
        geom_polygon(data = data_region, aes(x = long, y = lat, group = NAME_2, fill = average_insurance_region), 
                     colour = "transparent", size = 0.02) +
        annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
        annotation_scale(location = "br") + 
        coord_sf(crs = 4326) +
        scale_fill_gradient(
          low = "firebrick3",
          high = "gold1",
          na.value = "grey",  # NA color is grey
          guide = "colourbar",
          limits = c(min,max),
          aesthetics = "fill",  # Set scale range from 0 to 100
          oob = scales::squish  # Values outside the range are squished to the nearest limit
        ) +
        labs(y = "Latitude", x = "Longitude", fill = "Percentage (%)") +
        # ggtitle(paste0("Average ",cap_first(outcome_var)," DHS ", country)) +
        theme(legend.position = "right") 
      map_insurance
      ggsave(map_insurance, file = paste0("Maps/DHS/Average/average_map_",outcome_var,"_DHS_region_",country,".png"))
      cli::cli_alert_info("Creating Average {outcome_var} Coverage DHS map for {country} on regional level DONE.")
    }
  }
}

# 6. General / Functions for Census merging ----
# Function that scraps all the accents
plain_ascii<-function(x) {
  special_char<-c("í", "ï" ,"ó", "é", "è", "ê" ,"á", "ú", "ñ", "ü", "Í", "Ó", "É", "Á", "Ú", "Ñ", "Ü")
  plain_char<- c("i", "i","o", "e", "e", "e", "a", "u", "n", "u", "I", "O", "E", "A", "U", "N", "U")
  i<-1
  for(char in special_char) {
    x<-lapply(x, gsub, pattern=char, replacement=plain_char[i])
    i<-i+1
  }
  return(x)
}

