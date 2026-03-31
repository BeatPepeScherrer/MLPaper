#############################################################################################################
#  
#  Description: Census data
#
#  Authors:     Kathrin Durizzo               
#  Date:        25.11.2024
#
#############################################################################################################

#------------------------------------------------------------------------------------------------------------
#
#                                         Setup
#
#------------------------------------------------------------------------------------------------------------
# library(tidyverse)
# library(ggplot2)
# library(ggspatial)
# library(rgdal)
# library(broom)
# library(labelled)
# library(haven)
# library(readxl)
# library(xlsx)
# library(eeptools)
# library(fastDummies)

cli::cli_alert_info("Reading in data and merging...")

census <- read_dta("DataRaw/Census/ipumsi_00010_female_15_49.dta")
# in Stata: 
# drop if age<15 | age>59
# drop if sex != 2
# keep country dhs_ipumsi_bj dhs_ipumsi_bf dhs_ipumsi_cm dhs_ipumsi_eg dhs_ipumsi_et dhs_ipumsi_gh dhs_ipumsi_gn dhs_ipumsi_ke dhs_ipumsi_ls dhs_ipumsi_lr dhs_ipumsi_ml dhs_ipumsi_mw dhs_ipumsi_mz dhs_ipumsi_ng dhs_ipumsi_rw dhs_ipumsi_sn dhs_ipumsi_za dhs_ipumsi_sd dhs_ipumsi_tz dhs_ipumsi_tg dhs_ipumsi_ug dhs_ipumsi_zm dhs_ipumsi_zw sex marst age edattain nchild empstat urban electric toilet phone year sample serial geolev1 geolev2

country_code <- read_xlsx("DataRaw/Census/country_name_code.xlsx")

# Create one variable for all the region's names
census<-census%>%
  # Country name cleaning: Extract value label for a country and capitalize the first character
  dplyr::mutate(NAME_0 = labelled::to_character(country), 
         NAME_0=gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",NAME_0, perl = TRUE)) 



census<-census%>%
  # Add country ISO2 codes for future reference
  left_join(country_code,by="NAME_0")

cli::cli_alert_info("Reading in data and merging DONE.")

census<-census%>%
  # Merge all into one variable (to get a DHS code)
  unite(GID_1_IPUMS,dhs_ipumsi_bj:dhs_ipumsi_bf:dhs_ipumsi_cm:dhs_ipumsi_eg:dhs_ipumsi_et:dhs_ipumsi_gh:dhs_ipumsi_gn:dhs_ipumsi_ke:dhs_ipumsi_ls:
          dhs_ipumsi_lr:dhs_ipumsi_ml:dhs_ipumsi_mw:dhs_ipumsi_mz:dhs_ipumsi_ng:dhs_ipumsi_rw:dhs_ipumsi_sn:dhs_ipumsi_za:dhs_ipumsi_sd:dhs_ipumsi_tz:
          dhs_ipumsi_tg:dhs_ipumsi_ug:dhs_ipumsi_zm:dhs_ipumsi_zw,na.rm=TRUE,remove = FALSE)%>%
  # Region name cleaning: Convert the numeric code of regions into a string based on their value labels
  mutate_at(vars(contains("dhs_ipumsi_")),~to_character(.),na.rm=TRUE)%>%
  unite(NAME_1_IPUMS,dhs_ipumsi_bj:dhs_ipumsi_bf:dhs_ipumsi_cm:dhs_ipumsi_eg:dhs_ipumsi_et:dhs_ipumsi_gh:dhs_ipumsi_gn:dhs_ipumsi_ke:dhs_ipumsi_ls:
          dhs_ipumsi_lr:dhs_ipumsi_ml:dhs_ipumsi_mw:dhs_ipumsi_mz:dhs_ipumsi_ng:dhs_ipumsi_rw:dhs_ipumsi_sn:dhs_ipumsi_za:dhs_ipumsi_sd:dhs_ipumsi_tz:
          dhs_ipumsi_tg:dhs_ipumsi_ug:dhs_ipumsi_zm:dhs_ipumsi_zw,na.rm=TRUE,remove = FALSE)%>%
  # Remove special characters and create a unique region identifier
  mutate(NAME_1_RAW= gsub('[[:punct:] ]+','',NAME_1_IPUMS), NAME_1_RAW=plain_ascii(NAME_1_RAW),
         GID_1_IPUMS_ID=paste0(GID_0,"_",GID_1_IPUMS))


census$NAME_0[census$NAME_0 == "Benin"] <- "benin"
census$NAME_0[census$NAME_0 == "Botswana"] <- "botswana"
census$NAME_0[census$NAME_0 == "Cameroon"] <- "cameroon"
census$NAME_0[census$NAME_0 == "Ethiopia"] <- "ethiopia"
census$NAME_0[census$NAME_0 == "Ghana"] <- "ghana"
census$NAME_0[census$NAME_0 == "Guinea"] <- "guinea"
census$NAME_0[census$NAME_0 == "Kenya"] <- "kenya"
census$NAME_0[census$NAME_0 == "Lesotho"] <- "lesotho"
census$NAME_0[census$NAME_0 == "Liberia"] <- "liberia"
census$NAME_0[census$NAME_0 == "Malawi"] <- "malawi"
census$NAME_0[census$NAME_0 == "Mali"] <- "mali"
census$NAME_0[census$NAME_0 == "Mauritius"] <- "mauritius"
census$NAME_0[census$NAME_0 == "Mozambique"] <- "mozambique"
census$NAME_0[census$NAME_0 == "Nigeria"] <- "nigeria"
census$NAME_0[census$NAME_0 == "Rwanda"] <- "rwanda"
census$NAME_0[census$NAME_0 == "Senegal"] <- "senegal"
census$NAME_0[census$NAME_0 == "Sudan"] <- "sudan"
census$NAME_0[census$NAME_0 == "Tanzania"] <- "tanzania"
census$NAME_0[census$NAME_0 == "Togo"] <- "togo"
census$NAME_0[census$NAME_0 == "Uganda"] <- "uganda"
census$NAME_0[census$NAME_0 == "Zambia"] <- "zambia"
census$NAME_0[census$NAME_0 == "Zimbabwe"] <- "zimbabwe"

census$NAME_0[census$NAME_0 == "Burkina Faso"] <- "burkina_faso"
census$NAME_0[census$NAME_0 == "Sierra Leone"] <- "sierra_leone"
census$NAME_0[census$NAME_0 == "South Africa"] <- "south_africa"
census$NAME_0[census$NAME_0 == "South Sudan"] <- "south_sudan"
table(census$NAME_0, useNA = "always")
table(census$GID_0, useNA = "always")


# Generate a variable for sex of HH member
table(census$sex, useNA = "always")
census$female[census$sex==1] <- 0
census$female[census$sex==2] <- 1
table(census$female, useNA = "always")


# Generate a dummy if person is married or living with a partner vs. never been in union, widowed, divorced or seperated
census$married <- NA
census$married[census$marst == 1 | census$marst == 3 | census$marst == 4] <- 0
census$married[census$marst == 2] <- 1
table(census$married, useNA = "always")


# Generate a variable for age categories
table(census$age)
census$age_cat <- NA
census$age_cat[census$age>=0 & census$age<= 14] <- "0-14"
census$age_cat[census$age>=15 & census$age<= 24] <- "15-24"
census$age_cat[census$age>=25 & census$age<= 29] <- "25-29"
census$age_cat[census$age>=30 & census$age<= 34] <- "30-34"
census$age_cat[census$age>=35 & census$age<= 39] <- "35-39"
census$age_cat[census$age>=40 & census$age<= 44] <- "40-44"
census$age_cat[census$age>=45 & census$age<= 49] <- "45-49"
census$age_cat[census$age>=50 & census$age<= 54] <- "50-54"
census$age_cat[census$age>=55 & census$age<= 59] <- "55-59"
census$age_cat[census$age>=60 & is.na(census$age)] <- "59+"
table(census$age_cat, useNA = "always")
census <- census[ which(census$age>=15 & census$age<=59),]


# Generate variable for education levels
table(census$edattain)
census$education <- NA 
census$education[census$edattain == 0 | census$edattain == 1] <- 0
census$education[census$edattain == 2] <- 1
census$education[census$edattain == 3] <- 2
census$education[census$edattain == 4] <- 3	
table(census$education, useNA = "always")
census$education <- factor(census$education,
                         levels = c(0,1,2,3),
                         labels = c("no_edu", "primary_edu", "secondary_edu", "tertiary_edu")) 


#generate variable for kid at home
table(census$nchild, useNA = "always")
census$kidsathome <- NA
census$kidsathome[census$nchild > 0] <- 1
census$kidsathome[census$nchild == 0] <- 0
table(census$kidsathome, useNA = "always")


# Generate variable for work vs. no work (occupation not possibe)
table(census$empstat)
census$working <- NA
census$working[census$empstat==0 | census$empstat==2 | census$empstat==3 |census$empstat==4] <- 0
census$working[census$empstat==1] <- 1
table(census$working, useNA = "always")


# Type of place of residence
census$rural <- NA
census$rural[census$urban == 2] <- 0
census$rural[census$urban == 1] <- 1
table(census$rural, useNA = "always")
census$rural[census$NAME_0=="botswana"] <- 1
census$rural[census$NAME_0=="zambia"] <- 1


# Has electricity
census$electricity <- NA
census$electricity[census$electric == 2] <- 0
census$electricity[census$electric == 1] <- 1
table(census$electricity, useNA = "always")
census$electricity[census$NAME_0=="guinea"] <- 0
census$electricity[census$NAME_0=="mauritius"] <- 0


# Has toilet
census$toilet_old <- census$toilet
census$toilet <- NA
census$toilet[census$toilet_old != 10] <- 1
census$toilet[census$toilet_old == 10] <- 0
table(census$toilet, useNA = "always")
census$toilet[census$NAME_0=="guinea"] <- 0
census$toilet[census$NAME_0=="mauritius"] <- 0
census$toilet[census$NAME_0=="kenya"] <- 0


# Phone availability  (DHS question: Landline telephone)
census$tel <- NA
census$tel[census$phone == 1] <- 1
census$tel[census$phone == 2] <- 0
table(census$tel, useNA = "always")
census$tel[census$NAME_0=="guinea"] <- 0
census$tel[census$NAME_0=="mauritius"] <- 0
census$tel[census$NAME_0=="mali"] <- 0
census$tel[census$NAME_0=="cameroon"] <- 0
census$tel[census$NAME_0=="liberia"] <- 0
census$tel[census$NAME_0=="zimbabwe"] <- 0



census <- census[,c("age_cat", "education", "female",
              "working", "rural", "married", "kidsathome", "electricity", "toilet", "tel",
              "country", "year","sample", "serial", "NAME_0", "GID_1_IPUMS", "NAME_1_IPUMS", "GID_1_IPUMS_ID", "GID_0", "geolev1", "geolev2")]
summary(census)

census$geolev1[is.na(census$geolev1)] <- 0
census$geolev2[is.na(census$geolev2)] <- 0


cli::cli_alert_info("Creating dummies...")
census <- dummy_cols(census, select_columns = c("age_cat", "education", "NAME_0"))
cli::cli_alert_info("Creating dummies DONE.")
myvars <- names(census) %in% c("age_cat", "education")
census <- census[!myvars]

census <- na.omit(census)
census$ID <- 1:nrow(census)


census$female <- as.factor(census$female)
census$working <- as.factor(census$working)
census$rural <- as.factor(census$rural)
census$married <- as.factor(census$married)
census$kidsathome <- as.factor(census$kidsathome)
census$electricity <- as.factor(census$electricity)
census$toilet <- as.factor(census$toilet)
census$tel <- as.factor(census$tel)
names(census)[names(census) == "age_cat_15-24"] <- "age_cat_15_24"
names(census)[names(census) == "age_cat_25-29"] <- "age_cat_25_29"
names(census)[names(census) == "age_cat_30-34"] <- "age_cat_30_34"
names(census)[names(census) == "age_cat_35-39"] <- "age_cat_35_39"
names(census)[names(census) == "age_cat_40-44"] <- "age_cat_40_44"
names(census)[names(census) == "age_cat_45-49"] <- "age_cat_45_49"
names(census)[names(census) == "age_cat_50-54"] <- "age_cat_50_54"
names(census)[names(census) == "age_cat_55-59"] <- "age_cat_55_59"
#census$age_cat_15_24 <- as.factor(census$age_cat_15_24)
#census$age_cat_25_29 <- as.factor(census$age_cat_25_29)
#census$age_cat_30_34 <- as.factor(census$age_cat_30_34)
##census$age_cat_35_39 <- as.factor(census$age_cat_35_39)
#census$age_cat_40_44 <- as.factor(census$age_cat_40_44)
#census$age_cat_45_49 <- as.factor(census$age_cat_45_49)
#census$age_cat_50_54 <- as.factor(census$age_cat_50_54)
#census$age_cat_55_59 <- as.factor(census$age_cat_55_59)
#census$education_no_edu <- as.factor(census$education_no_edu)
#census$education_primary_edu <- as.factor(census$education_primary_edu)
#census$education_secondary_edu <- as.factor(census$education_secondary_edu)
#census$education_tertiary_edu <- as.factor(census$education_tertiary_edu)

census$NAME_0_other <- 0
table(census$NAME_0, useNA = "always")
table(census$GID_0, useNA = "always")

save(census, file="DataRaw/Census/Census_clean_female_15_49.RData")

cli::cli_alert_info("Save census data DONE.")
