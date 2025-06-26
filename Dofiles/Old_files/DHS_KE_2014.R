#############################################################################################################
#  
#  Description: DHS Kenya
#  Author:      Kathrin Durizzo               
#  Date:        16.03.2023
#
#############################################################################################################

#------------------------------------------------------------------------------------------------------------
#
#                                         Setup
#
#------------------------------------------------------------------------------------------------------------
options(warn = -1)

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("MASS")
pkgTest("ROCR")
#pkgTest("pROC")
pkgTest("data.table")
pkgTest("sandwich")
#pkgTest("stargazer")
pkgTest("foreign")
pkgTest("haven")
pkgTest("caret")
pkgTest("glmnet")
pkgTest("plyr")
pkgTest("randomForest")
pkgTest("questionr")
pkgTest("survey")
pkgTest("fastDummies")

getwd()
setwd("X:/research/Data/PhDs/Kathrin2023/MLPaper/DataRaw/DHS/KE_2014_DHS_04142020_951_17084")


data_IR <- read_dta("KEIR72DT/KEIR72FL.DTA")
data_IR$merge001 <- data_IR$v001
data_IR$merge002 <- data_IR$v002
data_IR$merge003 <- data_IR$v003

data_MR <- read_dta("KEMR72DT/KEMR72FL.DTA")
data_MR$merge001 <- data_MR$mv001
data_MR$merge002 <- data_MR$mv002
data_MR$merge003 <- data_MR$mv003

data_PR <- read_dta("KEPR72DT/KEPR72FL.DTA")
data_PR$merge001 <- data_PR$hv001
data_PR$merge002 <- data_PR$hv002
data_PR$merge003 <- data_PR$hvidx


data <- merge(data_PR, data_MR, by = c("merge001", "merge002", "merge003"), all=TRUE)
data <- merge(data, data_IR, by = c("merge001", "merge002", "merge003"), all=TRUE)


#Sample Weight
data$hv_wt = data$hv005 / 1000000

#generate HHID
data$HHID <- paste(data$merge001, data$merge002) 

#generate year
data$year <- "2017_2018"

rm("data_IR", "data_MR", "data_PR")


# Generate a variable for number of HH members
data$member <- 1
data$member_all_HH <- ave(data$member, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
table(data$member_all_HH)
data$member_young[data$hv105<=14] <- 1
data$member_young_HH <- ave(data$member_young, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
table(data$member_young_HH)
data$member_old[data$hv105>59] <- 1
data$member_old_HH <- ave(data$member_old, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
table(data$member_old_HH)

# Drop observations below 15 and above 59
# Generate a variable for age categories
data$age <- data$hv105
data$age_cat <- NA
data$age_cat[data$age>=0 & data$age<= 14] <- "0-14"
data$age_cat[data$age>=15 & data$age<= 24] <- "15-24"
data$age_cat[data$age>=25 & data$age<= 29] <- "25-29"
data$age_cat[data$age>=30 & data$age<= 34] <- "30-34"
data$age_cat[data$age>=35 & data$age<= 39] <- "35-39"
data$age_cat[data$age>=40 & data$age<= 44] <- "40-44"
data$age_cat[data$age>=45 & data$age<= 49] <- "45-49"
data$age_cat[data$age>=50 & data$age<= 54] <- "50-54"
data$age_cat[data$age>=55 & data$age<= 59] <- "55-59"
data$age_cat[data$age>=60 & is.na(data$age)] <- "59+"
table(data$age_cat)

# members 15-59
data$member_HH <- ave(data$member, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
summary(data$member_HH)

summary(data$member_all_HH)
summary(data$member_young_HH)
summary(data$member_old_HH)

data <- data[ which(data$age>=15 & data$age<=49),]

#------------------------------------------------------------------------------------------------------------
#
# II. Creates insurance variables
#
#------------------------------------------------------------------------------------------------------------
# Insurance
data$insurance <- data$mv481
data$insurance[is.na(data$insurance)] <- data$v481[is.na(data$insurance)]
data$insurance[data$insurance==9] <- NA
table(data$insurance, useNA = "always")
wtd.mean(data$insurance, weights = data$hv_wt)

data$insurance_HH <- ave(data$insurance, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$insurance_HH[data$insurance_HH!= 0 & data$insurance_HH!=1] <- NA
table(data$insurance_HH, useNA = "always")
wtd.mean(data$insurance_HH, weights = data$hv_wt)


#------------------------------------------------------------------------------------------------------------
#
# III. Creates control variables DHS
#
#------------------------------------------------------------------------------------------------------------
# Generate a variable for sex of HH member
data$female[data$hv104==1] <- 0
data$female[data$hv104==2] <- 1
#data$sex <- factor(data$sex,
#                    levels = c(0,1),
#                    labels = c("male", "female")) 
table(data$female, useNA = "always")
wtd.mean(data$female, weights = data$hv_wt)
data$female_HH <- ave(data$female, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
data$female_HH <- data$female_HH / data$member_HH
table(data$female_HH, useNA = "always")
wtd.mean(data$female_HH, weights = data$hv_wt)


# age
wtd.table(data$age_cat, weights = data$hv_wt)
wtd.mean(data$age, weights = data$hv_wt)


# Generate a dummy if person is married or living with a partner vs. never been in union, widowed, divorced or seperated
data$married <- NA
data$married[data$hv115 >= 2 & data$hv115<=4] <- 0
data$married[data$hv115 == 0] <- 0
data$married[data$hv115 == 1] <- 1
data$married[data$v501 == 0 & is.na(data$married)] <- 0
data$married[data$v501 >= 3 & data$v501 <= 5 & is.na(data$married)] <- 0
data$married[data$v501 >= 1 & data$v501 <= 2 & is.na(data$married)] <- 1
data$married[data$mv501 == 0 & is.na(data$married)] <- 0
data$married[data$mv501 >= 3 & data$mv501 <= 5 & is.na(data$married)] <- 0
data$married[data$v501 >= 1 & data$mv501 <= 2 & is.na(data$married)] <- 1
#data$married <- factor(data$married,
#                   levels = c(0,1),
#                   labels = c("not_married", "married")) 
table(data$married, useNA = "always")
wtd.mean(data$married, weights = data$hv_wt)
data$married_HH <- ave(data$married, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$married_HH[data$married_HH!= 0 & data$married_HH!=1] <- NA
table(data$married_HH, useNA = "always")
wtd.mean(data$married_HH, weights = data$hv_wt)


# Generate variable for education levels
data$education <- NA 
data$education[data$hv106 == 0] <- 0
data$education[data$hv106 == 1] <- 1
data$education[data$hv106 == 2] <- 2
data$education[data$hv106 == 3] <- 3	
summary(data$education)
data$education_HH <- ave(data$education, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$education_HH[data$education_HH!= 0 & data$education_HH!=1 & data$education_HH!=2 & data$education_HH!=3] <- NA
summary(data$education_HH)
data$education <- factor(data$education,
                         levels = c(0,1,2,3),
                         labels = c("no_edu", "primary_edu", "secondary_edu", "tertiary_edu")) 
data$education_HH <- factor(data$education_HH,
                            levels = c(0,1,2,3),
                            labels = c("no_edu", "primary_edu", "secondary_edu", "tertiary_edu")) 
table(data$education, useNA = "always")
wtd.table(data$education, weights = data$hv_wt)
table(data$education_HH, useNA = "always")
wtd.table(data$education_HH, weights = data$hv_wt)


# Generate variable for occupation 
data$occupation <- NA
data$occupation <- data$mv717
data$occupation[is.na(data$occupation)] <- data$v717

# Generate variable for work vs. no work
data$working <- NA
data$working[data$occupation==0 | data$occupation==6] <- 0
data$working[data$occupation>=1 & data$occupation<=5] <- 1
data$working[data$occupation>=7 & data$occupation<=8] <- 1
table(data$working, useNA = "always")
wtd.mean(data$working, weights = data$hv_wt)
# Note: many NA!

data$working_HH <- ave(data$working, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$working_HH[data$working_HH!= 0 & data$working_HH!=1] <- NA
table(data$working_HH, useNA = "always")
wtd.mean(data$working_HH, weights = data$hv_wt)
# Note: many NA!

data$occupation <- factor(data$occupation,
                          levels = c(0,1,2,3,4,5,6,7,8,9,98),
                          labels = c("Not_working" ,
                                     "Professional" ,
                                     "Clerical"  ,
                                     "Sales" ,
                                     "Agricultural_self" ,
                                     "Agricultural" ,
                                     "Domestic" ,
                                     "Services" ,
                                     "Skilled_manual" ,
                                     "Unskilled_manual",
                                     "Other"))
table(data$occupation, useNA = "always")
wtd.table(data$occupation, weights = data$hv_wt)
# Note: many NA!

# Type of place of residence
data$rural <- NA
data$rural[data$hv025 == 1] <- 0
data$rural[data$hv025 == 2] <- 1
#data$rural <- factor(data$rural,
#                          levels = c(0,1),
#                          labels = c("Urban", "Rural"))
table(data$rural, useNA = "always")
wtd.mean(data$rural, weights = data$hv_wt)


# Region
data$region <- data$hv024 
data$region <- factor(data$region,
                      levels = c(1:8),
                      labels = c("coast" ,
                                 "north eastern" ,
                                 "eastern",
                                 "central",
                                 "rift valley",
                                 "western",
                                 "nyanza",
                                 "nairobi"))
table(data$region, useNA = "always")
wtd.table(data$region, weights = data$hv_wt)


# Sex of Household head
data$sex_HH_head <- NA
data$sex_HH_head[data$hv219 == 1] <- 0 
data$sex_HH_head[data$hv219 == 2] <- 1
#data$sex_HH_head <- factor(data$sex_HH_head,
#                     levels = c(0,1),
#                     labels = c("male", "female" ))
table(data$sex_HH_head, useNA = "always")
wtd.mean(data$sex_HH_head, weights = data$hv_wt)


# Wealth index
data$wealthindex <- NA
data$wealthindex <- data$hv270
data$wealthindex <- factor(data$wealthindex,
                           levels = c(1,2,3,4,5),
                           labels = c("1_Quintile" ,
                                      "2_Quintile",
                                      "3_Quintile",
                                      "4_Quintile",
                                      "5_Quintile"))
table(data$wealthindex, useNA = "always")
wtd.mean(data$wealthindex, weights = data$hv_wt)


# Exposure to mass media  at least once a week newspaper, radio or TV
data$media <- 0
data$media[data$v157>=2| data$v158>=2| data$v159>=2] <- 1
data$media[is.na(data$v157)& is.na(data$mv157)] <- NA
data$media[data$mv157>=2| data$mv158>=2| data$mv159>=2] <- 1
data$media_hh <- ave(data$media, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$media_hh[data$media_hh!=0 & data$media_hh!=1] <- NA
#data$media_hh <- factor(data$media_hh,
#                           levels = c(0,1),
#                           labels = c("no consumption mass media in HH","consumption mass media in HH" ))
table(data$media_hh, useNA = "always")
wtd.mean(data$media_hh, weights = data$hv_wt)
wtd.mean(data$media, weights = data$hv_wt)
#SOME NA!


# Has TV
data$tv <- data$hv208
data$tv[data$tv == 9] <- NA
#data$tv <- factor(data$tv,
#                      levels = c(0,1),
#                      labels = c("no tv", "tv" ))
table(data$tv, useNA = "always")
wtd.mean(data$tv, weights = data$hv_wt)


# Has mobile telephone
data$mobile <- data$hv243a
data$mobile[data$mobile == 9] <- NA
#data$mobile <- factor(data$mobile,
#                           levels = c(0,1),
#                           labels = c("no mobile phone", "mobile phone" ))
table(data$mobile, useNA = "always")
wtd.mean(data$mobile, weights = data$hv_wt)


# Has bicycle
data$bicycle <- data$hv210
data$bicycle[data$bicycle == 9] <- NA
#data$bicycle <- factor(data$bicycle,
#                            levels = c(0,1),
#                            labels = c("no bicycle", "bicycle" ))
table(data$bicycle, useNA = "always")
wtd.mean(data$bicycle, weights = data$hv_wt)


# Has motorcycle
data$motorcycle <- data$hv211
data$motorcycle[data$motorcycle == 9] <- NA
#data$motorcycle <- factor(data$motorcycle,
#                       levels = c(0,1),
#                       labels = c("no motorcycle", "motorcycle" ))
table(data$motorcycle, useNA = "always")
wtd.mean(data$motorcycle, weights = data$hv_wt)


# Has car
data$car <- data$hv212
data$car[data$car == 9] <- NA
#data$car <- factor(data$car,
#                      levels = c(0,1),
#                      labels = c("no car", "car" ))
table(data$car, useNA = "always")
wtd.mean(data$car, weights = data$hv_wt)


# Mosquitonets
data$mosquitonet <- data$hv227
data$mosquitonet[data$mosquitonet == 9] <- NA
#data$mosquitonet <- factor(data$mosquitonet,
#                        levels = c(0,1),
#                        labels = c("no mosquitonet","mosquitonet" ))
table(data$mosquitonet, useNA = "always")
wtd.mean(data$mosquitonet, weights = data$hv_wt)


# Has Bank account
data$bankaccount <- data$hv247 
data$bankaccount[data$bankaccount == 9] <- NA
data$bankaccount[data$bankaccount == 8] <- NA
#data$bankaccount <- factor(data$bankaccount,
#                     levels = c(0,1),
#                     labels = c("no bankaccount", "bankaccount" ))
table(data$bankaccount, useNA = "always")
wtd.mean(data$bankaccount, weights = data$hv_wt)


# Create a variable for traditional religion (vodoun, autre traditionnelles)
data$trad_religion <- 0
data$trad_religion[is.na(data$mv130) & is.na(data$v130) ] <- NA
data$trad_religion[data$mv130==1|data$mv130==2|data$v130==1|data$v130==2] <- 1
table(data$trad_religion, useNA = "always")
wtd.mean(data$trad_religion, weights = data$hv_wt)
data$trad_religion_hh <- ave(data$trad_religion, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$trad_religion_hh[data$trad_religion_hh!=0 & data$trad_religion_hh!=1] <- NA
table(data$trad_religion_hh, useNA = "always")
wtd.mean(data$trad_religion_hh, weights = data$hv_wt)
# SOME NA


# Create variable for fertility preference
data$fertility[!is.na(data$v602)] <- 0
data$fertility[data$v602 ==1] <- 1
data$fertility_hh <- ave(data$fertility, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$fertility_hh[data$fertility_hh!=0 & data$fertility_hh!=1] <- NA
table(data$fertility_hh, useNA = "always")
wtd.mean(data$fertility_hh, weights = data$hv_wt)
data$fertility <-0
data$fertility_hh <-0
# SOME NA



#------------------------------------------------------------------------------------------------------------
#
# VI. health outcomes
#
#------------------------------------------------------------------------------------------------------------

# Visited HF in last 12 months
data$visitHF12m <- data$v394 
data$visitHF12m_hh <- ave(data$visitHF12m, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$visitHF12m_hh[data$visitHF12m_hh!=0 & data$visitHF12m_hh!=1] <- NA 
#data$visitHF12m_hh <- factor(data$visitHF12m_hh,
#                       levels = c(0,1),
#                       labels = c("No visit in last 12 months of HH", "at least one visit in last 12 months of HH"))
table(data$visitHF12m_hh, useNA = "always")
wtd.mean(data$visitHF12m_hh, weights = data$hv_wt)
# SOME NA!


# Location birth of last child
data$locationBirth <- NA
data$locationBirth[data$m15_1 == 11 | data$m15_1 == 12] <- 0 
data$locationBirth[data$m15_1>= 13 & data$m15_1<=96] <- 1
data$locationBirth_hh <- ave(data$locationBirth, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$locationBirth_hh[data$locationBirth_hh!=0 & data$locationBirth_hh!=1] <- NA
#data$locationBirth_hh <- factor(data$locationBirth_hh,
#                             levels = c(0,1),
#                             labels = c("last birth of HH not in health facility","last birth of HH in health facility" ))
table(data$locationBirth_hh, useNA = "always")
wtd.mean(data$locationBirth_hh, weights = data$hv_wt)
#MANY NA!


# Last brith C section
data$csection <- data$v401
data$csection[data$csection == 9] <- NA
data$csection_hh <- ave(data$csection, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$csection_hh[data$csection_hh!=0 & data$csection_hh!=1] <- NA
#data$csection_hh <- factor(data$csection_hh,
#                                 levels = c(0,1),
#                                 labels = c("last born in HH was without csection", "last born in HH was with csection" ))
table(data$csection_hh, useNA = "always")
wtd.mean(data$csection_hh, weights = data$hv_wt)
# MANY NA!



# Postnatal Check wihtin 2 months for last born
data$postnatalcheck <- NA
data$postnatalcheck[data$m70_1==1] <- 1
data$postnatalcheck[data$m70_1==0] <- 0
data$postnatalcheck_hh <- ave(data$postnatalcheck, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$postnatalcheck_hh[data$postnatalcheck_hh!=0 & data$postnatalcheck_hh!=1] <- NA
#data$postnatalcheck_hh <- factor(data$postnatalcheck_hh,
#                                levels = c(0,1),
#                                labels = c("last born in HH has not received postnatal check","last born in HH has received postnatal check" ))
table(data$postnatalcheck_hh, useNA = "always")
wtd.mean(data$postnatalcheck_hh, weights = data$hv_wt)
# MANY NA!

#kidsathome
data$kidsathome <- NA
data$kidsathome[data$member_young_HH == 0] <- 0
data$kidsathome[data$member_young_HH != 0] <- 1
table(data$kidsathome, useNA = "always")


#electricity
data$electricity <- NA
data$electricity[data$hv206 == 0] <- 0
data$electricity[data$hv206 == 1] <- 1
table(data$electricity, useNA = "always")


#toilet
table(data$hv205)
data$toilet <- NA
data$toilet[data$hv205 == 31] <- 0
data$toilet[data$hv205 != 31] <- 1
table(data$toilet, useNA = "always")

# Has landline telephone
data$tel <- data$hv221
data$tel[data$tel == 9] <- NA
table(data$tel, useNA = "always")

data$NAME_1 <- "Kenya"

data$country <- "KENYA_2014"

data <- data[ which(data$female==1),]

df <- data[,c("insurance", "age_cat", "education",
              "working", "rural", "married", "kidsathome", "electricity", "toilet", "tel",
              "visitHF12m", "locationBirth",  "postnatalcheck", "csection",
              "hv001","hv_wt", "hv022", "hv021", "country", "NAME_1", "region",
              "sex_HH_head", "wealthindex", "media", "tv", "mobile", "bicycle", "motorcycle", 
              "car", "mosquitonet", "bankaccount", "trad_religion_hh", "fertility_hh")]

summary(df)

#------------------------------------------------------------------------------------------------------------
#
# V. Add Geolocation
#
#------------------------------------------------------------------------------------------------------------
df_geolocation <- read_dta("X:/research/Data/PhDs/Kathrin2023/MLPaper/Data/Geolocation/KEGE71FL_data.DTA")
df_geolocation$hv001 <- df_geolocation$DHSCLUST
df_geolocation <- df_geolocation [,c("hv001", "LATNUM", "LONGNUM")]


#------------------------------------------------------------------------------------------------------------
#
# VI. Save
#
#------------------------------------------------------------------------------------------------------------
df <- dummy_cols(df, select_columns = c("age_cat", "education", "wealthindex"))
myvars <- names(df) %in% c("age_cat", "education", "wealthindex") 
df <- df[!myvars]

names(df)[names(df) == "NAME_1"] <- "NAME_0"
df$NAME_0[df$NAME_0 == "Kenya"] <- "kenya"

df <- merge(df, df_geolocation, by = c("hv001"), all=TRUE)


save(df, file="X:/research/Data/PhDs/Kathrin2023/MLPaper/Data/Kenya_2014.RData")

table(df$insurance)
summary(df$insurance)
wtd.mean(data$insurance, weights = data$hv_wt)
