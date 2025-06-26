#############################################################################################################
#  
#  Description: DHS Benin 2017 2018
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
pkgTest("haven")  # read_dta
pkgTest("caret")
pkgTest("glmnet")
pkgTest("plyr")
pkgTest("randomForest")
pkgTest("questionr")
pkgTest("survey")
pkgTest("fastDummies")

getwd()
setwd("X:/research_assistants/Pepe/MLPaper/DataRaw/DHS/BJ_2017-18_DHS_04142020_937_17084")

#Merge Data
data_IR <- read_dta("BJIR71DT/BJIR71FL.DTA")
data_IR$merge001 <- data_IR$v001
data_IR$merge002 <- data_IR$v002
data_IR$merge003 <- data_IR$v003

data_MR <- read_dta("BJMR71DT/BJMR71FL.DTA")
data_MR$merge001 <- data_MR$mv001
data_MR$merge002 <- data_MR$mv002
data_MR$merge003 <- data_MR$mv003

data_PR <- read_dta("BJPR71DT/BJPR71FL.DTA")
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



#------------------------------------------------------------------------------------------------------------
#
# I. Creates control variables
#
#------------------------------------------------------------------------------------------------------------

############## Generate variables from HH survey ############################

# Household members
data$member_all_HH <- data$hv009
data$member_all_HH <- scale(data$member_all_HH)
summary(data$member_all_HH, useNA = "always")

data$member_women_HH <- data$hv010
data$member_women_HH <- scale(data$member_women_HH)
summary(data$member_women_HH, useNA = "always")

data$member_men_HH <- data$hv011
data$member_men_HH <- scale(data$member_men_HH)
summary(data$member_men_HH, useNA = "always")

data$member_all_HH <- data$hv009
data$member_all_HH <- scale(data$member_all_HH)
summary(data$member_all_HH, useNA = "always")

data$member_child_under_5 <- data$hv014
data$member_child_under_5 <- scale(data$member_child_under_5)
summary(data$member_child_under_5, useNA = "always")

# Region
data$region <- data$hv024 
data$region <- factor(data$region,
                      levels = c(1:12),
                      labels = c("Alibori" ,
                                 "Atacora" ,
                                 "Atlantic"  ,
                                 "Borgou"  ,
                                 "Collines"  ,
                                 "Couffo" ,
                                 "Donga"  ,
                                 "Littoral"  ,
                                 "Mono" ,
                                 "Oueme"  ,
                                 "Plateau" ,
                                 "Zou"  ))
table(data$region, useNA = "always")

# Type of place of residence
data$rural <- NA
data$rural[data$hv025 == 1] <- 0
data$rural[data$hv025 == 2] <- 1
table(data$rural, useNA = "always")

# "Source of drinking water"
data$safe_drinking_water <- 0
data$safe_drinking_water[data$hv201==11 |data$hv201== 12 |
                           data$hv201==13 |data$hv201== 14 |
                           data$hv201==31 |data$hv201== 41 |
                           data$hv201==71 |data$hv201== 72 ] <- 1
# c("piped into dwelling", "piped to yard/plot", "piped to neighbor","public tap/standpipe", 
# "tube well or borehole", "protected well", "protected spring","bottled water", "water sachets")
table(data$safe_drinking_water, useNA = "always")

# water not available for at least a day last two weeks 
data$water_interrupted <- 0
data$water_interrupted[data$hv201a==1] <- 1
table(data$water_interrupted, useNA = "always")

# type of toilet facility
data$safe_toilet_facility <- 0
data$safe_toilet_facility[data$hv205==11 |data$hv205== 12 |
                           data$hv205==21 |data$hv205==22|
                           data$hv205== 41 ] <- 1
# c("flush to piped sewer system", "flush to septic tank", "ventilated improved pit latrine (vip)", 
# "pit latrine with slab", "composting toilet")
table(data$safe_toilet_facility, useNA = "always")

# electricity
data$electricity <- 0
data$electricity[data$hv206 == 1] <- 1
table(data$electricity, useNA = "always")

# radio
data$radio <- 0
data$radio[data$hv207 == 1] <- 1
table(data$radio, useNA = "always")

# television
data$television <- 0
data$television[data$hv208 == 1] <- 1
table(data$television, useNA = "always")

# refrigerator
data$refrigerator <- 0
data$refrigerator[data$hv209 == 1] <- 1
table(data$refrigerator, useNA = "always")

# bicycle
data$bicycle <- 0
data$bicycle[data$hv210 == 1] <- 1
table(data$bicycle, useNA = "always")

# motorcycle
data$motorcycle <- 0
data$motorcycle[data$hv211 == 1] <- 1
table(data$motorcycle, useNA = "always")

# car
data$car <- 0
data$car[data$hv212 == 1] <- 1
table(data$car, useNA = "always")

# safe_floor_material
data$safe_floor_material <- 0
data$safe_floor_material[data$hv213 >= 31 &data$hv213 <= 35] <- 1
# c("parquet, polished wood", "vinyl, asphalt strips", "ceramic tiles", "cement", "carpet"), 1, 0)
table(data$safe_floor_material, useNA = "always")

# Sex of Household head
data$female_HH_head <- NA
data$female_HH_head[data$hv219 == 1] <- 0 
data$female_HH_head[data$hv219 == 2] <- 1
#data$female_HH_head <- factor(data$female_HH_head,
#                     levels = c(0,1),
#                     labels = c("male", "female" ))
table(data$female_HH_head, useNA = "always")

# Age of Household head
data$age_HH_head <- data$hv220
data$age_HH_head <- scale(data$age_HH_head)
table(data$age_HH_head, useNA = "always")

# Has landline telephone
data$tel <- data$hv221
data$tel[data$tel == 9] <- NA
table(data$tel, useNA = "always")

# toilet
data$toilet <- 0
data$toilet[data$hv205 != 31] <- 1
table(data$toilet, useNA = "always")

# share toilet with other households
data$share_toilet <- 0
data$share_toilet[data$hv225 == 1] <- 1
table(data$share_toilet, useNA = "always")

# safe_cooking_fuel
data$safe_cooking_fuel <- 0
data$safe_cooking_fuel[data$hv226 >= 1 &data$hv226 <= 4] <- 1
# c("electricity", "lpg", "natural gas", "biogas")
table(data$safe_cooking_fuel, useNA = "always")

# Mosquitonets
data$mosquitonet <- data$hv227
table(data$mosquitonet, useNA = "always")

# All children sleep under mosquitonets
data$child_mosquitonet <- 0
data$child_mosquitonet[data$hv228==1] <- 1
table(data$child_mosquitonet, useNA = "always")

# soap present at place for hand washing
data$soap <- 0
data$soap[data$hv232==1] <- 1
table(data$soap, useNA = "always")

# Has mobile telephone
data$mobile <- data$hv243a
table(data$mobile, useNA = "always")

# Has watch
data$watch <- data$hv243b
table(data$watch, useNA = "always")

# Has cart
data$cart <- data$hv243c
table(data$cart, useNA = "always")

# Has boat
data$boat <- data$hv243d
table(data$boat, useNA = "always")

# Has fr
data$computer <- data$hv243e
table(data$computer, useNA = "always")

# owns land usable for agriculture
data$owns_land <- data$hv244
table(data$owns_land, useNA = "always")

# owns livestock,herds or farm animals
data$owns_livestock <- data$hv246
table(data$owns_livestock, useNA = "always")

# Has Bank account
data$bankaccount <- data$hv247 
table(data$bankaccount, useNA = "always")

# household members smoke inside the house
data$smoke_inside <- 0
data$smoke_inside[data$hv252 != 0] <- 1
table(data$smoke_inside, useNA = "always")

# Wealth index
data$wealthindex <- data$hv270
data$wealthindex <- factor(data$wealthindex,
                           levels = c(1,2,3,4,5),
                           labels = c("1_Quintile" ,
                                      "2_Quintile",
                                      "3_Quintile",
                                      "4_Quintile",
                                      "5_Quintile"))
table(data$wealthindex, useNA = "always")

# number of mosquitonets
data$mosquitonets_nr <- data$hml1
data$mosquitonets_nr <- scale(data$mosquitonets_nr)
table(data$mosquitonets_nr, useNA = "always")

# Has generator
data$generator <- data$sh121g 
table(data$generator, useNA = "always")

# Has music_system
data$music_system <- data$sh121h
table(data$music_system, useNA = "always")

# Has oven
data$oven <- data$sh121i
table(data$oven, useNA = "always")

# Has dvd_player
data$dvd_player <- data$sh121j 
table(data$dvd_player, useNA = "always")

# Has video_recorder
data$video_recorder <- data$sh121k
table(data$video_recorder, useNA = "always")

# Has internet_connection
data$internet_connection <- data$sh121l 
table(data$internet_connection, useNA = "always")

# Has washing_machine
data$washing_machine <- data$sh121m 
table(data$washing_machine, useNA = "always")

# Has canoe
data$canoe <- data$sh122h
table(data$canoe, useNA = "always")

# Has credit_union
data$credit_union <- data$sh123a
table(data$credit_union, useNA = "always")

# safe trash disposal
data$safe_trash_disposal <- 0
data$safe_trash_disposal[data$sh144a >= 1 &data$sh144a <= 2] <- 1
# c("public dump", "private or ngo dump")
table(data$safe_trash_disposal, useNA = "always")

# safe sewag_disposal
data$safe_sewag_disposal <- 0
data$safe_sewag_disposal[data$sh144b == 3 | data$sh144b == 5] <- 1
# c("septic tank", "sewers")
table(data$safe_sewag_disposal, useNA = "always")

# child involved in child_labor
data$child_labor[data$sh159>=1] <- 1
data$child_labor_HH <- ave(data$child_labor, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
data$child_labor_HH[data$child_labor_HH >=1] <- 1
table(data$child_labor_HH, useNA = "always")

# child involved in child labor: carrying heavy loads
data$child_labor_carry[data$sh165==1] <- 1
data$child_labor_carry_HH <- ave(data$child_labor_carry, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
data$child_labor_carry_HH[data$child_labor_carry_HH >=1] <- 1
table(data$child_labor_carry_HH, useNA = "always")

# child involved in child labor: unsafe work
data$child_labor_unsafe[data$sh166==1] <- 1
data$child_labor_unsafe_HH <- ave(data$child_labor_unsafe, data$HHID, FUN= function(x) sum(x, na.rm = TRUE))
data$child_labor_unsafe_HH[data$child_labor_unsafe_HH >=1] <- 1
table(data$child_labor_unsafe_HH, useNA = "always")

# Thinks physical punishment needed
data$physical_punishment <- 0
data$physical_punishment[data$sh184==1] <- 1   
table(data$physical_punishment, useNA = "always")

# Has slept last night
data$slept <- data$hv103 
table(data$slept, useNA = "always")

# sex of HH member
data$female[data$hv104==1] <- 0
data$female[data$hv104==2] <- 1
table(data$female, useNA = "always")

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
table(data$age_cat, useNA = "always")
#@Pepe: changed 0-14 to 0_14 etc. due to error causing in model

# Generate variable for education levels
data$education <- NA 
data$education[data$hv106 == 0] <- 0
data$education[data$hv106 == 1] <- 1
data$education[data$hv106 == 2] <- 2
data$education[data$hv106 == 3] <- 3	
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
table(data$education_HH, useNA = "always")

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

data$married_HH <- ave(data$married, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$married_HH[data$married_HH!= 0 & data$married_HH!=1] <- NA
table(data$married_HH, useNA = "always")

# migratory status
data$migrant <- 0
data$migrant[data$sh11g!=1] <-1
table(data$migrant, useNA = "always")

# literate
data$literate<- 0
data$literate[data$sh20a!=4]<- 1
table(data$literate, useNA = "always")


############## Generate variables from women survey ############################

# domestic violence
data$domestic_violence <- NA
data$domestic_violence[data$v044!=1] <- 0
data$domestic_violence[data$v044==1] <- 1
table(data$domestic_violence, useNA = "always")

# Create a variable for traditional religion (vodoun, autre traditionnelles)
data$trad_religion <- 0
data$trad_religion[is.na(data$mv130) & is.na(data$v130) ] <- NA
data$trad_religion[data$mv130==1|data$mv130==2|data$v130==1|data$v130==2] <- 1
table(data$trad_religion, useNA = "always")

data$trad_religion_HH <- ave(data$trad_religion, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$trad_religion_HH[data$trad_religion_HH!=0 & data$trad_religion_HH!=1] <- NA
table(data$trad_religion_HH, useNA = "always")

# frequency of reading newspaper or magazine 
data$read_newspaper <- NA
data$read_newspaper[data$v157==0] <- 0
data$read_newspaper[data$v157!=0] <- 1
table(data$read_newspaper, useNA = "always")

# frequency of listening to radio
data$listen_radio <- NA
data$listen_radio[data$v158==0] <- 0
data$listen_radio[data$v158!=0] <- 1
table(data$listen_radio, useNA = "always")

# frequency of watching television 
data$watch_TV <- NA
data$watch_TV[data$v157==0] <- 0
data$watch_TV[data$v157!=0] <- 1
table(data$watch_TV, useNA = "always")

# Exposure to mass media  at least once a week newspaper, radio or TV
data$media <- 0
data$media[data$v157>=2| data$v158>=2| data$v159>=2] <- 1
data$media[is.na(data$v157)& is.na(data$mv157)] <- NA
data$media[data$mv157>=2| data$mv158>=2| data$mv159>=2] <- 1
table(data$media, useNA = "always")

data$media_HH <- ave(data$media, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$media_HH[data$media_HH!=0 & data$media_HH!=1] <- NA
#data$media_HH <- factor(data$media_HH,
#                           levels = c(0,1),
#                           labels = c("no consumption mass media in HH","consumption mass media in HH" ))
table(data$media_HH, useNA = "always")

# Total children ever born
data$child_born <- NA
data$child_born[data$v201==0] <- 0
data$child_born[data$v201!=0] <- 1
table(data$child_born, useNA = "always")

# sons and daughters at home
data$sondaughterhome <- data$v202+data$v203
data$sondaughterhome[data$sondaughterhome>=1] <- 1
table(data$sondaughterhome, useNA = "always")

# sons and daughters died
data$sondaughterdied <- data$v206+data$v207
data$sondaughterdied[data$sondaughterdied>=1] <- 1
table(data$sondaughterdied, useNA = "always")

# pregnant
data$pregnant<- 0
data$pregnant[data$ha54==1]<- 1
data$pregnant[data$v213==1]<- 1
table(data$pregnant, useNA = "always")

# ever had a terminated pregnancy
data$terminated_pregnancy <- NA
data$terminated_pregnancy[data$v228==0] <- 0
data$terminated_pregnancy[data$v228==1] <- 1
table(data$terminated_pregnancy, useNA = "always")

# birth in last three years
data$birth_last_3y <- NA
data$birth_last_3y[data$v238==0] <- 0
data$birth_last_3y[data$v238!=0] <- 1
table(data$birth_last_3y, useNA = "always")

# knows modern contraceptive methods
data$knows_mordern_method <- NA
data$knows_mordern_method[data$v301!=3] <- 0
data$knows_mordern_method[data$v301==3] <- 1
table(data$knows_mordern_method, useNA = "always")

# contraceptive with health care access required
data$contraceptive_health_care <- NA
data$contraceptive_health_care[data$v312>=0] <- 0
data$contraceptive_health_care[data$v312==1|data$v312==2|data$v312==3|
                                 data$v312==6|data$v312==11|data$v312==16] <- 1
#c("pill", "iud", "injections", "female sterilization", "implants/norplant", "emergency contraception"), 1, 0)
table(data$contraceptive_health_care, useNA = "always")

# smokes any cigarette or tobacco
data$smokes <- NA
data$smokes[data$v463z==0] <- 1
data$smokes[data$v463z==1] <- 0
table(data$smokes, useNA = "always")

# Create variable for fertility preference
data$fertility[!is.na(data$v602)] <- 0
data$fertility[data$v602 ==1] <- 1
data$fertility_HH <- ave(data$fertility, data$HHID, FUN= function(x) max(x, na.rm = TRUE))
data$fertility_HH[data$fertility_HH!=0 & data$fertility_HH!=1] <- NA
table(data$fertility_HH, useNA = "always")

# respondent currently working
data$currently_working <- data$v714
table(data$currently_working , useNA = "always")

# person who usually decides how to spend respondent's earnings
data$female_spend_earnings <- NA
data$female_spend_earnings[data$v739!=1] <- 0
data$female_spend_earnings[data$v739==1] <- 1
table(data$female_spend_earnings, useNA = "always")

# person who usually decides how to spend respondent's earnings
data$female_spend_health_care <- NA
data$female_spend_health_care[data$v743a!=1] <- 0
data$female_spend_health_care[data$v743a==1] <- 1
table(data$female_spend_health_care, useNA = "always")

# person ever heard of STI
data$heard_sti <- data$v750
table(data$heard_sti, useNA = "always")

# ever tested for HIV
data$tested_HIV <- data$v781
table(data$tested_HIV, useNA = "always")



#------------------------------------------------------------------------------------------------------------
#
# II. Creates outcome variables
#
#------------------------------------------------------------------------------------------------------------
# Insurance
data$insurance <- data$mv481
data$insurance[is.na(data$insurance)] <- data$v481[is.na(data$insurance)]
table(data$insurance, useNA = "always")

# Visited HF in last 12 months
data$visitHF12m <- data$v394 
table(data$visitHF12m, useNA = "always")

# Location birth of last child
data$locationBirth <- NA
data$locationBirth[data$m15_1 == 11 | data$m15_1 == 12] <- 0 
data$locationBirth[data$m15_1>= 13 & data$m15_1<=96] <- 1
table(data$locationBirth, useNA = "always")

# Last brith C section
data$csection <- data$v401
table(data$csection, useNA = "always")

# Postnatal Check wihtin 2 months for last born
data$postnatalcheck <- NA
data$postnatalcheck[data$m70_1==1] <- 1
data$postnatalcheck[data$m70_1==0] <- 0
table(data$postnatalcheck, useNA = "always")


#------------------------------------------------------------------------------------------------------------
#
# III. Drop observation and saves
#
#------------------------------------------------------------------------------------------------------------

# Drop observations below 15 and above 59
data <- data[ which(data$age>=15 & data$age<=49),]

# Only look at females
data <- data[ which(data$female==1),]

# Add some DHS specific information for later merging with GEO data
data$NAME_1 <- "Benin"
data$country <- "BENIN_2017_18"

# Make a sub sample of only relevant variables
df <- data[,c( "hv001","hv_wt", "hv022", "hv021", "country", "NAME_1", "region",
               "insurance", "age_cat", "education",
              "currently_working", "rural", "married", "sondaughterhome", "electricity", "toilet", "tel",
              "visitHF12m", "locationBirth",  "postnatalcheck", "csection",
              "female_HH_head", "wealthindex", "media", "television", "mobile", "bicycle", "motorcycle", 
              "car", "child_mosquitonet", "bankaccount", "trad_religion_HH", "fertility_HH",
              "member_all_HH", "member_women_HH", "member_men_HH", "member_child_under_5", 
              "safe_drinking_water", "water_interrupted", "safe_toilet_facility",
              "radio", "refrigerator", "safe_floor_material", "age_HH_head", 
              "share_toilet", "safe_cooking_fuel", "mosquitonet", 
              "soap", "watch", "cart", "boat", "computer", "owns_land", 
              "owns_livestock", "smoke_inside", 
              "mosquitonets_nr", "generator", "music_system", "oven", "dvd_player", 
              "video_recorder", "internet_connection", "washing_machine", "canoe", 
              "credit_union", "safe_trash_disposal", "safe_sewag_disposal", "child_labor_HH", 
              "child_labor_carry_HH", "child_labor_unsafe_HH", "physical_punishment", 
              "slept", "female", "education_HH", "married_HH", "migrant", 
              "literate", "domestic_violence", "trad_religion", 
              "read_newspaper", "listen_radio", "watch_TV", "media_HH", "child_born",
              "sondaughterdied", "pregnant", "terminated_pregnancy", 
              "birth_last_3y", "knows_mordern_method", "contraceptive_health_care", 
              "smokes", "fertility", "female_spend_earnings", "female_spend_health_care",
              "heard_sti", "tested_HIV", "year")]
# summary(df)

#------------------------------------------------------------------------------------------------------------
#
# V. Add Geolocation
#
#------------------------------------------------------------------------------------------------------------
df_geolocation <- read_dta("X:/research/Data/PhDs/Kathrin2023/MLPaper/Data/Geolocation/BJGE71FL_data.DTA")
df_geolocation$hv001 <- df_geolocation$DHSCLUST
df_geolocation <- df_geolocation [,c("hv001", "LATNUM", "LONGNUM")]


#------------------------------------------------------------------------------------------------------------
#
# VI. Save
#
#------------------------------------------------------------------------------------------------------------

# generate dummies for categorical variables
df <- dummy_cols(df, select_columns = c("age_cat", "education","education_HH", "wealthindex"))
myvars <- names(df) %in% c("age_cat", "education","education_HH", "wealthindex") 
df <- df[!myvars]

# rename to be able to later merge with census
names(df)[names(df) == "NAME_1"] <- "NAME_0"
df$NAME_0[df$NAME_0 == "Benin"] <- "benin"

# merge with geolocation
df <- merge(df, df_geolocation, by = c("hv001"), all=TRUE)

# save
save(df, file="X:/research_assistants/Pepe/MLPaper/Data/Benin_2017_18.RData")

table(df$insurance)
summary(df$insurance)
wtd.mean(data$insurance, weights = data$hv_wt)
