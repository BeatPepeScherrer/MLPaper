#############################################################################################################
#  
#'  Description:  This file utilizes functions from functions.R to create a final 
#'                dataset from the DHS data for 31 countries.
#'  Author:       Beat Scherrer             
#'  Date:         17.07.2024
#
#############################################################################################################

# 0. Setup ----

# Packages
library(dplyr)
library(openxlsx)
library(fastDummies)
library(cli)
library(purrr)
library(naniar)
library(stats)
library(readxl)

# Sourcing scripts
source("Dofiles/dhs_functions.R")

# 1. Create variables and lists with function inputs (paths, region names, etc) ----
# Create a vector of the folder names of all countries
country_folders <- list.files(path = "DataRaw/DHS")

countries <- c("Benin","Burkina.Faso","Burundi","Cameroon", "Chad",
               "DRC","Republic.of.the.Congo","Cote.dIvoire","Eswatini","Ethiopia","The.Gambia",
               "Ghana","Guinea","Kenya","Lesotho","Liberia","Madagascar", "Malawi",
               "Mali", "Mauritania", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal","South.Africa",
               "Sierra.Leone", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")
outcome_vars <- c("insurance","locationBirth","postnatalcheck", "visitHF12m")

# Create a list containing all countries, input paths, and region names
country_list <- list(
  Burkina.Faso = list(country = "Burkina.Faso", country_code = "BF", IR_path = paste0(country_folders[1],"/BFIR62DT/BFIR62FL.DTA"),
                      MR_path = paste0(country_folders[1],"/BFMR62DT/BFMR62FL.DTA"), PR_path = paste0(country_folders[1],"/BFPR62DT/BFPR62FL.DTA")),
  Benin = list(country = "Benin", country_code = "BJ", IR_path = paste0(country_folders[2],"/BJIR71DT/BJIR71FL.DTA"),
               MR_path = paste0(country_folders[2],"/BJMR71DT/BJMR71FL.DTA"), PR_path = paste0(country_folders[2],"/BJPR71DT/BJPR71FL.DTA")),
  Burundi = list(country = "Burundi", country_code = "BU", IR_path = paste0(country_folders[3],"/BUIR70DT/BUIR70FL.DTA"),
                 MR_path = paste0(country_folders[3],"/BUMR70DT/BUMR70FL.DTA"), PR_path = paste0(country_folders[3],"/BUPR70DT/BUPR70FL.DTA")),
  Cameroon = list(country = "Cameroon", country_code = "CM", IR_path = paste0(country_folders[4],"/2018/CMIR71FL.DTA"),
                  MR_path = paste0(country_folders[4],"/2018/CMMR71FL.DTA"), PR_path = paste0(country_folders[4],"/2018/CMPR71FL.DTA")),
  DRC = list(country = "DRC", country_code = "CD", IR_path = paste0(country_folders[5],"/CDIR61DT/CDIR61FL.DTA"),
             MR_path = paste0(country_folders[5],"/CDMR61DT/CDMR61FL.DTA"), PR_path = paste0(country_folders[5],"/CDPR61DT/CDPR61FL.DTA")),
  Republic.of.the.Congo = list(country = "Republic.of.the.Congo", country_code = "CIG", IR_path = paste0(country_folders[6],"/CGIR61DT/CGIR61FL.DTA"),
                               MR_path = paste0(country_folders[6],"/CGMR61DT/CGMR61FL.DTA"), PR_path = paste0(country_folders[6],"/CGPR61DT/CGPR61FL.DTA")),
  Cote.dIvoire = list(country = "Cote.dIvoire", country_code = "CI", IR_path = paste0(country_folders[7],"/CIIR62DT/CIIR62FL.DTA"),
                      MR_path = paste0(country_folders[7],"/CIMR62DT/CIMR62FL.DTA"), PR_path = paste0(country_folders[7],"/CIPR62DT/CIPR62FL.DTA")),
  Eswatini = list(country = "Eswatini", country_code = "SZ", IR_path = paste0(country_folders[8],"/2007/szir51fl.dta"),
                  MR_path = paste0(country_folders[8],"/2007/szmr51fl.dta"), PR_path = paste0(country_folders[8],"/2007/SZPR52FL.DTA")),
  Ethiopia = list(country = "Ethiopia", country_code = "ET", IR_path = paste0(country_folders[9],"/ETIR71DT/ETIR71FL.DTA"),
                  MR_path = paste0(country_folders[9],"/ETMR71DT/ETMR71FL.DTA"), PR_path = paste0(country_folders[9],"/ETPR71DT/ETPR71FL.DTA")),
  The.Gambia = list(country = "The.Gambia", country_code = "GM", IR_path = paste0(country_folders[10],"/2019/GMIR81FL.DTA"),
                MR_path = paste0(country_folders[10],"/2019/GMMR81FL.DTA"), PR_path = paste0(country_folders[10],"/2019/GMPR81FL.DTA")),
  Ghana = list(country = "Ghana", country_code = "GH", IR_path = paste0(country_folders[11],"/GHIR72DT/GHIR72FL.DTA"),
               MR_path = paste0(country_folders[11],"/GHMR71DT/GHMR71FL.DTA"), PR_path = paste0(country_folders[11],"/GHPR72DT/GHPR72FL.DTA")),
  Guinea = list(country = "Guinea", country_code = "GN", IR_path = paste0(country_folders[12],"/GNIR71DT/GNIR71FL.DTA"),
                MR_path = paste0(country_folders[12],"/GNMR71DT/GNMR71FL.DTA"), PR_path = paste0(country_folders[12],"/GNPR71DT/GNPR71FL.DTA")),
  Kenya = list(country = "Kenya", country_code = "KE", IR_path = paste0(country_folders[13],"/KEIR72DT/KEIR72FL.DTA"),
               MR_path = paste0(country_folders[13],"/KEMR72DT/KEMR72FL.DTA"), PR_path = paste0(country_folders[13],"/KEPR72DT/KEPR72FL.DTA")),
  Comoros = list(country = "Comoros", country_code = "KM", IR_path = paste0(country_folders[14],"/KMIR61DT/KMIR61FL.DTA"),
                 MR_path = paste0(country_folders[14],"/KMMR61DT/KMMR61FL.DTA"), PR_path = paste0(country_folders[14],"/KMPR61DT/KMPR61FL.DTA")),
  Liberia = list(country = "Liberia", country_code = "LB", IR_path = paste0(country_folders[15],"/2019/LBIR7AFL.DTA"),
                 MR_path = paste0(country_folders[15],"/2019/LBMR7AFL.DTA"), PR_path = paste0(country_folders[15],"/2019/LBPR7AFL.DTA")),
  Lesotho = list(country = "Lesotho", country_code = "LS", IR_path = paste0(country_folders[16],"/LSIR71DT/LSIR71FL.DTA"),
                 MR_path = paste0(country_folders[16],"/LSMR71DT/LSMR71FL.DTA"), PR_path = paste0(country_folders[16],"/LSPR71DT/LSPR71FL.DTA")),
  Madagascar = list(country = "Madagascar", country_code = "MD", IR_path = paste0(country_folders[17],"/2021/MDIR80FL.DTA"),
                    MR_path = paste0(country_folders[17],"/2021/MDMR80FL.DTA"), PR_path = paste0(country_folders[17],"/2021/MDPR80FL.DTA")),
  Mauritania = list(country = "Mauritania", country_code = "MR", IR_path = paste0(country_folders[18],"/2021/MRIR71FL.DTA"),
                    MR_path = paste0(country_folders[18],"/2021/MRMR71FL.DTA"), PR_path = paste0(country_folders[18],"/2021/MRPR71FL.DTA")),
  Mali = list(country = "Mali", country_code = "ML", IR_path = paste0(country_folders[19],"/MLIR7HDT/MLIR7HFL.DTA"),
              MR_path = paste0(country_folders[19],"/MLMR7HDT/MLMR7HFL.DTA"), PR_path = paste0(country_folders[19],"/MLPR7HDT/MLPR7HFL.DTA")),
  Malawi = list(country = "Malawi", country_code = "MW", IR_path = paste0(country_folders[20],"/MWIR7ADT/MWIR7AFL.DTA"),
                MR_path = paste0(country_folders[20],"/MWMR7ADT/MWMR7AFL.DTA"), PR_path = paste0(country_folders[20],"/MWPR7ADT/MWPR7AFL.DTA")),
  Nigeria = list(country = "Nigeria", country_code = "NG", IR_path = paste0(country_folders[21],"/NGIR7ADT/NGIR7AFL.DTA"),
                 MR_path = paste0(country_folders[21],"/NGMR7ADT/NGMR7AFL.DTA"), PR_path = paste0(country_folders[21],"/NGPR7ADT/NGPR7AFL.DTA")),
  Niger = list(country = "Niger", country_code = "NI", IR_path = paste0(country_folders[22],"/NIIR61DT/NIIR61FL.DTA"),
               MR_path = paste0(country_folders[22],"/NIMR61DT/NIMR61FL.DTA"), PR_path = paste0(country_folders[22],"/NIPR61DT/NIPR61FL.DTA")),
  Namibia = list(country = "Namibia", country_code = "NM", IR_path = paste0(country_folders[23],"/NMIR61DT/NMIR61FL.DTA"),
                 MR_path = paste0(country_folders[23],"/NMMR61DT/NMMR61FL.DTA"), PR_path = paste0(country_folders[23],"/NMPR61DT/NMPR61FL.DTA")),
  Rwanda = list(country = "Rwanda", country_code = "RW", IR_path = paste0(country_folders[24],"/2020/RWIR81FL.DTA"),
                MR_path = paste0(country_folders[24],"/2020/RWMR81FL.DTA"), PR_path = paste0(country_folders[24],"/2020/RWPR81FL.DTA")),
  Sao.Tome = list(country = "Sao.Tome", country_code = "ST", IR_path = paste0(country_folders[25],"/2009/STIR50FL.DTA"),
                  MR_path = paste0(country_folders[25],"/2009/STMR50FL.DTA"), PR_path = paste0(country_folders[25],"/2009/STPR50FL.DTA")),
  Sierra.Leone = list(country = "Sierra.Leone", country_code = "SL", IR_path = paste0(country_folders[26],"/2019/SLIR7AFL.DTA"),
                      MR_path = paste0(country_folders[26],"/2019/SLMR7AFL.DTA"), PR_path = paste0(country_folders[26],"/2019/SLPR7AFL.DTA")),
  Senegal = list(country = "Senegal", country_code = "SN", IR_path = paste0(country_folders[27],"/SNIR7IDT/SNIR7IFL.DTA"),
                 MR_path = paste0(country_folders[27],"/SNMR7IDT/SNMR7IFL.DTA"), PR_path = paste0(country_folders[27],"/SNPR7IDT/SNPR7IFL.DTA")),
  South.Africa = list(country = "South.Africa", country_code = "ZA", IR_path = paste0(country_folders[28],"/2016/ZAIR71FL.DTA"),
                      MR_path = paste0(country_folders[28],"/2016/ZAMR71FL.DTA"), PR_path = paste0(country_folders[28],"/2016/ZAPR71FL.DTA")),
  Chad = list(country = "Chad", country_code = "TD", IR_path = paste0(country_folders[29],"/TDIR71DT/TDIR71FL.DTA"),
              MR_path = paste0(country_folders[29],"/TDMR71DT/TDMR71FL.DTA"), PR_path = paste0(country_folders[29],"/TDPR71DT/TDPR71FL.DTA")),
  Togo = list(country = "Togo", country_code = "TG", IR_path = paste0(country_folders[30],"/TGIR61DT/TGIR61FL.DTA"),
              MR_path = paste0(country_folders[30],"/TGMR61DT/TGMR61FL.DTA"), PR_path = paste0(country_folders[30],"/TGPR61DT/TGPR61FL.DTA")),
  Tanzania = list(country = "Tanzania", country_code = "TZ", IR_path = paste0(country_folders[31],"/TZIR7BDT/TZIR7BFL.DTA"),
                  MR_path = paste0(country_folders[31],"/TZMR7BDT/TZMR7BFL.DTA"), PR_path = paste0(country_folders[31],"/TZPR7BDT/TZPR7BFL.DTA")),
  Uganda = list(country = "Uganda", country_code = "UG", IR_path = paste0(country_folders[32],"/UGIR7BDT/UGIR7BFL.DTA"),
                MR_path = paste0(country_folders[32],"/UGMR7BDT/UGMR7BFL.DTA"), PR_path = paste0(country_folders[32],"/UGPR7BDT/UGPR7BFL.DTA")),
  Zambia = list(country = "Zambia", country_code = "ZM", IR_path = paste0(country_folders[33],"/ZMIR71DT/ZMIR71FL.DTA"),
                MR_path = paste0(country_folders[33],"/ZMMR71DT/ZMMR71FL.DTA"), PR_path = paste0(country_folders[33],"/ZMPR71DT/ZMPR71FL.DTA")),
  Zimbabwe = list(country = "Zimbabwe", country_code = "ZW", IR_path = paste0(country_folders[34],"/ZWIR72DT/ZWIR72FL.DTA"),
                  MR_path = paste0(country_folders[34],"/ZWMR72DT/ZWMR72FL.DTA"), PR_path = paste0(country_folders[34],"/ZWPR72DT/ZWPR72FL.DTA"))
)
country_codes <- NA
for (i in 1:length(countries)) {
  country_codes[i] <- country_list[[countries[i]]][["country_code"]]
}

# Create list containing values for binary recoding
recode_list <- list(
  list(old_name = "hv025", new_name = "rural", true_values = c(2)), # Type of place of residence
  list(old_name = "hv201", new_name = "drinking_water", true_values = c(11,12,13,14,31,41,71,72)), #Source of drinking water 
  list(old_name = "hv201a", new_name = "water_interrupted", true_values = c(1), na_values = c(8)), #Water not available for at least a day last two weeks
  list(old_name = "hv205", new_name = "safe_toilet_facility", true_values = c(11,12,21,22,41)), #Type of toilet facility
  list(old_name = "hv206", new_name = "electricity", true_values = c(1)),
  list(old_name = "hv207", new_name = "radio", true_values = c(1)),
  # list(old_name = "hv208", new_name = "television", true_values = c(1)),
  list(old_name = "hv209", new_name = "refrigerator", true_values = c(1)),
  list(old_name = "hv210", new_name = "bicycle", true_values = c(1)),
  list(old_name = "hv211", new_name = "motorcycle", true_values = c(1)),
  list(old_name = "hv212", new_name = "car", true_values = c(1)),
  list(old_name = "hv213", new_name = "safe_floor_material", true_values = c(31,32,33,34,35)), #Main floor material
  list(old_name = "hv219", new_name = "female_HH_head", true_values = c(2)),   # Sex of Household head
  list(old_name = "hv205", new_name = "toilet", true_values = c(11,12,13,14,15,21,22,23,41,42,43,96)), # has a toilet
  list(old_name = "hv225", new_name = "share_toilet", true_values = c(1)), ## share toilet with other households
  list(old_name = "hv226", new_name = "safe_cooking_fuel", true_values = c(1,2,3,4)),# safe_cooking_fuel
  # list(old_name = "hv227", new_name = "mosquitonet", true_values = c(1)), # mosquito nets
  list(old_name = "hv228", new_name = "child_mosquitonet", true_values = c(1)), # All children sleep under mosquitonets
  list(old_name = "hv232", new_name = "soap", true_values = c(1,2,3,4)), # soap present at place for hand washing
  list(old_name = "hv243a", new_name = "mobile", true_values = c(1)), # Has mobile telephone
  list(old_name = "hv243b", new_name = "watch", true_values = c(1)),
  list(old_name = "hv243c", new_name = "cart", true_values = c(1)),
  list(old_name = "hv243d", new_name = "boat", true_values = c(1)),
  list(old_name = "hv243e", new_name = "computer", true_values = c(1)),
  list(old_name = "hv244", new_name = "owns_land", true_values = c(1)), #Owns land usable for agriculture
  list(old_name = "hv246", new_name = "owns_livestock", true_values = c(1)), # owns livestock,herds or farm animals
  list(old_name = "hv247", new_name = "bankaccount", true_values = c(1)), 
  list(old_name = "hv252", new_name = "smoke_inside", true_values = c(1,2,3,4), na_values = c(9)), # household members smoke inside the house
  list(old_name = "sh121g", new_name = "generator", true_values = c(1)),
  list(old_name = "sh121h", new_name = "music_system", true_values = c(1)),
  list(old_name = "sh121i", new_name = "oven", true_values = c(1)),
  list(old_name = "sh121j", new_name = "dvd_player", true_values = c(1)),
  list(old_name = "sh121k", new_name = "video_recorder", true_values = c(1)),
  list(old_name = "sh121l", new_name = "internet_connection", true_values = c(1)),
  list(old_name = "sh121m", new_name = "washing_machine", true_values = c(1)),
  list(old_name = "sh122h", new_name = "canoe", true_values = c(1)),
  list(old_name = "sh123a", new_name = "credit_union", true_values = c(1)),
  list(old_name = "sh144a", new_name = "safe_trash_disposal", true_values = c(1,2)), # c("public dump", "private or ngo dump")
  list(old_name = "sh144b", new_name = "safe_sewage_disposal", true_values = c(3,4)), # c("septic tank", "sewers")
  list(old_name = "sh184", new_name = "physical_punishment", true_values = c(1)), # Thinks physical punishment needed
  list(old_name = "hv104", new_name = "female", true_values = c(2)), # Sex of household member
  list(old_name = "sh11g", new_name = "migrant", true_values = c(2,3)),
  list(old_name = "v155", old_name_two = "mv155", new_name = "literate", true_values = c(2), na_values = c(3,4)), ###!!!### Check with Kathrin if i can leave my altered coding or switch to old one!
  list(old_name = "hv201", new_name = "safe_drinking_water", true_values = c(11,12,13,14,31,41,71,72)),
  list(old_name = "hv221", new_name = "tel", true_values = c(1), na_values = c(9)),
  list(old_name = "hv230b", new_name = "water_handwash_present", true_values = c(1), na_values = c(9)), # Presence of water at hand washing place
  list(old_name = "hv234a", new_name = "iodine", true_values = c(1), na_values = c(6,9)), # salt in HH contains iodine
  list(old_name = "hv237", new_name = "water_treated", true_values = c(1), na_values = c(8,9)), # Anything done to water to make safe to drink
  # list(old_name = "sh136aa", new_name = "malaria_mosquito", true_values = c(1), na_values = c(9)), # How can contract malaria : mosquito bite
  # list(old_name = "sh136ab", new_name = "malaria_insects", true_values = c(1), na_values = c(9)), # How can contract malaria : other insects
  # list(old_name = "sh136ac", new_name = "malaria_transfusion", true_values = c(1), na_values = c(9)), # How can contract malaria : blood transfusion
  # list(old_name = "sh136ad", new_name = "malaria_mother", true_values = c(1), na_values = c(9)), # How can contract malaria : mother to child
  # list(old_name = "sh136ae", new_name = "malaria_water", true_values = c(1), na_values = c(9)), # How can contract malaria : drinking water
  # list(old_name = "sh136af", new_name = "malaria_sorcery", true_values = c(1), na_values = c(9)), # How can contract malaria : sorcery
  list(old_name = "sh273", new_name = "beat_child", true_values = c(1), na_values = c(8,9)), # Child needs to be physically punished to be brought up properly
  list(old_name = "sh162a", old_name_two = "sh162b", old_name_three = "sh162c", old_name_four = "sh162d", new_name = "child_labor", true_values = c(1), na_values = c(9)), # Child engages in any work activity
  list(old_name = "sh165", new_name = "Child_labor_carry", true_values = c(1), na_values = c(9)), # Child labor Activities require carrying heavy loads
  list(old_name = "sh166", new_name = "Child_labor_unsafe", true_values = c(1), na_values = c(9)), # Child labor Activities require working with unsafe tools
  ### Data from women's survey ###
  list(old_name = "v157", new_name = "read_newspaper", true_values = c(1,2)), 
  list(old_name = "v158", new_name = "listen_radio", true_values = c(1,2)), # frequency of listening to radio
  list(old_name = "v159", new_name = "watch_TV", true_values = c(1)), # frequency of watching television
  list(old_name = "v157", old_name_two = "v158",
       old_name_three = "v159",old_name_four = "mv157",
       old_name_five = "mv158",old_name_six = "mv159",new_name = "media", true_values = c(2)), # Exposure to mass media  at least once a week newspaper, radio or TV
  # list(old_name = "v201", new_name = "child_born", true_values = c(1:16)), # Total children ever born
  list(old_name = "ha54", old_name_two = "v213", new_name = "pregnant", true_values = c(1)),
  list(old_name = "v228", new_name = "terminated_pregnancy", true_values = c(1)),
  list(old_name = "v238", new_name = "birth_last_3y", true_values = c(1:20)), # birth in last three years
  list(old_name = "v301", new_name = "knows_mordern_method", true_values = c(3)),
  list(old_name = "v312", new_name = "contraceptive_health_care", true_values = c(1,2,3,6,11,16)), # contraceptive with health care access required
  list(old_name = "v463z", new_name = "smokes", true_values = c(0)),
  list(old_name = "v602", new_name = "fertility", true_values = c(1)), # fertility preference, 1 if woman wants to have another child
  list(old_name = "v714", new_name = "currently_working", true_values = c(1)),
  list(old_name = "v739", new_name = "female_spend_earnings", true_values = c(1)),
  # list(old_name = "v743a", new_name = "female_spend_health_care", true_values = c(1)),
  list(old_name = "v750", new_name = "heard_sti", true_values = c(1)),
  list(old_name = "v781", new_name = "tested_HIV", true_values = c(1)),
  list(old_name = "hv115",old_name_two = "v501", old_name_three = "mv501", new_name = "married", true_values = c(1,2)), # Generate a dummy if person is married or living with a partner vs. never been in union, widowed, divorced or seperated
  list(old_name = "v743a", new_name = "decides_health_care", true_values = c(1,2), na_values = c(6)), # Person involved in decision on respondent's health care
  list(old_name = "v743b", new_name = "decides_purchases", true_values = c(1,2), na_values = c(6)), # Person involved in decision on respondent's household purchases
  list(old_name = "v823", new_name = "HIV_witchcraft", true_values = c(1), na_values = c(6)), # Person believes she can get HIV by witchcraft/supernatural means
  list(old_name = "v206", old_name_two = "v207", new_name = "sondaughterdied", true_values = c(1:10)), # Either son or daughter has died
  # list(old_name = "v202", old_name_two = "v203", new_name = "sondaughterhome", true_values = c(1:10)), # Either son or daughter is still at home
  list(old_name = "v326", new_name = "safe_source_contracept", true_values = c(11:25), na_values =c(9)), # safe source of contraceptive method
  list(old_name = "v384a", old_name_two = "v384b", old_name_three = "v384c", new_name = "heard_family_planning", true_values = c(1), na_values =c(9)), # heard of family planning via any media in last few months
  # list(old_name = "m1a_1", old_name_two = "m1_1", new_name = "tetanus", true_values = c(1:7), na_values =c(8,9)), # Has received tetanus injection before birth or pregnancy
  # list(old_name = "m50_1", new_name = "checkup_delivery", true_values = c(1), na_values = c(9)), # Respondent's check up after delivery
  # list(old_name = "m60_1", new_name = "drugs_parasites", true_values = c(1), na_values = c(8,9)), # Drugs for intestinal parasites during pregnancy
  list(old_name = "v474", new_name = "heard_tuberculosis", true_values = c(1), na_values = c(8,9)), # Ever heard of tuberculosis
  # list(old_name = "v477", new_name = "injected_tuberculosis", true_values = c(1:90), na_values = c(99)), # Had a tuberculosis-related injection last 12 months
  list(old_name = "h1_1", new_name = "health_card", true_values = c(1,2), na_values = c(9)), # Has health card for child (to record vaccinations)
  list(old_name = "h10_1", new_name = "vaccinated", true_values = c(1), na_values = c(8,9)), # Whether child Ever had vaccination
  list(old_name = "v613", new_name = "non_numeric_fertility_preference", true_values = c(96), na_values = c(99)), # Whether woman gave a non_numeric response for ideal number of children
  list(old_name = "d106", old_name_two = "d107", old_name_three = "d108", new_name = "dom_violence", true_values = c(1), na_values = c(9)), # Women has experienced less severe, severe or sexual violence
  ### Outcome variables ###
  list(old_name = "mv481", old_name_two = "v481", new_name = "insurance", true_values = c(1), na_values = c(9)),
  list(old_name = "v394", new_name = "visitHF12m", true_values = c(1)), # Visited HF in last 12 months
  list(old_name = "m15_1", new_name = "locationBirth", true_values = c(13:96)), # Location birth of last child
  list(old_name = "m70_1", new_name = "postnatalcheck", true_values = c(1), na_values = c(8)) # Postnatal Check within 2 months for last born
)

# Create list containing values for standardizing numericals
list_scale <- list(
  list(old_name = "hv009", new_name = "member_all_HH"),
  list(old_name = "hv010", new_name = "member_women_HH"),
  list(old_name = "hv011", new_name = "member_men_HH"),
  list(old_name = "hv014", new_name = "member_child_under_5"),
  list(old_name = "hml1", new_name = "mosquitonets_nr"),
  list(old_name = "hv245", new_name = "hectares"),
  list(old_name = "hv246a", new_name = "cattle"),
  list(old_name = "ha40", new_name = "bmi"),
  # list(old_name = "v456", new_name = "hemoglobin"), # hemeglobin level in g/dl (adjusted for altitude/smoking)
  list(old_name = "hv204", new_name = "water_time"), # Time to get to water source (minutes)
  # list(old_name = "v167", new_name = "trips"), # Number of trips in last 12 months
  list(old_name = "v613", new_name = "ideal_nr_children"), # Ideal number of children
  list(old_name = "sh164", new_name = "child_labor_hours") # Number of hours worked in child labor activities
)

# Call the function to create the final data sets for a given country
for (outcome_var in outcome_vars) {
  # Define output path
  output_path <- paste0("L:/research_assistants/Pepe/MLPaper/New_data_sets/",outcome_var,"/")
  for (country in countries) {
    # create final data sets for model estimation
    do.call(run_countries, c(country_list[[country]], list(outcome_var = outcome_var, output_path = output_path)))
  }
}
