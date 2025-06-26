#############################################################################################################
#  
#  Description: Model
#
#  Authors:     Kathrin Durizzo               
#  Date:        16.03.2021
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


getwd()
setwd("X:/research_assistants/Pepe/MLPaper/Data/old")

# read in cleaned DHS data
load("Country_merged.RData")
# in the country_merged data I merged all country datasets with their geocode (to know the GPS coordinates per DHS cluster) 
# and also all countries (you can also leave the countries separate - as you like). Here an example of merging geocode with BJ

#df_geolocation <- read_dta("X:/research_assistants/Pepe/MLPaper/Data/Geolocation/BJGE71FL_data.DTA")
#df_geolocation$hv001 <- df_geolocation$DHSCLUST
#df_geolocation <- df_geolocation [,c("hv001", "LATNUM", "LONGNUM")]
#df <- merge(df, df_geolocation, by = c("hv001"), all=TRUE)

# thus to run the following code with our data 
# 1. we need to add the cluster identifier hv001 to merge "LATNUM", "LONGNUM" to the data
# 2. we need to save the outcome predictions, e.g. as insurance_prediction_xgboost_long_model etc. in this code, I just make a mock example of the numbers
df$insurance_prediction_xgboost_long_model <- df$insurance 
names(df)[names(df) == "age_cat_15-24"] <- "age_cat_15_24"
names(df)[names(df) == "age_cat_25-29"] <- "age_cat_25_29"
names(df)[names(df) == "age_cat_30-34"] <- "age_cat_30_34"
names(df)[names(df) == "age_cat_35-39"] <- "age_cat_35_39"
names(df)[names(df) == "age_cat_40-44"] <- "age_cat_40_44"
names(df)[names(df) == "age_cat_45-49"] <- "age_cat_45_49"
names(df)[names(df) == "age_cat_50-54"] <- "age_cat_50_54"
names(df)[names(df) == "age_cat_55-59"] <- "age_cat_55_59"

df$insurance <- as.factor(df$insurance)
df$working <- as.factor(df$working)
df$rural <- as.factor(df$rural)
df$married <- as.factor(df$married)
df$kidsathome <- as.factor(df$kidsathome)
df$electricity <- as.factor(df$electricity)
df$toilet <- as.factor(df$toilet)


# read in GADM data
mADM<- readRDS(file = "X:/research_assistants/Pepe/MLPaper/DataRaw/GADM/mADM.rds")


# transfrom for overlaying DHS and GADM
coordinates(df) <- df[c("LONGNUM", "LATNUM")]
mADMdata <- st_as_sf(mADM)
df <- st_as_sf(df)
mADMdata <- st_transform(mADMdata, 4269)
df <- st_set_crs(df, 4269)

shp <- mADM
shp_df <- tidy(shp)
shp$id <- row.names(shp)
shp_df <- left_join(shp_df, shp@data, by="id")


# import Census and create additional empty additional variables
load("X:/research_assistants/Pepe/MLPaper/Data/Census_clean_female_15_49.RData")


# Specify the variables of short model
variables_short <- c("married", 
                     "age_cat_15_24", "age_cat_25_29", "age_cat_30_34", 
                     "age_cat_35_39", "age_cat_40_44", "age_cat_45_49", 
                     "education_no_edu", "education_primary_edu", 
                     "education_secondary_edu", "education_tertiary_edu", 
                     "working", "rural", "kidsathome", "electricity", "toilet",
                     "NAME_0", "region", "country", "LATNUM", "LONGNUM", "hv_wt",
                     "insurance", "visitHF12m", "locationBirth", "postnatalcheck")

# Subset the dataset to keep only these variables
df_short <- df[, variables_short]


# set parameters
train_control <- trainControl(method="repeatedcv", number=5, repeats=1, savePredictions="final")


#------------------------------------------------------------------------------------------------------------
#
#                   I. Create country maps of average outcomes of DHS data, region level
#
#------------------------------------------------------------------------------------------------------------
# overlay DHS and mADM for each country separately - otherwise it crashes
data_benin <- df%>%filter(NAME_0=="benin")
data_benin <- st_intersection(data_benin, mADMdata)


## create region averages
data_benin$average_insurance_region <- ave(as.numeric(data_benin$insurance), data_benin$GID_1, FUN= function(x) mean(x, na.rm = TRUE))
summary(data_benin$average_insurance_region)
data_benin$average_insurance_region <- data_benin$average_insurance_region*100
data_benin_region <- data_benin[!duplicated(data_benin$GID_1), ]
data_benin_region <- data_benin_region[,c("average_insurance_region", "GID_1")]


## Merge data and shapefile
data_benin_region<-shp_df%>%
  left_join(data_benin_region, by="GID_1")
data_benin_region <- data_benin_region%>%filter(NAME_0=="Benin")

## Map INSURANCE
map_insurance<-ggplot() + 
  # fill DHS countries
  geom_polygon(data = data_benin_region, aes(x = long, y = lat, group = group, fill=average_insurance_region), colour="transparent", size=0.02)+
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "firebrick3",
    high = "gold1",
    na.value = "white",
    guide = "colourbar",
    aesthetics = "fill")+
  labs(y="Latitude", x = "Longtitude", fill="Percentage (%)") +
  ggtitle("Average Insurance Coverage, DHS")+
  theme(legend.position="right") 
map_insurance
#ggsave(map_insurance, file="map_insurance_DHS_Benin.pdf")


#------------------------------------------------------------------------------------------------------------
#
#                   II. Create country maps of average outcomes of xgboost predictions, region level
#
#------------------------------------------------------------------------------------------------------------
## create region averages of xgboost prediction
data_benin$average_insurance_prediction_xgboost_long_model <- ave(as.numeric(data_benin$insurance_prediction_xgboost_long_model), data_benin$GID_1, FUN= function(x) mean(x, na.rm = TRUE))
summary(data_benin$average_insurance_prediction_xgboost_long_model)
data_benin$average_insurance_prediction_xgboost_long_model <- data_benin$average_insurance_prediction_xgboost_long_model*100
data_benin_region_pred <- data_benin[!duplicated(data_benin$GID_1), ]
data_benin_region_pred <- data_benin_region_pred[,c("average_insurance_prediction_xgboost_long_model", "GID_1")]


## Merge data and shapefile
data_benin_region_pred<-shp_df%>%
  left_join(data_benin_region_pred, by="GID_1")
data_benin_region_pred <- data_benin_region_pred%>%filter(NAME_0=="Benin")


## Map INSURANCE
map_insurance<-ggplot() + 
  # fill DHS countries
  geom_polygon(data = data_benin_region_pred, aes(x = long, y = lat, group = group, fill=average_insurance_prediction_xgboost_long_model), colour="transparent", size=0.02)+
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "firebrick3",
    high = "gold1",
    na.value = "white",
    guide = "colourbar",
    aesthetics = "fill")+
  labs(y="Latitude", x = "Longtitude", fill="Percentage (%)") +
  ggtitle("Predicted Insurance Coverage, DHS")+
  theme(legend.position="right") 
map_insurance
#ggsave(map_insurance, file="map_insurance_prediction_DHS_Benin.pdf")


rm(mADM, mADMdata, map_insurance, shp, shp_df, data_benin_region, data_benin_region_pred, df)

#------------------------------------------------------------------------------------------------------------
#
#                   III. Create short xgboost model for insurance coverage in sample for every country
#
#------------------------------------------------------------------------------------------------------------


#===============================================================================
# benin in sample
#===============================================================================
## split
df_short_benin <- df_short[which(df_short$NAME_0 == "benin"),]
df_short_benin <- df_short_benin[!names(df_short_benin) %in% c("country", "NAME_0")]
df_short_benin <- sf::st_drop_geometry(df_short_benin)
df_short_benin <- as.data.frame(na.omit(df_short_benin))
smp_size_insurance <- floor(0.66 * nrow(df_short_benin))
set.seed(1)   ## set the seed to make your partition reproducible
train_ind_insurance <- sample(seq_len(nrow(df_short_benin)), size = smp_size_insurance)
train_insurance <- df_short_benin[train_ind_insurance, ]
test_insurance <- df_short_benin[-train_ind_insurance, ]
## Add here upsampling/downsampling for imbalanced variables


## model
set.seed(1)
tune_grid <- expand.grid(nrounds=c(100,200,300,400), max_depth = c(3:7),
                         eta = c(0.05, 1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))
xgboost_insurance_cv <- train(as.factor(insurance) ~. -region -LATNUM -LONGNUM -hv_wt 
                              -visitHF12m -locationBirth -postnatalcheck, data = train_insurance, method = "xgbTree", trControl=train_control, tuneGrid = tune_grid, tuneLength = 10)



## in-sample DHS
predict_xgboost_insurance_cv <- predict(xgboost_insurance_cv,test_insurance, type="prob") 
Perf_xgboost_insurance_cv <- prediction(as.numeric(predict_xgboost_insurance_cv[[2]]), as.numeric(test_insurance$insurance))
pred_dummy <-  ifelse(predict_xgboost_insurance_cv[[2]]> 0.5 , 1,0)
test_insurance$insurance_xgboost_insurance_cv <- predict_xgboost_insurance_cv$`1`
summary(test_insurance$insurance_xgboost_insurance_cv)
test_insurance$NAME_0 <- "benin"
save(test_insurance, file="X:/research_assistants/Pepe/MLPaper/Data/test_insurance_xgboost_short_benin.RData")


# transfrom for overlaying DHS and GADM
coordinates(test_insurance) <- test_insurance[c("LONGNUM", "LATNUM")]
test_insurance <- st_as_sf(test_insurance)
test_insurance <- st_set_crs(test_insurance, 4269)
test_insurance <- st_intersection(test_insurance, mADMdata)


## create region averages of xgboost prediction
test_insurance$average_insurance_prediction_xgboost_short_model <- ave(as.numeric(test_insurance$insurance_xgboost_insurance_cv), test_insurance$GID_1, FUN= function(x) mean(x, na.rm = TRUE))
summary(test_insurance$average_insurance_prediction_xgboost_short_model)
test_insurance$average_insurance_prediction_xgboost_short_model <- test_insurance$average_insurance_prediction_xgboost_short_model*100
insurance_short_benin_region_pred <- test_insurance[!duplicated(test_insurance$GID_1), ]
insurance_short_benin_region_pred  <- insurance_short_benin_region_pred [,c("average_insurance_prediction_xgboost_short_model", "GID_1")]


## Merge data and shapefile
insurance_short_benin_region_pred<-shp_df%>%
  left_join(insurance_short_benin_region_pred, by="GID_1")
insurance_short_benin_region_pred <- insurance_short_benin_region_pred%>%filter(NAME_0=="Benin")


## Map INSURANCE
map_insurance<-ggplot() + 
  # fill DHS countries
  geom_polygon(data = insurance_short_benin_region_pred, aes(x = long, y = lat, group = group, fill=average_insurance_prediction_xgboost_short_model), colour="transparent", size=0.02)+
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "firebrick3",
    high = "gold1",
    na.value = "white",
    guide = "colourbar",
    aesthetics = "fill")+
  labs(y="Latitude", x = "Longtitude", fill="Percentage (%)") +
  ggtitle("Predicted Insurance Coverage, short model, DHS")+
  theme(legend.position="right") 
map_insurance
#ggsave(map_insurance, file="map_insurance_prediction_short_DHS_Benin.pdf")




#------------------------------------------------------------------------------------------------------------
#
#                   IV. Create short xgboost model for insurance coverage out of sample for every country
#
#------------------------------------------------------------------------------------------------------------

#===============================================================================
# benin out of sample
#===============================================================================
## split
df_short_benin <- df_short[which(df_short$NAME_0 == "benin"),]
df_short_benin <- df_short_benin[!names(df_short_benin) %in% c("country", "NAME_0", "region", "LATNUM", "LONGNUM", "hv_wt", "visitHF12m", "locationBirth", "postnatalcheck")]
df_short_benin <- sf::st_drop_geometry(df_short_benin)
df_short_benin <- as.data.frame(na.omit(df_short_benin))
smp_size_insurance <- floor(0.66 * nrow(df_short_benin))
set.seed(1)   ## set the seed to make your partition reproducible
train_ind_insurance <- sample(seq_len(nrow(df_short_benin)), size = smp_size_insurance)
train_insurance <- df_short_benin[train_ind_insurance, ]
test_insurance <- df_short_benin[-train_ind_insurance, ]
## Add here upsampling/downsampling for imbalanced variables


## model
set.seed(1)
tune_grid <- expand.grid(nrounds=c(100,200,300,400), max_depth = c(3:7),
                         eta = c(0.05, 1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))
xgboost_insurance_cv <- train(as.factor(insurance) ~., data = train_insurance, method = "xgbTree", trControl=train_control, tuneGrid = tune_grid, tuneLength = 10)


## Out-of-sample Census
census_benin <- census
variables_census <- c("married", 
                     "age_cat_15_24", "age_cat_25_29", "age_cat_30_34", 
                     "age_cat_35_39", "age_cat_40_44", "age_cat_45_49", 
                     "education_no_edu", "education_primary_edu", 
                     "education_secondary_edu", "education_tertiary_edu", 
                     "working", "rural", "kidsathome", "electricity", "toilet")
census_benin <- census_benin[, variables_census]
predict_xgboost_census <- predict(xgboost_insurance_cv, census_benin, type="prob")
summary(predict_xgboost_census$`1`)
predict_xgboost_census$ID <- 1:nrow(census_benin)
Census_xgboost_with_prediciton_benin <- merge(census, predict_xgboost_census, by="ID") 
Census_xgboost_with_prediciton_benin <- Census_xgboost_with_prediciton_benin[which(Census_xgboost_with_prediciton_benin$GID_0 == "BEN"),]
Census_xgboost_with_prediciton_benin$insurance <- Census_xgboost_with_prediciton_benin$`1`
summary(Census_xgboost_with_prediciton_benin$insurance)
summary(Census_xgboost_with_prediciton_benin$insurance)
save(Census_xgboost_with_prediciton_benin, file="X:/research_assistants/Pepe/MLPaper/Data/Census_xgboost_with_prediciton_benin.RData")
load(Census_xgboost_with_prediciton_benin)


### add key to Census data
Match_test_shp_Census <- read_excel("X:/research_assistants/Pepe/MLPaper/Data/Match_test_shp.xlsx", sheet = "Sheet2")
merged <- Match_test_shp_Census %>%
  left_join(Census_xgboost_with_prediciton_benin, by="GID_1_IPUMS_ID")
merged <- merged[names(merged) %in% c("GID_1","insurance")]
merged <-shp_df%>%
  left_join(merged, by="GID_1")


## MAP 
merged_Benin <-  merged[ which(merged$GID_0=="BEN"),]
map_insurance<-ggplot() + 
  # fill DHS countries
  geom_polygon(data = merged_Benin, aes(x = long, y = lat, group = group, fill=insurance), colour="transparent", size=0.02)+
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "firebrick3",
    high = "gold1",
    na.value = "white",
    guide = "colourbar",
    aesthetics = "fill")+
  labs(y="Latitude", x = "Longtitude", fill="Percentage (%)") +
  ggtitle("Predicted Insurance Coverage, Census")+
  theme(legend.position="right") 
map_insurance
  
  
  
  
  
  # Overlay with regional boundaries and fill 
  geom_polygon(data =merged_Benin, aes(x = long, y = lat, group = group,fill=average_region_insurance_nnet),colour="transparent",size=0.02)+
  # Overlay with country boundaries
  geom_polygon(data = shp_df_ALL%>%filter(NAME=="BENIN"), aes(x = long, y = lat, group = group), fill=NA,colour="gray",alpha=1,size=0.04)+
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "gray80",
    high = "gray10",
    na.value = "white",
    guide = "colourbar",
    aesthetics = "fill")+
  labs(y="Latitude", x = "Longtitude", fill="Percentage (%)") +
  ggtitle("Predicted Insurance Coverage Ghana")+
  theme(legend.position="right") 
map_insurance
ggsave(map_insurance, file="map_insurance_nnet_29122022_Ghana.pdf")
ggsave(map_insurance, file="map_insurance_nnet_29122022_Ghana.png")

