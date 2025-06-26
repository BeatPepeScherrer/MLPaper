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
library(haven)
library(readxl)

getwd()
setwd("X:/research_assistants/Pepe/MLPaper")


# read in subset of DHS data, incl. prediction with ML model
load("X:/research_assistants/Pepe/MLPaper/final_data/insurance/df_final_insurance_Ghana.RData")


# Specify the variables of short model and take subset: if dataset updated based on short model, this step is not necessary anymore!!
variables_short <- c("married", 
                     "age_cat_15_24", "age_cat_25_29", "age_cat_30_34", 
                     "age_cat_35_39", "age_cat_40_44", "age_cat_45_49", 
                     "education_no_edu", "education_primary_edu", 
                     "education_secondary_edu", "education_tertiary_edu", 
                     "currently_working", "rural", "sondaughterhome", "electricity", "toilet",
                     "NAME_0", "region", "hv001", "insurance", "predicted_class")
df_final <- df_final[, variables_short]


#change naming for census matching
colnames(df_final)[colnames(df_final) == "currently_working"] <- "working"
colnames(df_final)[colnames(df_final) == "sondaughterhome"] <- "kidsathome"


# Add geolocation to the DHS data
df_geolocation <- read_dta("X:/research_assistants/Pepe/MLPaper/Data/Geolocation/GHGE71FL_data.DTA")
df_geolocation$hv001 <- df_geolocation$DHSCLUST
df_geolocation <- df_geolocation[,c("hv001", "LATNUM", "LONGNUM")]
df <- merge(df_final, df_geolocation, by = c("hv001"), all=TRUE)


# read in GADM data
mADM<- readRDS(file = "X:/research_assistants/Pepe/MLPaper/DataRaw/GADM/mADM.rds")


# transform for overlaying DHS and GADM
coordinates(df) <- df[c("LONGNUM", "LATNUM")]
mADMdata <- st_as_sf(mADM)
df <- st_as_sf(df)
mADMdata <- st_transform(mADMdata, 4269)
df <- st_set_crs(df, 4269)
shp <- mADM
shp_df <- tidy(shp)
shp$id <- row.names(shp)
shp_df <- left_join(shp_df, shp@data, by="id")


# import Census data
load("X:/research_assistants/Pepe/MLPaper/DataRaw/Census/Census_clean_female_15_49.RData")
census <- census%>%filter(NAME_0_ghana==1)
census$ID <- 1:nrow(census)

#clean up
rm(df_final, df_geolocation)
rm(mADM, mADMdata, map_insurance, shp, shp_df)


#------------------------------------------------------------------------------------------------------------
#
#                   Ia. Create country maps of average outcomes of DHS data, region level
#
#------------------------------------------------------------------------------------------------------------
# overlay DHS and mADM for each country separately - otherwise it crashes
data <- df%>%filter(NAME_0=="Ghana")
data <- st_intersection(data, mADMdata)


## create region averages
data$average_insurance_region <- ave(as.numeric(data$insurance), data$GID_1, FUN= function(x) mean(x, na.rm = TRUE))
summary(data$average_insurance_region)
data$average_insurance_region <- data$average_insurance_region*100
data_region <- data[!duplicated(data$GID_1), ]
data_region <- data_region[,c("average_insurance_region", "GID_1")]


## Merge data and shapefile
data_region<-shp_df%>%
  left_join(data_region, by="GID_1")
data_region <- data_region%>%filter(NAME_0=="Ghana")

## Map INSURANCE
map_insurance <- ggplot() + 
  geom_polygon(data = data_region, aes(x = long, y = lat, group = group, fill = average_insurance_region), 
               colour = "transparent", size = 0.02) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + 
  coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "firebrick3",
    high = "gold1",
    na.value = "grey",  # NA color is grey
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(0, 100),  # Set scale range from 0 to 100
    oob = scales::squish  # Values outside the range are squished to the nearest limit
  ) +
  labs(y = "Latitude", x = "Longitude", fill = "Percentage (%)") +
  ggtitle("Average Insurance Coverage, DHS Ghana") +
  theme(legend.position = "right") 
map_insurance
#ggsave(map_insurance, file="map_insurance_DHS_region_Ghana.pdf")



#------------------------------------------------------------------------------------------------------------
#
#                   Ib. Create country maps of average outcomes of DHS data, district level
#
#------------------------------------------------------------------------------------------------------------
# Filter Ghana and overlay DHS and mADM data
data <- df %>% filter(NAME_0 == "Ghana")
data <- st_intersection(data, mADMdata)

# Create district averages and replace districts with <10 observations with NA
data$average_insurance_district <- ave(as.numeric(data$insurance), data$GID_2, FUN = function(x) mean(x, na.rm = TRUE))
data$average_insurance_district <- data$average_insurance_district * 100
data$GID_2_count <- ave(data$GID_2, data$GID_2, FUN = length)
data$GID_2_count <- as.numeric(data$GID_2_count)
data$average_insurance_district <- ifelse(data$GID_2_count < 30, NA, data$average_insurance_district)

# Keep unique GID_2 rows for merging
data_district <- data[!duplicated(data$GID_2), ]
data_district <- data_district[, c("average_insurance_district", "GID_2")]

# Merge data with shapefile
data_district <- shp_df %>%
  left_join(data_district, by = "GID_2")
data_district <- data_district%>%filter(NAME_0=="Ghana")

# Map Insurance
map_insurance <- ggplot() + 
  geom_polygon(data = data_district, aes(x = long, y = lat, group = group, fill = average_insurance_district), 
               colour = "transparent", size = 0.02) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + 
  coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "firebrick3",
    high = "gold1",
    na.value = "grey",  # NA color is grey
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(0, 100),  # Set scale range from 0 to 100
    oob = scales::squish  # Values outside the range are squished to the nearest limit
  ) +
  labs(y = "Latitude", x = "Longitude", fill = "Percentage (%)") +
  ggtitle("Average Insurance Coverage, DHS Ghana") +
  theme(legend.position = "right") 
map_insurance
#ggsave(map_insurance, file="map_insurance_DHS_district_Ghana.pdf")


#------------------------------------------------------------------------------------------------------------
#
#                   II. Create country maps of average outcomes of xgboost predictions, district level
#
#------------------------------------------------------------------------------------------------------------
# Filter Ghana and overlay DHS and mADM data
data <- df %>% filter(NAME_0 == "Ghana")
data <- st_intersection(data, mADMdata)

# Create district averages and replace districts with <10 observations with NA
data$average_insurance_district <- ave(as.numeric(data$predicted_class), data$GID_2, FUN = function(x) mean(x, na.rm = TRUE))
data$average_insurance_district <- data$average_insurance_district * 100
data$GID_2_count <- ave(data$GID_2, data$GID_2, FUN = length)
data$GID_2_count <- as.numeric(data$GID_2_count)
data$average_insurance_district <- ifelse(data$GID_2_count < 30, NA, data$average_insurance_district)

# Keep unique GID_2 rows for merging
data_district <- data[!duplicated(data$GID_2), ]
data_district <- data_district[, c("average_insurance_district", "GID_2")]

# Merge data with shapefile
data_district <- shp_df %>%
  left_join(data_district, by = "GID_2")
data_district <- data_district%>%filter(NAME_0=="Ghana")

# Map Insurance
map_insurance <- ggplot() + 
  geom_polygon(data = data_district, aes(x = long, y = lat, group = group, fill = average_insurance_district), 
               colour = "transparent", size = 0.02) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + 
  coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "firebrick3",
    high = "gold1",
    na.value = "grey",  # NA color is grey
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(0, 100),  # Set scale range from 0 to 100
    oob = scales::squish  # Values outside the range are squished to the nearest limit
  ) +
  labs(y = "Latitude", x = "Longitude", fill = "Percentage (%)") +
  ggtitle("Predicted Insurance Coverage, DHS Ghana") +
  theme(legend.position = "right") 
map_insurance
#ggsave(map_insurance, file="map_insurance_predicted_district_Ghana.pdf")






#------------------------------------------------------------------------------------------------------------
#
#                    III. Create country maps of average outcomes of xgboost predictions with census data, district level
#
#------------------------------------------------------------------------------------------------------------

#===============================================================================
# this should be included in short model generation in the same way as you do it for the DHS model
#===============================================================================
## split
variables_short <- c("married", 
                      "age_cat_15_24", "age_cat_25_29", "age_cat_30_34", 
                      "age_cat_35_39", "age_cat_40_44", "age_cat_45_49", 
                      "education_no_edu", "education_primary_edu", 
                      "education_secondary_edu", "education_tertiary_edu", 
                      "working", "rural", "kidsathome", "electricity", "toilet", "insurance")
df_final <- df_final[, variables_short]

df_final$insurance <- as.factor(df_final$insurance)
df_final$working <- as.factor(df_final$working)
df_final$rural <- as.factor(df_final$rural)
df_final$married <- as.factor(df_final$married)
df_final$kidsathome <- as.factor(df_final$kidsathome)
df_final$electricity <- as.factor(df_final$electricity)
df_final$toilet <- as.factor(df_final$toilet)
df_final$age_cat_15_24 <- as.factor(df_final$age_cat_15_24)
df_final$age_cat_25_29 <- as.factor(df_final$age_cat_25_29)
df_final$age_cat_30_34 <- as.factor(df_final$age_cat_30_34)
df_final$age_cat_35_39 <- as.factor(df_final$age_cat_35_39)
df_final$age_cat_40_44 <- as.factor(df_final$age_cat_40_44)
df_final$age_cat_45_49 <- as.factor(df_final$age_cat_45_49)
df_final$education_no_edu <- as.factor(df_final$education_no_edu)
df_final$education_primary_edu <- as.factor(df_final$education_primary_edu)
df_final$education_secondary_edu <- as.factor(df_final$education_secondary_edu)
df_final$education_tertiary_edu <- as.factor(df_final$education_tertiary_edu)

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
train_control <- trainControl(method="repeatedcv", number=1, repeats=1, savePredictions="final")

xgboost_insurance_cv <- train(insurance ~., data = train_insurance, method = "xgbTree", trControl=train_control, tuneGrid = tune_grid, tuneLength = 10)


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
Census_xgboost_with_prediciton <- merge(census, predict_xgboost_census, by="ID") 
Census_xgboost_with_prediciton$insurance <- Census_xgboost_with_prediciton$`1`
save(Census_xgboost_with_prediciton, file="X:/research_assistants/Pepe/MLPaper/Data/Census_xgboost_with_prediciton_Ghana.RData")
#===============================================================================
# this part above should be included in short model generation in the same way as you do it for the DHS model
#===============================================================================


Census_xgboost_with_prediciton$average_district_insurance <- ave(as.numeric(Census_xgboost_with_prediciton$insurance),Census_xgboost_with_prediciton$geolev2, FUN= function(x) mean(x, na.rm = TRUE))
Census_xgboost_with_prediciton$average_district_insurance <- Census_xgboost_with_prediciton$average_district_insurance*100
Census_xgboost_with_prediciton <- Census_xgboost_with_prediciton[!duplicated(Census_xgboost_with_prediciton$geolev2), ]
Census_xgboost_with_prediciton <- rename(Census_xgboost_with_prediciton , c("GEOLEVEL2" = "geolev2"))
Census_xgboost_with_prediciton$GEOLEVEL2 <- as.character(Census_xgboost_with_prediciton$GEOLEVEL2)


## Add world_geolev2 shapefile (not available for Botswana)
shp <- readOGR(dsn = "X:/research_assistants/Pepe/MLPaper/DataRaw/IPUMS/geo2_gh2000_2010/geo2_gh2000_2010.shp", stringsAsFactors = F)
shp_df <- tidy(shp)
shp$id <- row.names(shp)
shp_df_ghana <- left_join(shp_df, shp@data, by="id")


## merge
merged_ghana<-shp_df_ghana%>%
  left_join(Census_xgboost_with_prediciton, by="GEOLEVEL2")


## Map
map_insurance<-ggplot() + 
  # Overlay with regional boundaries and fill 
  geom_polygon(data = merged_ghana, aes(x = long, y = lat, group = group, fill=average_district_insurance), colour="transparent", size=0.02)+
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") + coord_sf(crs = 4326) +
  scale_fill_gradient(
    low = "firebrick3",
    high = "gold1",
    na.value = "white",
    guide = "colourbar",
    aesthetics = "fill",
    limits =c(0,100)) +
  labs(y="Latitude", x = "Longtitude", fill="Percentage (%)") +
  ggtitle("Predicted Insurance Coverage based on Census Data")+
  theme(legend.position="right")
map_insurance
#ggsave(map_insurance, file="X:/research/Data/PhDs/Kathrin2023/MLPaper/map_insurance_ghana_24072023_color.pdf")
#ggsave(map_insurance, file="X:/research/Data/PhDs/Kathrin2023/MLPaper/map_insurance_ghana_24072023_color.png")

