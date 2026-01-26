#' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'  
#'  Description:  This script creates 4 maps for each of the 32x4 countryxoutcome combination running the function "make_OOB_predictions_and_maps"
#'
#'  Authors:      Kathrin Durizzo, Beat Scherrer               
#'  Date:         30.07.2024
#'  
#'  @param outcome_var can take any of the following values: c("insurance","locationBirth","postnatalcheck", "visitHF12m")
#'  @param map_type can take any of the following values: c("predicted_census_district", "predicted_dhs_region_long", "predicted_dhs_region_short", "average_dhs_region", "all")
#'
#' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create a list containing all countries, input paths, and region names
country_list <- list(
  Burkina.Faso = list(country = "Burkina.Faso", country_code = "BF"),
  Benin = list(country = "Benin", country_code = "BJ"),
  Burundi = list(country = "Burundi", country_code = "BU"),
  Cameroon = list(country = "Cameroon", country_code = "CM"),
  DRC = list(country = "DRC", country_code = "CD"),
  Republic.of.the.Congo = list(country = "Republic.of.the.Congo", country_code = "CIG"),
  Cote.dIvoire = list(country = "Cote.dIvoire", country_code = "CI"),
  Eswatini = list(country = "Eswatini", country_code = "SZ"),
  Ethiopia = list(country = "Ethiopia", country_code = "ET"),
  The.Gambia = list(country = "The.Gambia", country_code = "GM"),
  Ghana = list(country = "Ghana", country_code = "GH"),
  Guinea = list(country = "Guinea", country_code = "GN"),
  Kenya = list(country = "Kenya", country_code = "KE"),
  Comoros = list(country = "Comoros", country_code = "KM"),
  Liberia = list(country = "Liberia", country_code = "LB"),
  Lesotho = list(country = "Lesotho", country_code = "LS"),
  Madagascar = list(country = "Madagascar", country_code = "MD"),
  Mauritania = list(country = "Mauritania", country_code = "MR"),
  Mali = list(country = "Mali", country_code = "ML"),
  Malawi = list(country = "Malawi", country_code = "MW"),
  Nigeria = list(country = "Nigeria", country_code = "NG"),
  Niger = list(country = "Niger", country_code = "NI"),
  Namibia = list(country = "Namibia", country_code = "NA"),
  Rwanda = list(country = "Rwanda", country_code = "RW"),
  Sao.Tome = list(country = "Sao.Tome", country_code = "ST"),
  South.Africa = list(country = "South.Africa", country_code = "ZA"),
  Sierra.Leone = list(country = "Sierra.Leone", country_code = "SL"),
  Senegal = list(country = "Senegal", country_code = "SN"),
  Chad = list(country = "Chad", country_code = "TD"),
  Togo = list(country = "Togo", country_code = "TG"),
  Tanzania = list(country = "Tanzania", country_code = "TZ"),
  Uganda = list(country = "Uganda", country_code = "UG"),
  Zambia = list(country = "Zambia", country_code = "ZM"),
  Zimbabwe = list(country = "Zimbabwe", country_code = "ZW")
)

outcome_vars <- c("insurance","locationBirth","postnatalcheck", "visitHF12m")

# Source function script
source("Dofiles/dhs_functions.R")

# CALL THE FUNCTION
map_types <- c("predicted_census_district", "predicted_dhs_region_long", "predicted_dhs_region_short", "average_dhs_region")
for (map_type in map_types) {
  for (outcome_var in outcome_vars) {
    do.call(make_OOB_predictions_and_maps, list(outcome_var = outcome_var, map_type = map_type))
  }
}
