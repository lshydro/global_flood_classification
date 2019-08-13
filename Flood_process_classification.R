###########################################################-
# Objective: Location independent flood process classification
# Author: Lina Stein, University of Bristol
# date: 13.08.2019
###########################################################-

#AWC_cat_reduced: Vector of mean available water capacity per catchment
#GLEAM_reduced: Matrix with daily values (rows) for actual evapotranspiration per catchment (cols)
#MSWEP_reduced: Matrix with daily values (rows) for precipitation per catchment (cols)
#BEST_reduced: Matrix with daily values (rows) for temperature per catchment (cols)
#tc_grid Vector of mean critical temperature per catchment
#ind_df$date: date vector over whole time series
#pfdf_names: full list of all catchment identifiers (gsim number)
#OUTPUT:
#event_mech_list: data frame with date of flood, flood magnitude, decision variables, classification thresholds and classified flood process

# Calculate soil and snow routine ----
class_input_df_list_out = soil_snow_func(AWC_in = (AWC_cat_reduced, ET_mat = GLEAM_reduced, rain_mat = MSWEP_reduced, Temp_mat = BEST_reduced, fdd_in = 2, Tcrit_in = tc_grid, date_vec = ind_df$date)

# Create decision data frames ----
event_df_list = lapply(pfdf_names, FUN = function(x){
  event_classification_df(x, day_thresh = 7, quant_thresh_rain = 0.9, quant_thresh_snow = 0.9, df_list = class_input_df_list_out)
})

# Run classification ----

event_mech_list = lapply(event_df_list, FUN = function(x){
  event_classification_quantile(x, parts_rainsnow = 1/3, parts_fracextreme = 2/3)
})
