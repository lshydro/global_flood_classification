
###########################################################-
# Objective: Location independent flood process classification
# Function file
# Author: Lina Stein, University of Bristol
# date: 13.08.2019
###########################################################-

snowmelt_func = function(fdd_func, T_func, Tcrit_func, Ssnow_func){
  #snowmelt is dependent on fdd and temperature difference between T and Tcrit or snow storage, whichever is smaller
  #all input supplied in soil_snow_func
  #fdd_func: melt rate
  #T_func: temperature that day
  #Tcrit_func: critical temperature below which rain turns to snow
  #Ssnow_func: snow storage that day
  
  min(fdd_func*max(T_func-Tcrit_func, 0), Ssnow_func)
}
soilstrg_func = function(Ssoil_prev_func, P_func, Ssmax_func, ET_func){
  #soil storage is previous soil storage + rainfall (limited by maximum soil storage) minus ET. Value cannot go below zero
  #all input supplied in soil_snow_func
  #Ssoil_prev_func: Soil storage previous day
  #P_func: precipitation that day
  #Ssmax_func: maximum available water capacity
  #ET_func: actual evapotranspiration that day
  
  max(min(Ssoil_prev_func+P_func, Ssmax_func) - ET_func,0)
}
soiloverflow_func = function(Ssoil_prev_func, P_func, Ssmax_func){
  #soil overflow is previous soil storage + rainfall minus max. soil storage. Value cannot go below zero
  #all input supplied in soil_snow_func
  #Ssoil_prev_func: Soil storage previous day
  #P_func: precipitation that day
  #Ssmax_func: maximum available water capacity
  
  max(Ssoil_prev_func+P_func - Ssmax_func, 0)
}

soil_snow_func = function(AWC_in, ET_mat, rain_mat, Temp_mat, fdd_in, Tcrit_in, date_vec){
  #INPUT
  #AWC_in: soil storage value
  #ET_mat: evapotranspiration matrix (col: catchments, row: days)
  #rain_mat: precipitation matrix (col: catchments, row: days)
  #Temp_mat: temperature matrix (col: catchments, row: days)
  #path_out: file path to store output files
  #fdd_in: melt rate
  #Tcrit_in: critical temperature below which rain turns to snow
  #date_vec: time stamp for timeseries
  #OUTPUT
  #Climate data input dataframe
  
  
  fdd = fdd_in #(mm/d/k)
  month_ind = format(date_vec, "%m")
  month_day_ind = format(date_vec,"%m-%d")
  
  #if Tcrit is not available (for example in the southern hemisphere), set value to 1 degree which is the average Jennings et al found in the northern hemisphere
  
  
  class_input_df_list = mapply(FUN = function(P_vec, Ta_vec, ET_vec, Ssmax, Tcrit){
    ET_vec[ET_vec<0] = 0
    
    if(is.na(Tcrit)){
      Tcrit_used = 1
    } else{
      Tcrit_used = Tcrit
    }
    
    #Snow storage accumulation
    #end of hottest month on average (use 28th of month for potential February dates)
    
    av_month_temp = aggregate(Ta_vec, by = list(month_ind), mean, na.rm = T)[,2]
    max_temp_month = as.character(which.max(av_month_temp))
    if(nchar(max_temp_month)==1){
      zero_date = paste0("0", max_temp_month, "-28")
    } else {
      zero_date = paste0(max_temp_month, "-28")
    }
    zero_ind = month_day_ind == zero_date
    
    #Vec pre-allocation
    #Snow storage
    Ssnow_vec = rep(NA, length(P_vec))
    Ssnow_vec[1] = 0
    #Snow melt
    P_snow_vec = rep(NA, length(P_vec))
    P_snow_vec[1] = 0
    #Soil storage
    Ssoil_vec = rep(NA, length(P_vec))
    Ssoil_vec[1] = 0
    #Soil overflow
    soil_overflw = rep(NA, length(P_vec))
    
    #only do calculations if soil storage value is available
    if(!is.na(Ssmax)){
      #only caculate soil calculations if Tcrit_used = NA
      if(is.na(Tcrit_used)){
        for(i in c(2:length(Ssoil_vec))){
          Ssoil_vec[i] = soilstrg_func(Ssoil_vec[i-1], P_vec[i], Ssmax, ET_vec[i])
          soil_overflw[i] = soiloverflow_func(Ssoil_vec[i-1], P_vec[i], Ssmax)
        }
      } else {
        #calculate soil and snow routine
        for(i in c(2:length(Ssoil_vec))){
          #calculate snow routine
          #snow storage
          if(is.na(Ta_vec[i])){
            #if temperature data is not available snowstorage is NA
            Ssnow_vec[i] = NA
          } else if(Ta_vec[i]<Tcrit_used){
            Ssnow_vec[i] = Ssnow_vec[i-1] + P_vec[i]
            P_snow_vec[i] = 0
            if(zero_ind[i]){
              Ssnow_vec[i] = 0
            }
          } else {
            P_snow_vec[i] = snowmelt_func(fdd, Ta_vec[i], Tcrit_used, Ssnow_vec[i-1])
            Ssnow_vec[i] = max(Ssnow_vec[i-1]-P_snow_vec[i],0)
            if(zero_ind[i]){
              Ssnow_vec[i] = 0
            }
          }
        }
        
        #"Pause" soil routine during snow cover
        P_vec_soil = P_vec
        P_vec_soil[Ssnow_vec>0] = 0 
        ET_vec_soil = ET_vec
        ET_vec_soil[Ssnow_vec>0] = 0
        
        for(i in c(2:length(Ssoil_vec))){
          #calculate soil storage
          if(is.na(Ssoil_vec[i-1])){
            Ssoil_vec[i-1] = 0
          }
          Ssoil_vec[i] = soilstrg_func(Ssoil_vec[i-1], P_vec_soil[i], Ssmax, ET_vec_soil[i])
          soil_overflw[i] = soiloverflow_func(Ssoil_vec[i-1], P_vec[i], Ssmax)
        }
      }
    }
    #all P below critical temperature set to 0, as it falls as snow
    P_vec[Ta_vec<Tcrit_used] = 0
    out_df = data.frame(date_vec, P_vec, Ta_vec, ET_vec, soil_overflw, Ssoil_vec/Ssmax, Ssnow_vec, P_snow_vec)  
    return(out_df)
  },P_vec = as.data.frame(rain_mat), Ta_vec = as.data.frame(Temp_mat), ET_vec = as.data.frame(ET_mat), Ssmax = AWC_in, Tcrit = Tcrit_in, SIMPLIFY = F)
  #change file name according to parameter set?
  #save(class_input_df_list, file = paste0(path_out, "class_input_df_list.Rdata"))
  return(class_input_df_list)
  
} #end of function  



event_classification_df = function(cat_name, day_thresh, quant_thresh_rain, quant_thresh_snow, df_list, full_cat_list = pfdf_names){
  #Create decision data frame based on which the classification tree classifies events
  #INPUT
  #cat_name: catchment identifier (gsim number)
  #day_thresh: how many days before the flood event are taken into account
  #quant_thresh_rain: percentile threshold limit for short and long rainfall
  #quant_thresh_snow: percentile threshold limit for snowmelt
  #df_list: Climate data input generated from soil_snow_func 
  #full_cat_list: full list of all catchment identifiers
  #flood_df: list of flood data frames with flood date and flood magnitude for each catchment
  #OUTPUT:
  #decision data frame 
  
  cat = which(full_cat_list == cat_name) 
  flood_date = .subset2(flood_df[[cat]][[2]], "AMAX_date")
  flood_magn = as.numeric(as.character(.subset2(flood_df[[cat]][[2]], "AMAX")))
  
  flood_data_df = data.frame(flood_date, flood_magn)
  colnames(flood_data_df) = c("flood_date", "flood_magn")
  
  df = df_list[[cat]]
  P_val = .subset2(df, "P_vec")
  snow_val = .subset2(df, "P_snow_vec")
  sat_val = .subset2(df, "Ssoil_vec.Ssmax")
  dates_full = .subset2(df, 1)
  
  #due to the structure of the classification function all decision thresholds need to be the same length as the data, therefore repeated 34 times
  P7_rollsum = RcppRoll::roll_sum(c(numeric(day_thresh-1), P_val), n = day_thresh, align = "right")
  #add numeric zeros to achieve partial rollsums at start of ts
  P7_mean = mean(P7_rollsum, na.rm = T)
  P7_quant = quantile(P7_rollsum, quant_thresh_rain, na.rm = T)
  Pday_quant = quantile(P_val, quant_thresh_rain, na.rm = T)
  S7_rollsum = RcppRoll::roll_sum(c(numeric(day_thresh-1), snow_val), n = day_thresh, align = "right")
  S7_quant = quantile(S7_rollsum[S7_rollsum>1], quant_thresh_snow, na.rm = T)
  
  temp = round(c(Pday_quant, P7_mean, P7_quant, S7_quant), 2)
  temp_out = matrix(rep(temp, 34), nrow = 34, byrow = T)
  colnames(temp_out) = c("Pday_quant", "P7_mean", "P7_quant", "S7_quant")
  
  
  #create data frame with relevant information to reach decision of flood mechanism
  decision_df_list = lapply(flood_date, FUN = function(x){
    if(is.na(x)){ #flood date missing
      out = rep(list(NA), 6)
    } else if(all(is.na(sat_val))){ #max soil routine is NA
      out = rep(list(NA), 6)
    } else if(!(x %in% ind_df[,1])){ #dates missing
      out = rep(list(NA), 6)
    } else{
      flood_day = which(dates_full == x)
      flood_period = c(1:length(dates_full)) %in% c(max((flood_day-(day_thresh-1)),1):flood_day) #max function so that index does not go below 1
      
      #temp_df = df[max((flood_day-(day_thresh-1)),1):flood_day,] 
      Pmax = max(P_val[flood_period])
      P7 = sum(P_val[flood_period])
      S7 = sum(snow_val[flood_period])
      Ptotal = sum(c(P7, S7), na.rm = T)
      Pmax_frac = Pmax/P7
      sat_start = sat_val[flood_period][1] #saturation on first day of period
      out = list(sat_start, Pmax, P7, Pmax_frac, S7, Ptotal)
      out = lapply(out, round, 2)
      #if no maximum is found, output is length zero, change to NA
      out = lapply(out, function(x){ifelse(length(x)==0, NA, x)})
    }
    out
  })
  
  decision_df_temp = as.data.frame(rbindlist(decision_df_list))
  colnames(decision_df_temp) = c("sat_start", "Pmax", "P7", "Pmax_frac", "S7", "Ptotal")
  decision_df = data.frame(flood_data_df, decision_df_temp, temp_out)
  decision_df
}


event_classification_quantile = function(decision_df, parts_rainsnow = 1/3, parts_fracextreme = 2/3, sat_thresh = 0.9){
  #classification tree for flood peak classification
  #INPUT:
  #decision df: decision data frame generated from event_classification_df function
  #parts_rainsnow: threshold limit overlap between rainfall and snowmelt
  #parts_fracextreme: threshold limit for how much weekly rainfall has to fall on one day to be considered a short rainfall
  #sat_thresh: threshold limit saturated soil
  #OUTPUT: 
  #decision df with additional flood process column
  
  ELSE = TRUE
  decision_result = decision_df %>% mutate(.,mech_out = with(.,case_when(
    #minimum rainfall limit
    (Ptotal < 1) ~ "noclass",
    (!is.na(S7) & (S7/Ptotal)>=parts_rainsnow & (P7/Ptotal)>=parts_rainsnow) ~ "rainandsnow",
    (!is.na(S7) & S7>=S7_quant) ~ "snowmelt",
    (sat_start >= sat_thresh & P7 >= P7_mean) ~ "soilsat",
    (P7 >= P7_quant & Pmax_frac >= parts_fracextreme) ~ "rainfall",
    (P7 >= P7_quant & Pmax_frac < parts_fracextreme) ~ "longrainfall",
    (Pmax >= Pday_quant) ~ "rainfall",
    (Pmax < Pday_quant) ~ "noclass",
    ELSE ~ "missingData"
  )))
  
  #turn numbers back into dates
  decision_result = decision_result %>% mutate(flood_date = as.Date(flood_date,  origin = "1970-01-01"))
  return(decision_result)
}
