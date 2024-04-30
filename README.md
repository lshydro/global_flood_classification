# global_flood_classification
Location-independent classification of flood peak into their flood generating process.
Version 1 of this project gives an overview how the flood classification is programmed and the resulting output.  

## Function_file.R

Includes the functions to calculate soil moisture and snowmelt, a decision data frame for the flood classification and the classification function. 

## Flood_process_classification.R

Gives an example of what input data for the classification is needed and how the functions are applied. 


## event_df.csv

Output for the classification. Gsim.no, gsim.AMAX_date, gsim.lat.new, gsim.long.new are taken from the Global Streamflow Indices and Metadata Archive. For more information please refer to 

>Do, Hong Xuan; Gudmundsson, Lukas; Leonard, Michael; Westra, Seth (2018): The Global Streamflow Indices and Metadata Archive (GSIM) – Part 1: The production of a daily streamflow archive and metadata. Earth System Science Data, 10(2), 765-785, https://doi.org/10.5194/essd-10-765-2018

Process_out refers to the classified flood generating process. For more information on the classification and to cite this work please refer to:

>Stein, L., Pianosi, F., & Woods, R. (2020). Event‐based classification for global study of river flood generating processes. Hydrological Processes, 34(7), 1514-1529.
