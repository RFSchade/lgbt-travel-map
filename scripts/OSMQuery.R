#===========================================================#
#=============> Querying Open Street Map Data <=============#
#===========================================================#

# Quick install/load of all used packages if needed 
# install.packages("pacman")
# pacman::p_load(pacman, osmdata, sf, tidyverse, raster)
# If you run this script from withing "data_cleaning_script.Rmd", this should not be necessary 

#=====> Querying Europe (with the exception of Greenland and French Guiana) 
# Creating a bounding box for Europe
box = c(xmin = -28.3, ymin = 31.1, xmax = 45.7, ymax = 71.7)
# Turning 'box' into a bbox object
EEA_bb  <- st_bbox(box)
class(EEA_bb)
# Defining the bbox for the OPenStreetMap request
# > To change the the time between queries, change the "timeout" argument in the functon below
# > It is currently set to 2500 seconds
EEA_q <- opq(bbox = EEA_bb, timeout = 2500) 
# Defining requests 
EEA_qa <- add_osm_feature(EEA_q, key = 'lgbtq',value = 'primary')
EEA_qb <- add_osm_feature(EEA_q, key = 'lgbtq',value = 'only')
EEA_qc <- add_osm_feature(EEA_q, key = 'lgbtq',value = 'yes')
EEA_qd <- add_osm_feature(EEA_q, key = 'gay',value = 'yes')
EEA_qe <- add_osm_feature(EEA_q, key = 'gay',value = 'welcome')
EEA_qf <- add_osm_feature(EEA_q, key = 'gay',value = 'only')
# Requesting data 
# (as querying the data all at once seemed to cause trouble with the limit of requests for one's IP adress, data associated with different keys and values is queried separately)
# (This is time consuming, but at least it works if you wait a little inbetween requests)
lgbtq_primary <- osmdata_sf(EEA_qa)
lgbtq_only <- osmdata_sf(EEA_qb)
lgbtq_yes <- osmdata_sf(EEA_qc)
gay_yes <- osmdata_sf(EEA_qd)
gay_welcome <- osmdata_sf(EEA_qe)
gay_only <- osmdata_sf(EEA_qf)
# Gathering the data 
EEA_safe_spaces <- c(lgbtq_primary,
                     lgbtq_only, 
                     lgbtq_yes,
                     gay_yes, 
                     gay_welcome, 
                     gay_only)

#=====> Note
# > The commented code below queries data from bounding boxes containing Greenland and French Guiana respectively
# > It is left commented because the queries did not yield any data

# #=====> Querying Greenland  
## Creating a bounding box for Greenland 
# greenland_box <- c(xmin = -76.6, ymin = 57.3, xmax = -9.0, ymax = 84.0)
# # Turning 'greenland_box' into a bbox object
# greenland_bb  <- st_bbox(greenland_box)
# class(greenland_bb)
# # Defining the bbox for the OPenStreetMap request
# greenland_q <- opq(bbox = greenland_bb, timeout = 180)
# # Defining requests 
# greenland_qa <- add_osm_feature(greenland_q, key = 'lgbtq',value = 'primary')
# greenland_qb <- add_osm_feature(greenland_q, key = 'lgbtq',value = 'only')
# greenland_qc <- add_osm_feature(greenland_q, key = 'lgbtq',value = 'yes')
# greenland_qd <- add_osm_feature(greenland_q, key = 'gay',value = 'yes')
# greenland_qe <- add_osm_feature(greenland_q, key = 'gay',value = 'welcome')
# greenland_qf <- add_osm_feature(greenland_q, key = 'gay',value = 'only')
# # Requesting data 
# greenland_lgbtq_primary <- osmdata_sf(greenland_qa)
# greenland_lgbtq_only <- osmdata_sf(greenland_qb)
# greenland_lgbtq_yes <- osmdata_sf(greenland_qc)
# greenland_gay_yes <- osmdata_sf(greenland_qd)
# greenland_gay_welcome <- osmdata_sf(greenland_qe)
# greenland_gay_only <- osmdata_sf(greenland_qf)

# #=====> Querying French Guiana
## Creating a bounging box for French Guiana
# guiana_box <- c(xmin = -63.19, ymin = -0.33, xmax = -50.13, ymax = 9.59)
# # Turning 'guiana_box' into a bbox object
# guiana_bb  <- st_bbox(guiana_box)
# class(guiana_bb)
# # Defining the bbox for the OPenStreetMap request
# guiana_q <- opq(bbox = guiana_bb, timeout = 180)
# # Defining requests 
# guiana_qa <- add_osm_feature(guiana_q, key = 'lgbtq',value = 'primary')
# guiana_qb <- add_osm_feature(guiana_q, key = 'lgbtq',value = 'only')
# guiana_qc <- add_osm_feature(guiana_q, key = 'lgbtq',value = 'yes')
# guiana_qd <- add_osm_feature(guiana_q, key = 'gay',value = 'yes')
# guiana_qe <- add_osm_feature(guiana_q, key = 'gay',value = 'welcome')
# guiana_qf <- add_osm_feature(guiana_q, key = 'gay',value = 'only')
# # Requesting data 
# guiana_lgbtq_primary <- osmdata_sf(guiana_qa)
# guiana_lgbtq_only <- osmdata_sf(guiana_qb)
# guiana_lgbtq_yes <- osmdata_sf(guiana_qc)
# guiana_gay_yes <- osmdata_sf(guiana_qd)
# guiana_gay_welcome <- osmdata_sf(guiana_qe)
# guiana_gay_only <- osmdata_sf(guiana_qf)