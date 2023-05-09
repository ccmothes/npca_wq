library(tidyverse)
library(sf)

sf::sf_use_s2(FALSE)

source('1-src/getParkUnits.R')
source('1-src/loadATTAINS.R')

# Load in park boundaries:
park_boundaries <- getParkUnits()

# Pull in all water features in NHDPlus V2:
# layers <- sf::st_layers(dsn = 'data/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb')

nhd_flowlines <- sf::st_read(dsn = 'data/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb',
                             layer = "NHDFlowline_Network")

nhd_waterbody <- sf::st_read(dsn = 'data/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb',
                             layer = "NHDWaterbody")

nhd_other <- sf::st_read(dsn = 'data/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb',
                         layer = "NHDFlowline_NonNetwork")

colors <- data.frame(
  overallstatus = c("Not Supporting", "Fully Supporting", "Not Assessed", NA),
  col = c("#DC851E", "#059FA4", "#A1A522", "#C2CAD7"),
  Priority = c(1, 2, 3, 4)
)

attains_assmnt_parms <- loadATTAINS(path = 'data/ATTAINS',
                                    layer = "attains_au_assmnt_parms") %>%
  filter(parametercategorycode %in% c("5A","5","4C","4B", "4A")) %>%
  group_by(assessmentunitidentifier) %>%
  summarize(Impairments=as.character(list(unique(parametergroupname))))

attains_catchments <- loadATTAINS(path = 'data/ATTAINS', layer = "attains_au_catchments") %>%
  st_drop_geometry() %>%
  left_join(., colors, by = "overallstatus") %>%
  left_join(., attains_assmnt_parms, by = "assessmentunitidentifier")
  
contus_catchments <- readRDS('data/nhdplus_contus_catchments_3857.RDS') %>%
  sp::merge(attains_catchments, by.x = 'FEATUREID', by.y=  'nhdplusid', all.x = TRUE) 
  
# Identify catchments contained within park units that have a water feature WITHIN the park boundary:
# This is a necessary step for removing catchments whose contributing area it represents is for a 
# waterbody outside the park boundary. Examples: Antietam NP accidentally tracing the entire Potomac
# River watershed.
flows <- nhd_flowlines %>%
  sf::st_zm() %>%
  .[park_boundaries,] %>%
  sf::st_drop_geometry() %>%
  dplyr::select(comid=COMID)

bods <- nhd_waterbody %>%
  sf::st_zm() %>%
  .[park_boundaries,] %>%
  sf::st_drop_geometry() %>%
  dplyr::select(comid=COMID)

other <- nhd_other %>%
  sf::st_zm() %>%
  .[park_boundaries,] %>%
  sf::st_drop_geometry() %>%
  dplyr::select(comid=COMID)

# This represents COMIDs whose water feature actually exists in the park:
flows_in_parks <- bind_rows(flows, bods, other) %>%
  distinct(comid)

subset_sites <- readRDS('data/00_nps_all_catchments.RDS') %>%
  sf::st_drop_geometry() %>%
  rename(comid = FEATUREID) %>% 
  filter(comid %in% flows_in_parks$comid)

parks <- getParkUnits() %>%
  sf::st_drop_geometry() %>%
  distinct(UNIT_CODE) %>%
  .$UNIT_CODE

nhd <- read_csv('data/nhd_flow_network.csv')

for(i in 1:length(parks)){
  
  park_subset_sites <- subset_sites %>%
    dplyr::filter(UNIT_CODE == parks[i]) %>%
    distinct(comid, .keep_all = TRUE) %>%
    .$comid
  
  # watershed delineator function
  watersheder <- function(park_subset_sites){
    # create an empty tibble in case there is no upstream data
    upstream_nhd <- tibble(UNIT_CODE = parks[i],
                           value = NA)
    try(
      upstream_nhd <- nhdplusTools::get_UT(nhd, park_subset_sites) %>%
        as_tibble() %>%
        mutate(UNIT_CODE = parks[i]))
    write_csv(upstream_nhd, paste0('data/ws_full/', park_subset_sites, "_", parks[i], '.csv')
    )
    return(upstream_nhd)
  }
  
  thing <- tibble(UNIT_CODE = parks[i],
                  comid = NA)
  
  try(
    thing <- purrr::map(park_subset_sites, watersheder) %>%
      bind_rows() %>%
      dplyr::rename(comid = value) %>%
      dplyr::filter(!comid %in% park_subset_sites)
  )
  
  write_csv(thing, paste0('data/park_ws/', parks[i], '.csv'))
  

short <- str_sub(list.files('data/park_ws', full.names = FALSE) , start = 1, end = 4) 

for(i in 1:length(short)){

shorty <- read_csv(paste0('data/park_ws/', short[i], '.csv'))
contus_catchments %>%
    dplyr::filter(FEATUREID %in% shorty$comid) %>%
    saveRDS(paste0('data/park_ws_attains/', short[i], '.RDS'))
print(paste0(short[i], " watershed saved!"))
}
  
  print(paste0(parks[i], " complete!"))
  
  
}
