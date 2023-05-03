library(mapview)
library(tidyverse)

parks <- getParkUnits()

park_cats <- readRDS('data/00_nps_all_catchments.RDS')

cat <- readRDS('data/nhdplusv2_catchments.RDS')

appa <- read_csv('data/park_ws/APPA.csv')
sacn <- read_csv('data/park_ws/SACN.csv')
sucr <- read_csv('data/park_ws/SUCR.csv')
anti <- read_csv('data/park_ws/ANTI.csv')
jazz <- read_csv('data/park_ws/JAZZ.csv')

test <- cat %>%
  filter(FEATUREID %in% sucr$comid)

catchment <- readRDS('data/nhdplusv2_catchments.RDS') %>%
   filter(FEATUREID %in% park_cats$FEATUREID)

mapview(filter(parks, UNIT_CODE == "SUCR"))+mapview(filter(park_cats, UNIT_CODE == "SUCR")) + mapview(catchment)