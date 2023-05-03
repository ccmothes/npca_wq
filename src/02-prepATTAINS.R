# workflow for pulling all ATTAINS data within park units. Once assessment unit features have been found in 
# park units, we subset the holistic catchment ATTAINS to include only those catchment-assessment unit combos
# found through this exercise.

library(tidyverse)
library(sf)
library(mapview)
library(jsonlite)
library(rvest)
library(RODBC)
library(nngeo)

# Choose projection
projection <- 3857 # ATTAINS projection

# Load in all ATTAINS datasets
source('src/01-loadATTAINS.R')

# Load in function to pull in most up-to-date park boundaries
source('src/getParkUnits.R')

# Pull in park boundaries
parks <- getParkUnits() %>%
  sf::st_transform(projection) %>%
  dplyr::select(UNIT_CODE)

sf::st_crs(attains_areas)

# Pulling in an NHDPlus V2 shapefile of catchments
# (pre-downloaded):
nhdplus_contus_catchments <- readRDS('data/nhdplusv2_catchments.RDS')
sf::st_crs(nhdplus_contus_catchments)

## "ALL" GATHERING

# This workflow verifies that the catchments within park units actually have the geospatial feature 
# being assessed WITHIN the park boundary. NOTE TO SELF ADD A FIGURE OF A CATCHMENT WITH/WITHOUT AS AN EXAMPLE.

# Select and clip catchments within NPS park boundary shapefile:
nps_all_catchments <- nhdplus_contus_catchments %>%
  sf::st_transform(projection) %>%
  .[parks,]
# Get the catchments's area (m^2):
nps_all_catchments$catch_area <- sf::st_area(nps_all_catchments, by_element = TRUE) 
  # cut out ("clip") catchments within park units - remove all else
nps_all_catchments <- nps_all_catchments %>%
  sf::st_intersection(., parks) 
# Get the area of the catchment within the park unit:
nps_all_catchments$in_park_catch_area <- sf::st_area(nps_all_catchments, by_element = TRUE)
# Save as RDS + shapefile
saveRDS(nps_all_catchments, 'data/00_nps_all_catchments.RDS')
sf::st_write(select(nps_all_catchments, UNIT_CODE), 'data/00_nps_all_catchments.shp', append = FALSE)

# find all ATTAINS area features within NPS boundaries:
nps_attains_areas <- attains_areas %>%
  .[parks,] 
# Get the area's area (m^2):
nps_attains_areas$area_area <- sf::st_area(nps_attains_areas, by_element = TRUE)
  # cut out ("clip") ATTAINS areas within park units - remove all else
nps_attains_areas <- nps_attains_areas %>%
  sf::st_intersection(., nps_all_catchments)
# Get the area of the area within the park unit:
nps_attains_areas$in_park_area_area <- sf::st_area(nps_attains_areas, by_element = TRUE)
# Save as RDS + shapefile
saveRDS(nps_attains_areas, 'data/00_nps_attains_areas.RDS')
sf::st_write(dplyr::select(nps_attains_areas, UNIT_CODE), 'data/00_nps_attains_areas.shp', append = FALSE)

# find all ATTAINS line features within NPS boundaries:
nps_attains_lines <- attains_lines %>%
  .[parks,] 
# Get the line's length:
nps_attains_lines$line_length <- sf::st_length(nps_attains_lines, by_element = TRUE) 
  # cut out ("clip") ATTAINS line features within park units - remove all else
nps_attains_lines <- nps_attains_lines %>%
  sf::st_intersection(., nps_all_catchments)
# Get the length of the ATTAINS line feature within the park unit:
nps_attains_lines$in_park_line_length <- sf::st_length(nps_attains_lines, by_element = TRUE)
# Save as RDS + shapefile
saveRDS(nps_attains_lines, 'data/00_nps_attains_lines.RDS')
sf::st_write(dplyr::select(nps_attains_lines, UNIT_CODE), 'data/00_nps_attains_lines.shp', append = FALSE)

# find all ATTAINS point features within NPS boundaries:
nps_attains_points <- attains_points %>%
  .[parks,] %>%
  sf::st_intersection(., nps_all_catchments) %>%
  sf::st_cast(., "POINT")
# Save as RDS + shapefile
saveRDS(nps_attains_points, 'data/00_nps_attains_points.RDS')
sf::st_write(dplyr::select(nps_attains_points, UNIT_CODE), 'data/00_nps_attains_points.shp', append = FALSE)



## NON-CONTUS
# find all ATTAINS "catchment" features within NPS boundaries FOR PARKS NOT IN CONTUS:
nps_noncontus_all_catchments <- attains_catchments %>%
  dplyr::filter(state %in% c("AK", "GU", "HI", "VI", "PR", "AS")) %>%
  .[parks,]
# Get the catchments's area (m^2):
nps_noncontus_all_catchments$catch_area <- sf::st_area(nps_noncontus_all_catchments, by_element = TRUE) 
nps_noncontus_all_catchments <- nps_noncontus_all_catchments %>%
  sf::st_intersection(., parks) 
# Get the area of the area within the park unit:
nps_noncontus_all_catchments$in_park_catch_area <- sf::st_area(nps_noncontus_all_catchments, by_element = TRUE)
# Save as RDS + shapefile
saveRDS(nps_noncontus_all_catchments, 'data/00_nps_noncontus_all_pseudocatchments.RDS')
sf::st_write(dplyr::select(nps_noncontus_all_catchments, UNIT_CODE), 'data/00_nps_noncontus_all_pseudocatchments.shp', append = FALSE)

# find all ATTAINS area features within NPS boundaries FOR PARKS NOT IN CONTUS:
nps_noncontus_attains_areas <- attains_areas %>%
  dplyr::filter(state %in% c("AK", "GU", "HI", "VI", "PR", "AS")) %>%
  .[parks,] 
# Get the area's area (m^2):
nps_noncontus_attains_areas$area_area <- sf::st_area(nps_noncontus_attains_areas, by_element = TRUE) 
nps_noncontus_attains_areas <- nps_noncontus_attains_areas %>%
  sf::st_intersection(., nps_noncontus_all_catchments)
# Get the area of the area within the park unit:
nps_noncontus_attains_areas$in_park_area_area <- sf::st_area(nps_noncontus_attains_areas, by_element = TRUE)
# Save as RDS + shapefile
saveRDS(nps_noncontus_attains_areas, 'data/00_nps_noncontus_attains_areas.RDS')
sf::st_write(dplyr::select(nps_noncontus_attains_areas, UNIT_CODE), 'data/00_nps_noncontus_attains_areas.shp', append = FALSE)

# find all ATTAINS line features within NPS boundaries FOR PARKS NOT IN CONTUS:
nps_noncontus_attains_lines <- attains_lines %>%
  dplyr::filter(state %in% c("AK", "GU", "HI", "VI", "PR", "AS")) %>%
  .[parks,] 
# Get the line's length:
nps_noncontus_attains_lines$line_length <- sf::st_length(nps_noncontus_attains_lines, by_element = TRUE) 
nps_noncontus_attains_lines <- nps_noncontus_attains_lines %>%
  sf::st_intersection(., nps_noncontus_all_catchments)
# Get the length of the ATTAINS line feature within the park unit:
nps_noncontus_attains_lines$in_park_line_length <- sf::st_length(nps_noncontus_attains_lines, by_element = TRUE)
# Save as RDS + shapefile
saveRDS(nps_noncontus_attains_lines, 'data/00_nps_noncontus_attains_lines.RDS')
sf::st_write(dplyr::select(nps_noncontus_attains_lines, UNIT_CODE), 'data/00_nps_noncontus_attains_lines.shp', append = FALSE)

# find all ATTAINS point features within NPS boundaries FOR PARKS NOT IN CONTUS:
nps_noncontus_attains_points <- attains_points %>%
  dplyr::filter(state %in% c("AK", "GU", "HI", "VI", "PR", "AS")) %>%
  .[parks,] %>%
  sf::st_intersection(., nps_noncontus_all_catchments) %>%
  sf::st_cast(., "POINT")
# Save as RDS + shapefile
saveRDS(nps_noncontus_attains_points, 'data/00_nps_noncontus_attains_points.RDS')
sf::st_write(dplyr::select(nps_noncontus_attains_points, UNIT_CODE), 'data/00_nps_noncontus_attains_points.shp', append=FALSE)