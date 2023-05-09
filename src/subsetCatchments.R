subsetCatchments <- function(layer, path = 'data/'){
  
  # Select and clip catchments within NPS park boundary shapefile:
  nps_all_catchments <- layer %>%
    .[parks,]
  # # Get the catchments's area (m^2):
  # nps_all_catchments$catch_area <- sf::st_area(nps_all_catchments, by_element = TRUE) 
  # cut out ("clip") catchments within park units - remove all else
  nps_all_catchments <- nps_all_catchments %>%
    sf::st_intersection(., parks)
  # Get the area of the catchment within the park unit:
  # nps_all_catchments$in_park_catch_area <- sf::st_area(nps_all_catchments, by_element = TRUE)
    
  # Save as RDS + shapefile
  saveRDS(nps_all_catchments, paste0(path, '/00_nps_all_catchments.RDS'))
  sf::st_write(select(nps_all_catchments, UNIT_CODE), paste0(path, '/00_nps_all_catchments.shp'), append = FALSE)
  
  return(nps_all_catchments)

  }
