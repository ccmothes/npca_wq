subsetATTAINSpark <- function(layer, type, path = 'data/'){
  
    # find all ATTAINS features within NPS boundaries
    nps_attains <- layer %>%
      .[parks,] 
    
    # get the raw ATTAINS area's area (m^2)...
    try(nps_attains$area_area <- sf::st_area(nps_attains, by_element = TRUE))
    
    # ... or, get the line's length (m)
    try(nps_attains$line_length <- sf::st_length(nps_attains, by_element = TRUE))
    
    # cut out ("clip") ATTAINS data within park units - remove all else
    nps_attains <- nps_attains %>%
      sf::st_intersection(., dplyr::select(parks, UNIT_CODE))
    
    # get the area (m^2) of the area within the park unit...
    try(nps_attains$in_park_area_area <- sf::st_area(nps_attains, by_element = TRUE))
    
    # ... or, get the length (m) of the ATTAINS line feature within the park unit
    try(nps_attains$in_park_line_length <- sf::st_length(nps_attains, by_element = TRUE))
    
    # transform ATTAINS points into "POINT" features
    if(type == "point") {nps_attains <- nps_attains %>% sf::st_cast(., "POINT")}
    
    # Save as RDS + shapefile
    saveRDS(nps_attains, paste0(path, '/00_nps_attains_park_', type, '.RDS'))
    sf::st_write(dplyr::select(nps_attains, UNIT_CODE, assessmentunitidentifier), paste0(path, '/00_nps_attains_park_', type, '.shp'), append = FALSE)
    
    return(nps_attains)
    
}