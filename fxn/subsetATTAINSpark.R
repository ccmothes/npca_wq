# parks or catchments

subsetATTAINSpark <- function(layer, sub_by = parks,  type, path = 'data/mid/'){
  
    # if(exists("parks") == FALSE) {parks <- getParkUnits(projection = 3857)}
    
  # find all ATTAINS features within NPS boundaries
    nps_attains <- layer %>%
      .[parks,] 
    
    print("Only ATTAINS in parks")

    # get the raw ATTAINS area's area (m^2)...
    try(nps_attains$area_area <- sf::st_area(nps_attains, by_element = TRUE))
    
    # ... or, get the line's length (m)
    try(nps_attains$line_length <- sf::st_length(nps_attains, by_element = TRUE))
    
    # cut out ("clip") ATTAINS data within park units - remove all else
    nps_attains <- nps_attains %>%
      sf::st_intersection(., sub_by)
    
    # get the area (m^2) of the area within the park unit...
    try(nps_attains$in_park_area_area <- sf::st_area(nps_attains, by_element = TRUE))
    
    # ... or, get the length (m) of the ATTAINS line feature within the park unit
    try(nps_attains$in_park_line_length <- sf::st_length(nps_attains, by_element = TRUE))
    
    # transform ATTAINS points into "POINT" features
    if(type == "point") {nps_attains <- nps_attains %>% sf::st_cast(., "POINT")}
    
    nps_attains <- nps_attains %>%
      sf::st_join(., npca_regions, left = TRUE) %>%
      # weird coastal points
      mutate(office = ifelse(is.na(office) & state =="TX", "Texas",
                      ifelse(is.na(office) & state =="MN", "Midwest", office)))
    
    # Save as RDS + shapefile
    #saveRDS(nps_attains, paste0(path, '/nps_attains_catchment_', type, '.RDS'))
    
    # nps_attained <- nps_attains %>% 
    # 
    #   dplyr::select(UNIT_CODE, assessmentunitidentifier, Shape) %>%
    #   sf::st_cast("MULTIPOLYGON")
      
    # sf::st_write(nps_attained, paste0(path, '/nps_attains_catchment_', type, '.shp'), 
    #                                                                                      append = FALSE)
    print(paste0(type, " complete!"))
    
    return(nps_attains)
    
}
