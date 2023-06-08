library(sf)
library(tidyverse)
sf_use_s2(FALSE)

boundaries <- st_read('data/in/nps_boundary/nps_boundary.shp') 
parks <- st_read('data/in/nps_boundary/nps_boundary.shp') %>%
  dplyr::filter(!STATE %in% c("AK", "HI", "GU", "VI", "MR", "PR")) %>%
  group_by(UNIT_CODE) %>%
  summarize() %>%
  .$UNIT_CODE

aus_in_huc <- function(hucs){
  
  aus <- httr::GET(paste0("https://attains.epa.gov/attains-public/api/huc12summary?huc=", hucs)) %>% 
    httr::content("text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(.,flatten = TRUE) %>%
    .[[1]] %>%
    .[[15]] %>%
    .[[1]] 
  
  return(aus)
  
}

park_data_finder <- function(parks){

  park <-boundaries %>%
  filter(UNIT_CODE == parks)

aus <- nhdplusTools::get_huc(AOI = park, type = "huc12") %>%
  .$huc12 %>%
  map(~aus_in_huc(.)) %>%
  bind_rows() %>%
  mutate(UNIT_CODE = park$UNIT_CODE)
saveRDS(aus, paste0('data/mid/unknowns/', park$UNIT_CODE, ".RDS"))
print(paste0(parks, " done!"))

return(aus)

}

data_in_hucs <- parks %>% 
  map(~park_data_finder(.)) %>%
  bind_rows()

attains_nps_table <- readRDS('shiny/data/attains_table.RDS')
attains_all_table <- readRDS('data/mid/attains_au_catchments.RDS')

data_w_geospatial_park <- data_in_hucs %>%
inner_join(attains_nps_table, by = c("assessmentUnitId" = "Assessment_Code")) %>%
  .$assessmentUnitId

data_maybe <- hucs %>%
  # remove au's with geospatial features:
  anti_join(attains_all_table, by = c("assessmentUnitId" = "assessmentunitidentifier")) %>%
  # for good measure, remove au's we already know exist in parks:
  anti_join(attains_nps_table, by = c("assessmentUnitId" = "Assessment_Code")) %>%
  # whatever else remains are AUs with no geospatial feature, that are within HUC12s that parks intersect.
  .$assessmentUnitId
 
############

# Download narrative description of these AUs without geospatial features:
missing_auer <- function(au_list){
  
  aus <- httr::GET(paste0("https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier=", au_list[1])) %>% 
    httr::content("text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(.,flatten = TRUE) %>%
    .[["items"]] %>%
    .[["assessmentUnits"]] %>%
    .[[1]]

  return(aus)
  
}

au_desc <- au_list  %>%
  map(~missing_auer(.)) %>%
  bind_rows() 
