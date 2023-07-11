library(sf)
library(tidyverse)
sf_use_s2(FALSE)
list.files('fxn/', full.names = TRUE) %>% map(~source(.))


boundaries <- st_read("data/in/nps_boundary.shp") %>%
  group_by(UNIT_CODE) %>%
  summarize()
parks <- st_read('data/in/nps_boundary.shp') %>%
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
  map(~park_data_finder(.))

data_in_hucs <- list.files('data/mid/unknowns/', full.names = TRUE) %>%
  map(~readRDS(.)) %>%
  bind_rows()

attains_nps_table <- readRDS('shiny/data/attains_table.RDS')
attains_all_table <- readRDS('data/mid/attains_au_catchments.RDS')

data_w_geospatial_park <- data_in_hucs %>%
inner_join(attains_nps_table, by = c("assessmentUnitId" = "Assessment_Code")) %>%
  .$assessmentUnitId

data_maybe <- data_in_hucs %>%
  # remove au's with geospatial features:
  anti_join(attains_all_table, by = c("assessmentUnitId" = "assessmentunitidentifier")) %>%
  # for good measure, remove au's we already know exist in parks:
  anti_join(attains_nps_table, by = c("assessmentUnitId" = "Assessment_Code")) %>%
  # whatever else remains are AUs with no geospatial feature, that are within HUC12s that parks intersect.
  .$assessmentUnitId %>%
  as_tibble() 
write_csv(data_maybe, 'data/mid/data_maybe.csv')

### USING THESE ASSESSMENT IDs TO FIND MISSING ONES FROM NEWEST VERSION: BASICALLY DO SAME WORKFLOW 

old_attains_points <- sf::st_read('data/in/ATTAINS/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_points") %>%
  filter(assessmentunitidentifier %in% data_maybe$value)

old_attains_areas <- sf::st_read('data/in/ATTAINS/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_areas") %>%
  filter(assessmentunitidentifier %in% data_maybe$value)

old_attains_lines <- sf::st_read('data/in/ATTAINS/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_lines") %>%
  filter(assessmentunitidentifier %in% data_maybe$value)





#####




# Load in park boundaries (from NPS IRMA)
parks <- sf::st_read("data/in/nps_boundary.shp") %>%
  # transform to the projection of the NHD:
  sf::st_transform(4269)

projector <- function(nhdhr){
  
  feature <- readRDS(nhdhr) 
  if(sf::st_crs(feature) != 3857) {feature <- feature %>% sf::st_transform(3857)}
  return(feature)
  
} 

# Subset and clip the ATTAINS geospatial data to only include assessment units within NPS park boundaries.

park_catch <- readRDS('data/mid/final_raw_catch.RDS') %>%
  sf::st_make_valid(.)

npca_regions <- sf::st_read("data/in/npca_regions.shp") 

parks <- sf::st_read("data/in/nps_boundary.shp") 

# Load in ATTAINS water quality standard parameter data:
attains_assmnt_parms <- readRDS("data/mid/attains_au_assmnt_parms.RDS")

# subset area features
nps_attains_areas <- old_attains_areas %>% 
  subsetATTAINS(layer = ., sub_by = park_catch, type = "area") %>%
  mutate(assessment_type = "AREA") %>%
  inner_join(., right_dims_areas, by = c("nhdplusid","assessmentunitidentifier"))

# subset line features
nps_attains_lines <- old_attains_lines %>%
  subsetATTAINS(layer = ., sub_by = park_catch, type = "line") %>%
  mutate(assessment_type = "LINE") %>%
  inner_join(., right_dims_lines, by = c("nhdplusid","assessmentunitidentifier"))

# subset point features (simple, no thresholds/clipping needed)
# Also, none exist within the park boundaries
nps_attains_points <- old_attains_points %>%
  subsetATTAINS(layer = ., sub_by = park_catch, type = "point") %>%
  mutate(assessment_type = "POINT")

# this represents all assessment units (areal, point, linear) that are physically within the park boundaries
# as a data frame
nps_all_attains <- bind_rows(nps_attains_areas, nps_attains_lines) %>%
  as_tibble(.) %>%
  select(., -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)) %>%
  dplyr::left_join(., attains_assmnt_parms,
                   by = "assessmentunitidentifier", multiple = "all")

# Next link all of this information to our park catchment layer so it can be geospatially represented:
old_nps_attains_to_catchment <- park_catch %>%
  as_tibble(.) %>%
  dplyr::inner_join(nps_all_attains,
                   by = c("nhdplusid", "UNIT_CODE"), multiple = "all") %>%
  dplyr::mutate(attains = ifelse(is.na(assessmentunitidentifier), "NO ATTAINS DATA", "ATTAINS DATA")) %>%
  sf::st_as_sf(.)

old_nps_attains_to_catchment$catchment_area_fin <- sf::st_area(old_nps_attains_to_catchment, by_element = TRUE)


updated_catchments <- readRDS('data/mid/nps_attains_catchment_geospatial.RDS') %>%
  filter()
####















############

# Download narrative description of these AUs without geospatial features:
missing_auer <- function(au_list){
  
  aus <- httr::GET(paste0("https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier=", au_list)) %>% 
    httr::content("text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(.,flatten = TRUE) %>%
    .[["items"]] %>%
    .[["assessmentUnits"]] %>%
    .[[1]]
saveRDS(aus, paste0('data/mid/unknowns/', au_list, "_description.RDS"))
  return(aus)
  
}

au_desc <- data_maybe  %>%
  map(~missing_auer(au_list = .)) %>%
  bind_rows() 
