library(tidyverse)

npca_regions <- getNPCA()  

# remove unnecessary columns
col_remove <- c("GRIDCODE", "submissionid", "GLOBALID", "orig_fid", "SOURCEFC", "Shape", "Shape_Area", "Shape_Length", "AreaSqKM")

nps_attains_points <- readRDS('data/00_nps_attains_points.RDS') %>% dplyr::select(-col_remove) %>% dplyr::select(c(UNIT_CODE, FEATUREID, everything(.))) %>%
  #sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "POINT")
nps_attains_lines <- readRDS('data/00_nps_attains_lines.RDS') %>% dplyr::select(-col_remove) %>% dplyr::select(c(UNIT_CODE, FEATUREID, everything(.))) %>%
  #sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "LINE")
nps_attains_areas <- readRDS('data/00_nps_attains_areas.RDS') %>% dplyr::select(-col_remove) %>% dplyr::select(c(UNIT_CODE, FEATUREID, everything(.))) %>%
  #sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "AREA")
# this represents all assessment units that are physically within the park boundaries
nps_all_attains <- bind_rows(nps_attains_areas, nps_attains_lines, nps_attains_points) 

# next, we subset the holistic "attains_catchments" metadata that represents the `nps_all_attains` data:
# i.e., select only assessment unit/catchment combos that actually exist within the park
nps_all_attains_catchments <- attains_catchments %>%
  sf::st_drop_geometry() %>%
  dplyr::inner_join(., select(nps_all_attains, FEATUREID, assessmentunitidentifier, assessment_type,
                              catch_area, in_park_catch_area, area_area, in_park_area_area, line_length, in_park_line_length),
                    by=c("nhdplusid" = "FEATUREID", "assessmentunitidentifier"), multiple = "all") %>%
  dplyr::filter(!state %in% c("AK", "GU", "HI", "VI", "PR", "AS"))

# ... and then join this information to our catchment shapefile:
nps_all_attains_data <- readRDS('data/00_nps_all_catchments.RDS') %>%
  dplyr::select(UNIT_CODE, FEATUREID) %>%
  dplyr::left_join(nps_all_attains_catchments, by = c("FEATUREID" = "nhdplusid"), multiple = "all") %>%
  # join up table that shows what parameters were assessed in each assessment unit 
  dplyr::left_join(
    # remove duplicate/unnecessary columns:
    dplyr::select(attains_assmnt_parms, -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
    by = "assessmentunitidentifier", multiple = "all") %>%
  # if there is no ATTAINS data associated with the catchment, that means there is no geospatial
  # ATTAINS info in that catchment's area within the park
  dplyr::mutate(NO_DATA = ifelse(is.na(submissionid) & is.na(GLOBALID), "NO ATTAINS DATA", "ATTAINS DATA")) %>%
  sf::st_join(., npca_regions, left = TRUE) 
saveRDS(nps_all_attains_data, 'data/00_nps_all_attains_data.RDS')
sf::st_write(nps_all_attains_data, 'data/00_nps_all_attains_data.shp', append = FALSE)


# proof <- nps_all_attains_data %>%
#   filter(submissionid.x != submissionid.y |
#            assmnt_joinkey.x != assmnt_joinkey.y |
#            organizationid.x != organizationid.y |
#            orgtype.x != orgtype.y |
#            tas303d.x != tas303d.y |
#            reportingcycle.x != reportingcycle.y |
#            waterbodyreportlink.x != waterbodyreportlink.y)

# Visually is this layer actually what I want it to be?
# test_attains_data <- nps_all_attains_data %>%
#   filter(UNIT_CODE %in% c("SHEN","WASH")) #%>% filter(FEATUREID==5908449)
# test_lines <- nps_attains_lines <- readRDS('data/00_nps_attains_lines.RDS') %>% filter(UNIT_CODE %in% c("SHEN","WASH")) #%>% filter(FEATUREID==13570)
# test_areas <- nps_attains_areas <- readRDS('data/00_nps_attains_areas.RDS') %>% filter(UNIT_CODE %in% c("SHEN","WASH")) #%>% filter(FEATUREID==13570)
# 
# mapview::mapview(test_attains_data, zcol = "NO_DATA", col.regions = c("blue","red")) + mapview::mapview(test_lines) #+ mapview::mapview(test_areas)
# # ... I THINK IT IS YAYYYYY!!!!!!!

all_parks <- getParkUnits() %>% 
  sf::st_drop_geometry()

# which parks don't have any assessment data?
park_data <- nps_all_attains_data %>%
  dplyr::group_by(UNIT_CODE, office) %>%
  dplyr::summarize(ir_cats = as.character(list(unique(parametercategorycode)))) %>%
  dplyr::filter(is.na(ir_cats)) 

# which parks have impairments, and what are they?
park_impairments <- nps_all_attains_data %>%
  # IR categories for "impaired" waters:
  dplyr::filter(parametercategorycode %in% c("4A", "4C", "5", "4B", "5A")) %>%
  dplyr::mutate(nice = paste0(parametergroupname,' [', parametercategorycode, "]")) %>%
  dplyr::group_by(UNIT_CODE, office) %>%
  dplyr::summarize(impairment_list = as.character(list(unique(nice))))

# parks with only "good" water quality
parks_no_impairments <- nps_all_attains_data %>%
  dplyr::filter(!UNIT_CODE %in% park_impairments$UNIT_CODE) 
#STILL NEEDS WORK ^^
  
total_park_catchments <- nps_all_attains_data %>%
  n_distinct(.$FEATUREID)

catch_impairments <- nps_all_attains_data %>%
  dplyr::filter(parametercategorycode %in% c("4A", "4C", "5", "4B", "5A")) %>%
  dplyr::distinct(FEATUREID, parametergroupname) %>%
  dplyr::group_by(parametergroupname) %>%
  dplyr::summarize(total_catchments = n())
#
catch_good <- nps_all_attains_data %>%
  dplyr::filter(!FEATUREID %in% impaired_catchments$FEATUREID)

# A null value in the isThreatened column is the same as isThreatened = 'N'
