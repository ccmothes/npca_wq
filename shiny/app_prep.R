library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)

colors = data.frame(
  Assessment_Category = c("Impaired", "Good", "Unknown", "No State Data", "No Water Features"),
  col = c("#DC851E", "#059FA4", "#A1A522", "#C2CAD7", NA),
  dark_col = c("#813B00", "#005258", "#4F5900", "#5A616D", NA),
  Priority = c(1, 2, 3, 4, 5))

parks <- sf::st_read('data/in/nps_boundary/nps_boundary.shp') %>%
  st_drop_geometry() %>%
  select(UNIT_CODE, STATE, UNIT_NAME) %>%
  group_by(UNIT_CODE) %>%
  summarize(names = as.character(list(unique(UNIT_NAME))),
            states = as.character(list(unique(STATE))),
            count = n()) %>%
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                     ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                     ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                     ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                     ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                     ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                     ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                     ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                     ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", names))))))))),     
         STATE = ifelse(UNIT_CODE == "SAGU", "CA", states)) %>%
  select(UNIT_CODE, UNIT_NAME, STATE)
                        
# parks <- sf::st_read('data/in/nps_boundary/nps_boundary.shp') %>%
#   st_drop_geometry() %>%
#   select(UNIT_CODE, STATE, UNIT_NAME) %>%
#   group_by(UNIT_CODE) %>%
#   summarize()
#   distinct(UNIT_CODE, .keep_all = TRUE)

flowlines <- readRDS('data/mid/nhdhr_park_flow.RDS') %>%
  group_by(UNIT_CODE) %>%
  summarize() %>%
  left_join(parks, by = "UNIT_CODE") %>%
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326) 
saveRDS(flowlines, 'shiny/data/map_flowlines.RDS')
areas <- readRDS('data/mid/nhdhr_park_area.RDS') %>%
  group_by(UNIT_CODE) %>%
  summarize() %>%
  left_join(parks, by = "UNIT_CODE") %>%
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326) 
saveRDS(areas, 'shiny/data/map_areas.RDS')
waterbodies <- readRDS('data/mid/nhdhr_park_waterbody.RDS') %>%
  group_by(UNIT_CODE) %>%
  summarize() %>%
  sf::st_simplify(dTolerance = 10) %>%
  left_join(parks, by = "UNIT_CODE") %>% 
  st_transform(4326) 
saveRDS(waterbodies, 'shiny/data/map_waterbodies.RDS')

# Lastly remove all data linked to parks without water features in them (CONTUS only):
no_water <- parks %>%
  filter(!STATE %in% c("AK","HI","PR","VI","AS","GU","MP")) %>%
  # this is a weird lil park in the Middle of Lake Erie:
  filter(UNIT_CODE != "PEVI") %>%
  # clipped water features from the NHD HR:
  filter(!UNIT_CODE %in% readRDS('data/mid/nhdhr_park_area.RDS')$UNIT_CODE) %>%
  filter(!UNIT_CODE %in% readRDS('data/mid/nhdhr_park_flow.RDS')$UNIT_CODE) %>%
  filter(!UNIT_CODE %in% readRDS('data/mid/nhdhr_park_waterbody.RDS')$UNIT_CODE) %>%
  bind_rows(filter(parks, UNIT_NAME %in% read_csv('data/in/no_water_parks.csv')$UNIT_NAME)) %>%
  distinct(UNIT_NAME, .keep_all = TRUE) %>%
  mutate(hydro = "No Water Features")

parks <- parks 

# CATCHMENT DATA #

park_catch <- readRDS('data/mid/final_raw_catch.RDS') %>%
  left_join(., parks, by = "UNIT_CODE") %>%
  mutate(nhdplusid = as.character(nhdplusid)) %>%
  select(UNIT_CODE, UNIT_NAME, nhdplusid, CATCH_TYPE)

park_empty_catch_1 <- park_catch %>%
  .[readRDS('data/mid/nhdhr_park_area.RDS'),]

park_empty_catch_2 <- park_catch %>%
  .[readRDS('data/mid/nhdhr_park_flow.RDS'),]

park_empty_catch_3 <- park_catch %>%
  .[readRDS('data/mid/nhdhr_park_waterbody.RDS'),]

empty_catch <- park_catch %>% 
  filter(CATCH_TYPE == "NHDPlusHR") %>%
  filter(!nhdplusid %in% park_empty_catch_1$nhdplusid) %>%
  filter(!nhdplusid %in% park_empty_catch_2$nhdplusid) %>%
  filter(!nhdplusid %in% park_empty_catch_3$nhdplusid)


nps_all_attains <- readRDS('data/mid/nps_attains_catchment_table.RDS') %>%
  left_join(., parks, by = "UNIT_CODE")  %>%
  mutate(nhdplusid = as.character(nhdplusid)) %>%
  select(UNIT_NAME, office, nhdplusid, parametername, ircategory, parametercategorycode)

impaired_list <- nps_all_attains %>%
  dplyr::filter(parametercategorycode %in% c("5A","5","4C","4B","4A")) %>% 
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(nhdplusid, UNIT_NAME) %>%
  summarize(Impairments = as.character(list(unique(parametername))),
            Assessment_Category = "Impaired",
            Office = list(unique(office)))

good_list <- nps_all_attains %>%
  dplyr::filter(ircategory %in% c("1","2")) %>% 
  dplyr::filter(!nhdplusid %in% impaired_list$nhdplusid) %>%
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(nhdplusid, UNIT_NAME) %>%
  summarize(Impairments = "None",
            Assessment_Category = "Good",
            Office = list(unique(office)))

unknown_list <- nps_all_attains %>%
  dplyr::filter(ircategory %in% c("3")) %>% 
  dplyr::filter(!nhdplusid %in% impaired_list$nhdplusid) %>%
  dplyr::filter(!nhdplusid %in% good_list$nhdplusid) %>%
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(nhdplusid, UNIT_NAME) %>%
  summarize(Impairments = "Unknown",
            Assessment_Category = "Unknown",
            Office = list(unique(office)))

attains <- bind_rows(impaired_list, good_list, unknown_list)

for_app <- park_catch %>%
  left_join(., attains, by=c("nhdplusid", "UNIT_NAME")) %>% 
  mutate(Assessment_Category =  ifelse(UNIT_NAME %in% no_water$UNIT_NAME, "No Water Features",
                                       ifelse(is.na(Assessment_Category) & nhdplusid %in% empty_catch$nhdplusid, "No Water Features",
                                              ifelse(is.na(Assessment_Category) & !nhdplusid %in% empty_catch$nhdplusid, "No State Data", Assessment_Category)))) %>%
  sp::merge(colors, by ="Assessment_Category") %>%
  dplyr::rename(Park = UNIT_NAME) %>% 
  left_join(parks, by=c("Park" = "UNIT_NAME")) %>%
  filter(!STATE %in% c("AK","HI","PR","VI","AS","GU","MP")) %>%
  st_transform(4326) 

for_app$catchment_area <- sf::st_area(for_app, by_element = TRUE)

for_app <- for_app %>%
  sf::st_simplify(dTolerance = 0.0001)

saveRDS(for_app, 'shiny/data/for_app.RDS')


# PARK ATTAINS #

park_point_impaired <- readRDS('shiny/data/nps_attains_park_point.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(parametercategorycode %in% c("5A","5","4C","4B","4A")) %>% 
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = as.character(list(unique(parametername))),
            Assessment_Category = "Impaired",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  mutate(Impairments = gsub('"', "", Impairments)) %>%
  mutate(Impairments = gsub("^c\\(|\\)$", "", Impairments)) %>%
  left_join(colors, by = "Assessment_Category") %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_point_good <- readRDS('shiny/data/nps_attains_park_point.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(!assessmentunitidentifier %in% park_point_impaired$assessmentunitidentifier) %>%
  dplyr::filter(ircategory %in% c("1","2")) %>% 
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = "None",
            Assessment_Category = "Good",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  left_join(colors, by = "Assessment_Category") %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_point_unknown <- readRDS('shiny/data/nps_attains_park_point.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(!assessmentunitidentifier %in% park_point_impaired$assessmentunitidentifier) %>%
  dplyr::filter(!assessmentunitidentifier %in% park_point_good$assessmentunitidentifier) %>%
  dplyr::filter(ircategory %in% c("3")) %>% 
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = "Unknown",
            Assessment_Category = "Unknown",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  left_join(colors, by = "Assessment_Category") %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_points <- bind_rows(park_point_impaired, park_point_good) %>% #, park_point_unknown) %>%
  filter(!Park %in% no_water$UNIT_NAME)
saveRDS(park_points, 'shiny/data/points.RDS')

# AREA #

park_area_impaired <- readRDS('shiny/data/nps_attains_park_area.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(parametercategorycode %in% c("5A","5","4C","4B","4A")) %>% 
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = as.character(list(unique(parametername))),
            Assessment_Category = "Impaired",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  mutate(Impairments = gsub('"', "", Impairments)) %>%
  mutate(Impairments = gsub("^c\\(|\\)$", "", Impairments)) %>%
  left_join(colors, by = "Assessment_Category") %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_area_good <- readRDS('shiny/data/nps_attains_park_area.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(!assessmentunitidentifier %in% park_area_impaired$assessmentunitidentifier) %>%
  dplyr::filter(ircategory %in% c("1","2")) %>% 
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = "None",
            Assessment_Category = "Good",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  left_join(colors, by = "Assessment_Category") %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_area_unknown <- readRDS('shiny/data/nps_attains_park_area.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(!assessmentunitidentifier %in% park_area_impaired$assessmentunitidentifier) %>%
  dplyr::filter(!assessmentunitidentifier %in% park_area_good$assessmentunitidentifier) %>%
  dplyr::filter(ircategory %in% c("3")) %>%
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = "Unknown",
            Assessment_Category = "Unknown",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  left_join(colors, by = "Assessment_Category") %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_areas <- bind_rows(park_area_impaired, park_area_good, park_area_unknown) %>%
  filter(!Park %in% no_water$UNIT_NAME)
saveRDS(park_areas, 'shiny/data/areas.RDS')

# LINE #

park_line_impaired <- readRDS('shiny/data/nps_attains_park_line.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(parametercategorycode %in% c("5A","5","4C","4B","4A")) %>% 
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = as.character(list(unique(parametername))),
            Assessment_Category = "Impaired",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  left_join(colors, by = "Assessment_Category") %>%
  mutate(Impairments = gsub('"', "", Impairments)) %>%
  mutate(Impairments = gsub("^c\\(|\\)$", "", Impairments)) %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_line_good <- readRDS('shiny/data/nps_attains_park_line.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(!assessmentunitidentifier %in% park_line_impaired$assessmentunitidentifier) %>%
  dplyr::filter(ircategory %in% c("1","2")) %>% 
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = "None",
            Assessment_Category = "Good",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  left_join(colors, by = "Assessment_Category") %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_line_unknown <- readRDS('shiny/data/nps_attains_park_line.RDS') %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(readRDS('data/mid/attains_au_assmnt_parms.RDS'), -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(!assessmentunitidentifier %in% park_line_impaired$assessmentunitidentifier) %>%
  dplyr::filter(!assessmentunitidentifier %in% park_line_good$assessmentunitidentifier) %>%
  dplyr::filter(ircategory %in% c("3")) %>% 
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  
  filter(!UNIT_NAME %in% no_water$UNIT_NAME) %>%
  group_by(assessmentunitidentifier, UNIT_NAME) %>%
  summarize(Impairments = "Unknown",
            Assessment_Category = "Unknown",
            Link = as.character(list(unique(waterbodyreportlink)))) %>% 
  left_join(colors, by = "Assessment_Category") %>%
  select(Park = UNIT_NAME,
         assessmentunitidentifier,
         Assessment_Category,
         Link,
         Impairments,
         col, 
         dark_col) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)

park_lines <- bind_rows(park_line_impaired, park_line_good, park_line_unknown) %>%
  filter(!Park %in% no_water$UNIT_NAME)

saveRDS(park_lines, 'shiny/data/lines.RDS')

# ORW DATA #

orw <- readRDS('data/out/nhdhr_park_flows_and_orw.RDS') %>%
  filter(!is.na(Tier)) %>%
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  select(Tier, Park = UNIT_NAME, UNIT_CODE) %>% 
  sf::st_simplify(dTolerance = 10) %>%
  st_transform(4326)
saveRDS(orw, 'shiny/data/orw.RDS')

###

npca_regions <- sf::st_read("data/in/npca_regions.shp") 

parks <- sf::st_read('data/in/nps_boundary/nps_boundary.shp') %>%
  st_transform(3857) %>%
  st_join(npca_regions) %>%
  st_drop_geometry() %>%
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  group_by(UNIT_NAME) %>%
  summarize(NPCA_Region = list(unique(office))) %>%
  mutate(NPCA_Region = gsub('"', "", NPCA_Region)) %>%
  mutate(NPCA_Region = gsub("^c\\(|\\)$", "", NPCA_Region)) %>%
  mutate(NPCA_Region = ifelse(is.na(NPCA_Region), "Pacific", NPCA_Region))

attains_table <- bind_rows(st_drop_geometry(park_lines), st_drop_geometry(park_areas)) %>%
  bind_rows(st_drop_geometry(park_points)) %>%
  mutate(Impairments = gsub('"', "", Impairments)) %>%
  mutate(Impairments = gsub("^c\\(|\\)$", "", Impairments)) %>%
  left_join(., parks, by=c("Park"="UNIT_NAME")) %>%
  select(Park,
         NPCA_Region,
         Assessment_Code = assessmentunitidentifier,
         Assessment_Category,
         Impairments,
         URL = Link) 
saveRDS(attains_table, 'shiny/data/attains_table.RDS')

# PARK-WIDE IMPAIRMENT MAPPER

parkwide_all_attains <- readRDS('data/out/nps_all_wq_data_final.RDS') %>%
  dplyr::filter(parametercategorycode %in% c("5A","5","4C","4B", "4A")) %>%
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  
  mutate(parametergroupname = ifelse(parametergroupname == "METALS (OTHER THAN MERCURY)", "METALS OTHER THAN HG",
                              ifelse(parametergroupname == "POLYCHLORINATED BIPHENYLS (PCBS)", "POLYCHLORINATED BIPHENYLS - PCBS", parametergroupname))) %>%
  select(UNIT_NAME, Impairments = parametergroupname) %>%
  group_by(UNIT_NAME) %>%
  summarize(#NPCA_Region = list(unique(office)),
    Impairments = list(unique(Impairments))) %>%
  mutate(Impairments = gsub('"', "", Impairments)) %>%
  mutate(Impairments = gsub("^c\\(|\\)$", "", Impairments))

nps_points <-  sf::st_read('data/in/nps_boundary/nps_boundary.shp') %>%
  mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                                   ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                                          ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                                                 ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                                                        ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                                                               ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                                                                      ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                                                                             ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  #left_join(st_drop_geometry(sf::st_read('data/in/nps_boundary/nps_boundary.shp'))) %>%
  group_by(UNIT_NAME) %>%
  summarize() %>%
  left_join(parkwide_all_attains, by = "UNIT_NAME") %>%
  select(Park = UNIT_NAME,
         Impairments) %>%
  sf::st_centroid() %>%
  left_join(distinct(select(readRDS('shiny/data/attains_table.RDS'), Park, NPCA_Region)), by = "Park") %>%
  st_transform(4326)
saveRDS(nps_points, 'shiny/data/nps_points.RDS')

# Testing functionality of all files going into the app:

sf::sf_use_s2(FALSE)

boundary_lines <- nps_points <-  sf::st_read('data/in/nps_boundary/nps_boundary.shp') %>%
  dplyr::mutate(UNIT_NAME = ifelse(UNIT_CODE == "ANIA", "Aniakchak National Monument and Preserve",
                            ifelse(UNIT_CODE == "DENA", "Denali National Park and Preserve",
                            ifelse(UNIT_CODE == "GAAR", "Gates of the Arctic National Park and Preserve",
                            ifelse(UNIT_CODE == "GLBA", "Glacier Bay National Park and Preserve",
                            ifelse(UNIT_CODE == "GRSA", "Great Sand Dunes National Park and Preserve",
                            ifelse(UNIT_CODE == "KATM", "Katmai National Park and Preserve",
                            ifelse(UNIT_CODE == "LACL", "Lake Clark National Park and Preserve",
                            ifelse(UNIT_CODE == "SAGU", "Santa Monica Mountains National Park and Recreation Area",
                            ifelse(UNIT_CODE == "WRST", "Wrangell-St. Elias National Park and Preserve", UNIT_NAME)))))))))) %>%
  group_by(UNIT_NAME) %>%
  summarize() %>%
  rename(Park = UNIT_NAME) %>%
  st_zm() %>%
  sf::st_cast("MULTILINESTRING") %>%
  st_transform(4326)
saveRDS(boundary_lines, 'shiny/data/nps_boundary_lines.RDS')
# Catchment boundaries
inside <- readRDS('shiny/data/for_app.RDS') 

# ATTAINS
lines <- readRDS('shiny/data/lines.RDS') 
areas <- readRDS('shiny/data/areas.RDS')
points <- readRDS('shiny/data/points.RDS')
# Download Table
attains_data <- readRDS('shiny/data/attains_table.RDS')

#ORWs
orw <- readRDS('shiny/data/orw.RDS')

# For park mapper
nps_points <- readRDS('shiny/data/nps_points.RDS')

# NHD
nhd_lines <- readRDS('shiny/data/map_flowlines.RDS')
nhd_waterbodies <- readRDS('shiny/data/map_waterbodies.RDS')
nhd_areas <- readRDS('shiny/data/map_areas.RDS')

#ATTAINS Watershed
ws_lines <-readRDS('shiny/data/ws_lines.RDS')
ws_areas <-readRDS('shiny/data/ws_areas.RDS')
ws_points <- readRDS('shiny/data/ws_points.RDS')
