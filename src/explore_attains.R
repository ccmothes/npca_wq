library(sf)
# 
# attains_layers <- sf::st_layers(dsn = 'data/ATTAINS_Assessment_20220809.gpkg')
# 
# areas <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_areas")
# 
# lines <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_lines")
# 
# points <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_points")
# 
# catchments <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_catchments")
# 
# attributes <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_attributes")
# 
# control <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_control")
# 
# catchassmnt <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_catchassmnt")
# 
# assmnt_parms <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_assmnt_parms")
# 
# water_types <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_water_types")
# 
# meta <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_meta")
# 
# # How frequently does a single catchment (i.e. nhdplusid) have more than one assessment unit in it? How are they represented?
# mult_aus <- catchments %>%
#   st_drop_geometry() %>%
#   group_by(nhdplusid) %>%
#   summarize(n = n_distinct(assessmentunitidentifier))
# 
# 100* (n_distinct(filter(mult_aus, n>1))/nrow(mult_aus))
# # 14 percent of all assessed catchments have more than one assessment unit in them.
# 
# # how are those identified in the water_types data set?
# mult_aus <- catchments %>%
#   st_drop_geometry() %>%
#   group_by(nhdplusid) %>%
#   mutate(n = n_distinct(assessmentunitidentifier))
# 
# deeper <- mult_aus %>%
#   filter(n > 1) %>%
#   left_join(., water_types, by = "assessmentunitidentifier", multiple = "all")
# 
# # are there multiple water types per assessment unit...?
# nrow(water_types) - n_distinct(water_types$assessmentunitidentifier)
# 
# # I would not have expected there to be any AUs with more that one "water type"... what are these?
# mult_water <- water_types %>%
#   group_by(assessmentunitidentifier) %>%
#   filter(n_distinct(watertype) > 1)
# # they seem to be instances where an AU is an entire watershed. So, how are these areas/miles defined?
# # it's hard to say... here's an example of such a location: NJ02040302050110-01
# 
# greg_catch <- catchments %>% filter(assessmentunitidentifier == "NJ02040302050110-01")
# greg_points <- points %>% filter(assessmentunitidentifier == "NJ02040302050110-01")
# greg_lines <- lines %>% filter(assessmentunitidentifier == "NJ02040302050110-01")
# greg_areas <- areas %>% filter(assessmentunitidentifier == "NJ02040302050110-01")
# mapview(greg_areas) + mapview(greg_catch)
# sum(greg_catch$catchmentareasqkm) * 247.105 # assessed catchment acreage
