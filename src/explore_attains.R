library(sf)

attains_layers <- sf::st_layers(dsn = 'data/ATTAINS_Assessment_20220809.gpkg')

areas <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_areas")

lines <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_lines")

points <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_points")

catchments <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_catchments")

attributes <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_attributes")

control <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_control")

catchassmnt <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_catchassmnt")

assmnt_parms <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_assmnt_parms")

water_types <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_water_types")

meta <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', layer = "attains_au_meta")

# How frequently does a single catchment (i.e. nhdplusid) have more than one assessment unit in it? How are they represented?
mult_aus <- catchments %>%
  group_by(nhdplusid) %>%
  summarize(n = n_distinct(assessmentunitidentifier))
  







