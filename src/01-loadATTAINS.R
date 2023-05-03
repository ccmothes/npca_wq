# ATTAINS LAYERS TO BE LOADED IN
path <- list.files('data/ATTAINS', full.names = TRUE)[1]

attains_layers <- sf::st_layers(dsn = path)

attains_areas <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg',
                             layer = "attains_au_areas")

attains_lines <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', 
                             layer = "attains_au_lines")

attains_points <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', 
                              layer = "attains_au_points")

attains_catchments <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg', 
                                  layer = "attains_au_catchments")

attains_attributes <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg',
                                  layer = "attains_au_attributes")

attains_control <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg',
                               layer = "attains_au_control")

attains_catchassmnt <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg',
                                   layer = "attains_au_catchassmnt")

attains_assmnt_parms <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg',
                                    layer = "attains_au_assmnt_parms")

attains_water_types <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg',
                                   layer = "attains_au_water_types")

attains_meta <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg',
                            layer = "attains_au_meta")


