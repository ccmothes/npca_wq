sf::sf_use_s2(FALSE)


# Load in all raw geospatial ATTAINS data:
attains_areas <- readRDS('data/mid/attains_au_areas.RDS') 

attains_lines <- readRDS('data/mid/attains_au_lines.RDS')

attains_points <- readRDS('data/mid/attains_au_points.RDS')

# Load in ATTAINS water quality standard parameter data:
attains_assmnt_parms <- readRDS("data/mid/attains_au_assmnt_parms.RDS")

raw_ws <- list.files('data/mid/park_outside/', full.names = TRUE)

data_puller <- function(raw_ws){
  
  ws <- readRDS(raw_ws) %>%
    dplyr::select(UNIT_CODE) %>%
    st_make_valid() 
    
 try(ws <- ws %>% sf::st_cast("POLYGON"))
  
  areas <- st_zm(attains_points) %>%
    sf::st_intersection(., st_zm(ws)) %>%
    left_join(attains_assmnt_parms, by = 'assessmentunitidentifier')
  
print(paste0(raw_ws, " done!"))
  return(areas)
  
}

attains_areas <- raw_ws %>%
  map(~data_puller(.)) %>%
  bind_rows()
saveRDS(attains_areas, 'data/mid/attains_ws_areas.RDS')

attains_lines <- raw_ws %>%
  map(~data_puller(.)) %>%
  bind_rows()
saveRDS(attains_lines, 'data/mid/attains_ws_lines.RDS')

attains_points <- raw_ws %>%
  map(~data_puller(.)) %>%
  bind_rows()
saveRDS(attains_lines, 'data/mid/attains_ws_points.RDS')


