loadCatchments <- function(projection = 3857, path = 'data/'){
  
  nhdplus_contus_catchments <- readRDS('data/nhdplusv2_catchments.RDS') %>%
    sf::st_transform(projection) 
  saveRDS(nhdplus_contus_catchments, paste0(path, '/nhdplus_contus_catchments_', projection,'.RDS'))
  
  return(nhdplus_contus_catchments)
  
}
