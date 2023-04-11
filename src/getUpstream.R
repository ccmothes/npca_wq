# get watesheds for all park waters that intersect the park boundaries. currently only goes 30 km upstream.


getUpstream <- function(comids=boundary_flowlines){
  
  # Read in the NHD. This is a table representing all flow direction data across CONUS.
  nhd <- read_csv('data/nhd_flow_network.csv')
  
  subset_sites <- comids %>% #remove comid duplicates (i.e., samples located in the same catchment)
    distinct(comid,.keep_all=T)
  
  watersheds <- function(spid_union){
    
    tracer <- function(samples){
      
      small_db <- as_tibble(subset_sites)
      
      outlet <- small_db %>%
        dplyr::filter(comid == samples)
      
      upstream_nhd <- get_UT(nhd, outlet$comid,  distance = 30) %>% #upstream trace function in nhdplusTools
        as_tibble() %>%
        rename(comid_list = value) 
      write_csv(upstream_nhd, paste0('data/ws_list/', outlet$comid, '.csv'))
      upstream_nhd <- upstream_nhd %>%
        mutate(origin = outlet$comid)
      
    }
    
    ws <- #possibly(
      map_dfr(spid_union, tracer)
      #, otherwise = 1+1, quiet = TRUE)
  }
  
  upstream_list <- subset_sites %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(comid_list = map(comid, watersheds)) %>%
    tidyr::unnest(., cols = comid_list) %>%
    select(origin,
           comid = comid_list) %>%
    distinct(.keep_all = TRUE)
  
  # If you are running this code across MANY watersheds or watersheds are LARGE (think Mississippi River),
  # you can make the code faster by using the stored CONUS catchment polygons instead of the code below. 
  # Trade-off is polygons are not the most up-to-date since it uses a static, downloaded version.
  catchments <- readRDS('data/nhdplusv2_catchments.RDS') %>%
    rename(comid = FEATUREID) %>%
    filter(comid %in% upstream_list$comid)
  
  site_watersheds <- merge(catchments, upstream_list, by = 'comid', all.x = FALSE) %>%
    group_by(origin) %>%
    summarize() %>%
    rename(comid = origin) #%>%
  #mutate(comid = as.character(comid))
  saveRDS(site_watersheds, 'data/watersheds_30km.RDS')
  sf::st_read(site_watersheds, 'data/watersheds_30km.shp')
  # Here, an option to remove odd holes that form within the watershed that are due to 
  # the catchment boundaries not lining up perfectly. However, this MAY introduce wrong watershed
  # boundaries for watersheds with "closed" systems within them.
  site_watersheds <- site_watersheds %>%
    nngeo::st_remove_holes()
  
  return(site_watersheds)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}