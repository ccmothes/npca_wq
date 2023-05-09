pseudoATTAINScatchmentize <- function(layer, path = 'data/') {
  
  # noncontus <- attains_catchments %>%
  #   dplyr::filter(state %in% c("AK", "GU", "HI", "VI", "PR", "AS")) %>%
  #   sf::st_intersection(., parks) %>%
  #   dplyr::select(nhdplusid, UNIT_CODE) %>%
  #   as_tibble(.) %>%
  #   dplyr::left_join(., layer, by = c("nhdplusid", "UNIT_CODE"), multiple = "all") %>%
  #   dplyr::mutate(extent = "Non-CONTUS") %>%
  #   st_as_sf(.)
  
  # next, we subset the holistic "attains_catchments" metadata that represents the `nps_all_attains` data:
  # i.e., select only assessment unit/catchment combos that actually exist within the park
  nps_attains_to_catchment <- attains_catchments %>%
    dplyr::filter(nhdplusid %in% layer$nhdplusid) %>%
    sf::st_intersection(., dplyr::select(parks, UNIT_CODE), left = FALSE) %>%
    dplyr::select(nhdplusid, UNIT_CODE) %>%
    as_tibble(.) %>%
    dplyr::right_join(., layer,
                     by = c("nhdplusid", "UNIT_CODE"), multiple = "all") %>%
    #mutate(extent = "CONTUS") %>%
    #bind_rows(noncontus) %>%
    dplyr::mutate(attains = ifelse(is.na(assessmentunitidentifier), "NO ATTAINS DATA", "ATTAINS DATA")) %>%
    sf::st_as_sf(.)
  
  saveRDS(nps_attains_to_catchment, paste0(path, '/00_nps_pseudo_attains_to_catchment.RDS'))
  try(sf::st_write(select(nps_attains_to_catchment, UNIT_CODE, assessmentunitidentifier, nhdplusid), paste0(path, '/00_nps_pseudo_attains_to_catchment.shp'), append = FALSE))
  
  return(nps_attains_to_catchment)
  
}
