aggregateATTAINS <- function(path = 'data/') {

  nps_attains_points <- readRDS(paste0(path,'/00_nps_attains_point.RDS')) %>%
    sf::st_join(., npca_regions, left = TRUE) %>%
    dplyr::as_tibble(.) %>%
    mutate(assessment_type = "POINT")
  nps_attains_lines <- readRDS(paste0(path,'/00_nps_attains_line.RDS')) %>% 
    sf::st_join(., npca_regions, left = TRUE) %>%
    dplyr::as_tibble(.) %>%
    mutate(assessment_type = "LINE")
  nps_attains_areas <- readRDS(paste0(path,'/00_nps_attains_area.RDS')) %>% 
    sf::st_join(., npca_regions, left = TRUE) %>%
    dplyr::as_tibble(.) %>%
    mutate(assessment_type = "AREA")
  
  # nps_noncontus_attains_points <- readRDS(paste0(path,'/00_nps_noncontus_attains_point.RDS')) %>% 
  #   sf::st_join(., npca_regions, left = TRUE) %>%
  #   dplyr::as_tibble(.) %>%
  #   mutate(assessment_type = "POINT")
  # nps_noncontus_attains_lines <- readRDS(paste0(path,'/00_nps_noncontus_attains_line.RDS')) %>%
  #   sf::st_join(., npca_regions, left = TRUE) %>%
  #   dplyr::as_tibble(.) %>%
  #   mutate(assessment_type = "LINE")
  # nps_noncontus_attains_areas <- readRDS(paste0(path,'/00_nps_noncontus_attains_area.RDS')) %>%
  #   sf::st_join(., npca_regions, left = TRUE) %>%
  #   dplyr::as_tibble(.) %>%
  #   mutate(assessment_type = "AREA")
 
  # this represents all assessment units that are physically within the park boundaries
  nps_all_attains <- bind_rows(nps_attains_areas, nps_attains_lines, nps_attains_points) %>%
    mutate(office = ifelse(is.na(office) & state =="TX", "Texas",
                           ifelse(is.na(office) & state =="MN", "Midwest", office)))
  
  return(nps_all_attains)
  
} 