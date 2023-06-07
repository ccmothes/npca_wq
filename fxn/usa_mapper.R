# adapted from the tigris package's PR/HI/AK visualizing code

usa_mapper<-  function (input_sf){
  if (!any(grepl("sf", class(input_sf)))) {
    stop("The input dataset must be an sf object.", call = FALSE)
  }
  
  minimal_states <- tigris::states(resolution = "500k", 
                                   progress_bar = FALSE, 
                                   year = 2021) %>% 
    sf::st_transform("ESRI:102003")
  
  ak_bbox <- minimal_states %>% dplyr::filter(GEOID == "02") %>% 
    sf::st_bbox() %>% sf::st_as_sfc()
  hi_bbox <- minimal_states %>% dplyr::filter(GEOID == "15") %>% 
    sf::st_bbox() %>% sf::st_as_sfc()
  pr_bbox <- minimal_states %>% dplyr::filter(GEOID == "72") %>% 
    sf::st_bbox() %>% sf::st_as_sfc()
  vi_bbox <- minimal_states %>% dplyr::filter(GEOID == "78") %>% 
    sf::st_bbox() %>% sf::st_as_sfc()
  gu_bbox <- minimal_states %>% dplyr::filter(GEOID == "66") %>% 
    sf::st_bbox() %>% sf::st_as_sfc()
  as_bbox <- minimal_states %>% dplyr::filter(GEOID == "60") %>% 
    sf::st_bbox() %>% sf::st_as_sfc()
  ma_bbox <- minimal_states %>% dplyr::filter(GEOID == "69") %>% 
    sf::st_bbox() %>% sf::st_as_sfc()
  
  input_sf <- sf::st_transform(input_sf, sf::st_crs(minimal_states))
  
  ak_check <- suppressMessages(sf::st_intersects(input_sf, 
                                                 ak_bbox, sparse = FALSE)[, 1])
  hi_check <- suppressMessages(sf::st_intersects(input_sf, 
                                                 hi_bbox, sparse = FALSE)[, 1])
  pr_check <- suppressMessages(sf::st_intersects(input_sf, 
                                                 pr_bbox, sparse = FALSE)[, 1])
  vi_check <- suppressMessages(sf::st_intersects(input_sf, 
                                                 vi_bbox, sparse = FALSE)[, 1])
  gu_check <- suppressMessages(sf::st_intersects(input_sf, 
                                                 gu_bbox, sparse = FALSE)[, 1])
  as_check <- suppressMessages(sf::st_intersects(input_sf, 
                                                 as_bbox, sparse = FALSE)[, 1])
  ma_check <- suppressMessages(sf::st_intersects(input_sf, 
                                                 as_bbox, sparse = FALSE)[, 1])
  
  if (!any(ak_check) && !any(hi_check) && !any(pr_check) && !any(vi_check) && !any(gu_check)) {
    warning("None of your features are outisde CONTUS so no geometries will be shifted.\nTransforming your object's CRS to 'ESRI:102003'", 
            call. = FALSE)
    transformed_output <- sf::st_transform(input_sf, "ESRI:102003")
    return(transformed_output)
  }
  
  input_sf <- input_sf %>% sf::st_transform(sf::st_crs(minimal_states)) %>% 
    dplyr::mutate(state_fips = dplyr::case_when(suppressMessages(sf::st_intersects(input_sf, ak_bbox, sparse = FALSE)[, 1]) ~ "02", 
                                                suppressMessages(sf::st_intersects(input_sf, hi_bbox, sparse = FALSE)[, 1]) ~ "15", 
                                                suppressMessages(sf::st_intersects(input_sf, pr_bbox, sparse = FALSE)[, 1]) ~ "72", 
                                                suppressMessages(sf::st_intersects(input_sf, vi_bbox, sparse = FALSE)[, 1]) ~ "78",
                                                suppressMessages(sf::st_intersects(input_sf, gu_bbox, sparse = FALSE)[, 1]) ~ "66",
                                                suppressMessages(sf::st_intersects(input_sf, as_bbox, sparse = FALSE)[, 1]) ~ "60",
                                                suppressMessages(sf::st_intersects(input_sf, ma_bbox, sparse = FALSE)[, 1]) ~ "69",
                                                TRUE ~ "00"))
  
  ak_crs <- 3338
  hi_crs <- "ESRI:102007"
  pr_crs <- 3857
  vi_crs <- 3857
  gu_crs <- 3857
  as_crs <- 3857
  ma_crs <- 3857
  
  ak_centroid <- minimal_states %>% dplyr::filter(GEOID == "02") %>% sf::st_transform(ak_crs) %>% sf::st_geometry() %>% 
    sf::st_centroid()
  hi_centroid <- minimal_states %>% dplyr::filter(GEOID == "15") %>% sf::st_transform(hi_crs) %>% sf::st_geometry() %>% 
    sf::st_centroid()
  pr_centroid <- minimal_states %>% dplyr::filter(GEOID == "72") %>% sf::st_transform(pr_crs) %>% sf::st_geometry() %>% 
    sf::st_centroid()
  vi_centroid <- minimal_states %>% dplyr::filter(GEOID == "78") %>% sf::st_transform(vi_crs) %>% sf::st_geometry() %>% 
    sf::st_centroid()
  gu_centroid <- minimal_states %>% dplyr::filter(GEOID == "66") %>% sf::st_transform(gu_crs) %>% sf::st_geometry() %>% 
    sf::st_centroid()
  as_centroid <- minimal_states %>% dplyr::filter(GEOID == "60") %>% sf::st_transform(as_crs) %>% sf::st_geometry() %>% 
    sf::st_centroid()
  ma_centroid <- minimal_states %>% dplyr::filter(GEOID == "69") %>% sf::st_transform(ma_crs) %>% sf::st_geometry() %>% 
    sf::st_centroid()
  
  place_geometry_wilke <- function(geometry, position, scale = 1, centroid = sf::st_centroid(geometry)) {
    (geometry - centroid) * scale + sf::st_sfc(st_point(position)) 
  }
  
  cont_us <- dplyr::filter(minimal_states, !GEOID %in% c("02", "15", "72", "78", "66", "60", "69")) %>% sf::st_transform("ESRI:102003")
  us_lower48 <- dplyr::filter(input_sf, !state_fips %in% c("02", "15", "72", "78", "66", "60", "69")) %>% sf::st_transform("ESRI:102003")
  bb <- sf::st_bbox(cont_us)
  us_alaska <- dplyr::filter(input_sf, state_fips == "02")
  us_hawaii <- dplyr::filter(input_sf, state_fips == "15")
  us_puerto_rico <- dplyr::filter(input_sf, state_fips ==  "72")
  us_vi <- dplyr::filter(input_sf, state_fips ==  "78")
  us_gu <- dplyr::filter(input_sf, state_fips ==  "66")
  us_as <- dplyr::filter(input_sf, state_fips ==  "60")
  us_ma <- dplyr::filter(input_sf, state_fips ==  "69")
  shapes_list <- list(us_lower48)
  
  # Alaska
  if (any(ak_check)) {
    ak_rescaled <- sf::st_transform(us_alaska, ak_crs)
    st_geometry(ak_rescaled) <- place_geometry_wilke(sf::st_geometry(ak_rescaled), 
                                                     c(bb$xmin - 0.08 * (bb$xmax - bb$xmin), bb$ymin + 
                                                         1.2 * (bb$ymax - bb$ymin)), scale = 0.75, 
                                                     centroid = ak_centroid)
    sf::st_crs(ak_rescaled) <- "ESRI:102003"
    shapes_list <- c(shapes_list, list(ak_rescaled))
  }
  
  
  # Puerto Rico
  if (any(pr_check)) {
    pr_rescaled <- sf::st_transform(us_puerto_rico, 
                                    pr_crs)
    sf::st_geometry(pr_rescaled) <- place_geometry_wilke(sf::st_geometry(pr_rescaled), 
                                                         c(bb$xmin + 0.96 * (bb$xmax - bb$xmin), bb$ymin - 
                                                             0.00 * (bb$ymax - bb$ymin)), scale = 2, 
                                                         centroid = pr_centroid)
    st_crs(pr_rescaled) <- "ESRI:102003"
    shapes_list <- c(shapes_list, list(pr_rescaled))
  }
  
  # Virgin Islands
  if (any(vi_check)) {
    vi_rescaled <- sf::st_transform(us_vi, 
                                    vi_crs)
    
    sf::st_geometry(vi_rescaled) <- place_geometry_wilke(
      sf::st_geometry(vi_rescaled), 
      c(bb$xmin + 1.06 * (bb$xmax - bb$xmin), bb$ymin - 0.00 * (bb$ymax - bb$ymin)), 
      scale = 2, 
      centroid = vi_centroid)
    
    st_crs(vi_rescaled) <- "ESRI:102003"
    shapes_list <- c(shapes_list, list(vi_rescaled))
  }
  
  # Hawaii
  if (any(hi_check)) {
    hi_rescaled <- suppressWarnings(us_hawaii %>% sf::st_intersection(hi_bbox) %>% 
                                      sf::st_transform(hi_crs))
    sf::st_geometry(hi_rescaled) <- place_geometry_wilke(sf::st_geometry(hi_rescaled), 
                                                         c(bb$xmin - 0.01 * (bb$xmax - bb$xmin), bb$ymin + 
                                                             0.21 * (bb$ymax - bb$ymin)), scale = 2, 
                                                         centroid = hi_centroid)
    st_crs(hi_rescaled) <- "ESRI:102003"
    shapes_list <- c(shapes_list, list(hi_rescaled))
  }
  
  # American Samoa
  if (any(as_check)) {
    as_rescaled <- suppressWarnings(us_as %>% sf::st_intersection(as_bbox) %>% 
                                      sf::st_transform(as_crs))
    
    sf::st_geometry(as_rescaled) <- place_geometry_wilke(sf::st_geometry(as_rescaled), 
                                                         c(bb$xmin - 0.18 * (bb$xmax - bb$xmin), bb$ymin + 
                                                             0.0 * (bb$ymax - bb$ymin)), scale = 5, 
                                                         centroid = as_centroid)
    st_crs(as_rescaled) <- "ESRI:102003"
    shapes_list <- c(shapes_list, list(as_rescaled))
  }
  
  # Guam
  if (any(gu_check)) {
    gu_rescaled <- suppressWarnings(us_gu %>% sf::st_intersection(gu_bbox) %>% 
                                      sf::st_transform(gu_crs))
    sf::st_geometry(gu_rescaled) <- place_geometry_wilke(sf::st_geometry(gu_rescaled), 
                                                         c(bb$xmin - 0.35 * (bb$xmax - bb$xmin), bb$ymin + 
                                                             0.14 * (bb$ymax - bb$ymin)), scale = 5, 
                                                         centroid = gu_centroid)
    st_crs(gu_rescaled) <- "ESRI:102003"
    shapes_list <- c(shapes_list, list(gu_rescaled))
  }
  
  # Mariana Islands
  if (any(ma_check)) {
    ma_rescaled <- suppressWarnings(us_ma %>% sf::st_intersection(ma_bbox) %>% 
                                      sf::st_transform(ma_crs))
    sf::st_geometry(ma_rescaled) <- place_geometry_wilke(sf::st_geometry(ma_rescaled), 
                                                         c(bb$xmin - 0.25 * (bb$xmax - bb$xmin), bb$ymin + 
                                                             0.77 * (bb$ymax - bb$ymin)), scale = 5, 
                                                         centroid = ma_centroid)
    st_crs(ma_rescaled) <- "ESRI:102003"
    shapes_list <- c(shapes_list, list(ma_rescaled))
  }
  
  
  
  output_data <- shapes_list %>% dplyr::bind_rows() %>% 
    dplyr::select(-state_fips)
  
  return(output_data)
  
}

