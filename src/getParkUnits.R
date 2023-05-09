getParkUnits <- function(projection = 3857){
  
  call <- "https://irmaservices.nps.gov/datastore/v4/rest"

  #pull resource ID using reference ID of the park boundaries landing page
  downloadLink <- httr::GET(paste0(call, "/Reference/2296705/DigitalFiles")) %>% 
    httr::content("text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(.,flatten = TRUE) %>% 
    dplyr::as_tibble() %>% 
    filter(str_detect(fileName, "nps_boundary")) %>% 
    pull(downloadLink)
  #download boundary 
  temp1 <- tempfile()
  download.file(downloadLink, destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  sf::sf_use_s2(FALSE)
  
  parks <- sf::st_read(dsn = temp2) %>%
    dplyr::select(UNIT_CODE, UNIT_NAME, PARK_NAME = PARKNAME, UNIT_TYPE) %>%
    sf::st_transform(projection)
  
  return(parks)
  
}