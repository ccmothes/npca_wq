#' Import park boundary shapefile
#' 
#' This function uses the Data Store REST Api to download national park boundary 
#' shapefiles.
#' 
#' @param park The 4 digit national park code(s) for parks of interest
#' @param save Whether to save (TRUE) the resulting shapefile or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#' 
#' @return A spatial sf object for each specified park boundary
#' 
#' @seealso [pullDataStore()]
getParkUnits <- function(){
  
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
  
  parks <- sf::st_read(dsn = temp2)
  
  return(parks)
  
}