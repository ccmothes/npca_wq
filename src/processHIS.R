processHIS <- function(path = "data/HIS_raw/"){
  
  library(tidyverse)
  library(rvest)
  library(jsonlite)
  library(sf)
  
  list.files(path = path, pattern = "*.zip") %>%
    map(~ unzip(zipfile = paste0(path, .), exdir = path, overwrite = TRUE)) 
  
  list <- list.files(path = path, pattern = "*.mdb", full.names = TRUE)
  small_list <- list.files(path = path, pattern = "*.mdb", full.names = FALSE)
  
  converter <- function(list, small_list){
    
    db <- list
    con2 <- RODBC::odbcConnectAccess2007(db)
    RODBC::sqlTables(con2, tableType = "TABLE")$TABLE_NAME
    
    try(tblORW <- RODBC::sqlFetch(con2, "tblORW") %>%
          write_csv(paste0("data/HIS_ORW/", str_sub(small_list, 1, 4), '_tblORW.csv')), silent = TRUE)
    
    ET_Park <- RODBC::sqlFetch(con2, "ET_Park") 
    
    try(ET_Park <- ET_Park %>% left_join(tblORW, by = "ORW_ID"), silent = TRUE)
    
    ET_Park <- ET_Park %>% 
      mutate(UNIT_CODE = str_sub(small_list, 1, 4)) %>%
      write_csv(paste0("data/HIS_ORW/", str_sub(small_list, 1, 4), '_ET_Park_plus.csv'))
    
    print(paste0(str_sub(small_list, 1, 4), " HIS data converted to .csv"))
  }
  
  map2(list, small_list, possibly(converter, otherwise = print(" got no HIS data!")))
  
perm_id <- function(x){
  #x=list.files(path = "data/HIS_ORW", pattern = "*ET_Park_plus.csv", full.names = TRUE)[2]
    x <- read.csv(x) %>%
      select(5) %>%
      rename(permanent_identifier = 1) %>%
      mutate(permanent_identifier = as.character(permanent_identifier))
    
    return(x)
  
    }
  
perm_id <- list.files(path = "data/HIS_ORW", pattern = "*ET_Park_plus.csv", full.names = TRUE) %>%
  map(~perm_id(.)) %>% bind_rows()



cleaner <- function(x){
  
  x <- read.csv(x)
  
  names(x) <- tolower(names(x))
  
  x <- x %>%
    dplyr::select(any_of(c("unit_code", "gnis_id", "entity_id", "gnis_name",
                           "orw_id", "state", "reachcode",
                           "designation_level", "designation_name", 
                           "entire_extent_of_orw")))
  
  return(x)
  
}


orw_only <- list.files(path = "data/HIS_ORW", pattern = "*ET_Park_plus.csv", full.names = TRUE) %>%
  map(~cleaner(.)) %>%
  bind_rows() %>%
  cbind(perm_id) %>%
  filter(!is.na(orw_id)) %>%
  mutate(Tier = ifelse(grepl("Outstanding National Resource Water|ONRW|Tier 3|3", designation_name, ignore.case = TRUE), "Tier 3", "Tier 2.5"))
saveRDS(orw_only, 'data/parks_with_orw.RDS')
write_csv(orw_only, 'data/toNPCA/parks_with_orw.csv')

return(orw_only)

}