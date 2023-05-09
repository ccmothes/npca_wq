mapHIS<- function(path='data/parks_with_orw.RDS'){
  
  sf::sf_use_s2(FALSE)
  table <- readRDS(path) 
  nhd <- sf::st_read(dsn = "data/NHDPlus_H_National_Release_1_GDB/NHDPlus_H_National_Release_1_GDB.gdb",
                     layer = "NetworkNHDFlowline")
  
  nhd <- nhd %>% sp::merge(., table, by.x = "permanent_identifier", by.y = "permanent", all.x = FALSE)
  
  mapview::mapview(nhd)
  }

