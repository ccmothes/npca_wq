getATTAINS <- function(path = "data/ATTAINS"){
  
  call <- "https://dmap-data-commons-ow.s3.amazonaws.com/data/ATTAINS_Assessment_20230416_gpkg.zip"
  path <- path
  
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  unzip(temp1, exdir = path)
  
  print(paste0('ATTAINS data succesfully downloaded to ', path))
  
}