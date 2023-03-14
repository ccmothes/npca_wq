#' Pull metadata from NPS Data Store
#' 
#' This function uses the Data Store REST Api to pull metadata for Nation Parks,
#' specifically  water rights dockets and geospatial datasets.
#' 
#' @param park The park unit(s) 4 digit code you want to pull metadata for
#' @param max The maximum number of entries to return (use a large number to return all entries, default is 1000)
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save = TRUE`, the file path to save to
#' 
#' @return A single dataframe of metadata for each park
inventoryDataStore <- function(park, max = 1000, path = "data/HIS/"){
  
  #set base URL
  call <- "https://irmaservices.nps.gov/datastore/v4/rest"
  
  #Search all park info
  dat <- httr::GET(paste0(call, "/QuickSearch?q=", "Hydrographic", "&top=", max)) 
  
  #convert content to text
  dat_text <- httr::content(dat, "text", encoding = "UTF-8")
  
  #parse data in JSON
  dat_json <- jsonlite::fromJSON(dat_text, flatten = TRUE)
  
  #convert items to data.frame
  dat_df <- dplyr::as_tibble(dat_json$items)
  
  #filter out dockets and geospatial datasets
  dat_df_clean <- dat_df %>% 
    dplyr::filter(referenceType %in% c("Geospatial Dataset"),
                  grepl("Hydrographic and Impairment Statistic", title, ignore.case = TRUE)) %>% 
    dplyr::select(-newestVersion)
  
  #create empty vector to fill in resourceID
  resID <- vector("character", length = nrow(dat_df_clean))
  refName <- vector("character", length = nrow(dat_df_clean))
  
  #now get resourceID (dataset download ID) for each item
  for (j in 1:nrow(dat_df_clean)){
    
    refID <- as.character(dat_df_clean[j, "referenceId"])
    
    res <- httr::GET(paste0(call, "/Reference/", refID, "/DigitalFiles"))
    
    #extract resourceID
    resContent1 <- httr::content(res)[[1]]
    try(resContent2 <- httr::content(res)[[2]])
    
    if(grepl("xml", resContent1$fileName)==TRUE) {
      
      resContent <- resContent2
      
    } else {
      
      
      resContent <- resContent1
      
    }
    
    # if no file, no resourceID so assign NA
    if(length(resContent) == 0){
      
      resID[j] <- NA
      
    } else {
      
      resID[j] <- resContent$downloadLink
      refName[j] <- resContent$fileName
      
    }
  }
  
  final_df <- dat_df_clean %>% 
    dplyr::mutate(resourceId = resID,
                  referenceName = refName,
                  UNIT_CODE = str_sub(title,-4,-1)) %>%
    filter(!is.na(referenceName)) #%>%
    .[40:300,]
  
  for(i in 1:nrow(final_df)){
    
    df <- final_df[i,]
    #temp1 <- tempfile()
    download.file(df$resourceId, destfile = paste0("data/HIS/", df$referenceName), method = 'curl')#paste0(path, df$UNIT_CODE, "_HIS.mdb"), method = "curl")
    # try(unzip(temp1, exdir = 'data/HIS/'))
    # try(download.file(df$resourceId, destfile = paste0("data/HIS/", df$UNIT_CODE,"_HIS.mdb")))
    # unlink(temp1)
    print(paste0(df[,11], " finished!"))
    
  }  
  
  
}