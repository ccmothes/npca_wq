getNPCA <- function(projection = 3857){

  npca_regions <- tibble::tibble(state = c("AK",
                                           "DE", "DC", "MD", "PA", "VA", "WV",
                                           "IL", "IN", "IA", "KS", "NE", "OH", "MI", "MN", "MO", "SD", "WI",
                                           "CT", "ME", "NH", "NJ", "NY", "MA", "RI", "VT",
                                           "ID", "MT", "ND", "WY",
                                           "OR", "WA",
                                           "AS", "CA", "HI", "NV", "GU",
                                           "AL", "AR", "GA", "KY", "NC", "MS", "SC", "TN",
                                           "AZ", "CO", "NM", "UT",
                                           "FL", "LA", "PR", "VI",
                                           "TX", "OK"),
                                 office = c("Alaska",
                                            "Mid-Atlantic", "Mid-Atlantic", "Mid-Atlantic", "Mid-Atlantic", "Mid-Atlantic", "Mid-Atlantic",
                                            "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest",
                                            "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast",
                                            "Northern Rockies", "Northern Rockies", "Northern Rockies", "Northern Rockies",
                                            "Northwest", "Northwest",
                                            "Pacific", "Pacific", "Pacific", "Pacific", "Pacific",
                                            "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast",
                                            "Southwest", "Southwest", "Southwest", "Southwest",
                                            "Suncoast", "Suncoast", "Suncoast", "Suncoast",
                                            "Texas", "Texas"))
  
  npca_regions <- tigris::states() %>%
    sf::st_transform(projection) %>%
    sp::merge(., npca_regions, by.x = "STUSPS", by.y = "state") %>%
    dplyr::group_by(office) %>%
    dplyr::summarize() %>%
    sf::st_transform(projection)
  
  return(npca_regions)
  
}