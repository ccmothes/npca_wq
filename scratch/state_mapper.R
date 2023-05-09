library(tidyverse)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(sf)

state_mapper <- function(){

state_map <- tigris::states() %>%
  sf::st_transform("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>%
  as(Class = "Spatial")

state_map@data$id  <- rownames(state_map@data)

non_conus_fips <- c("02", "11", "15", "60", "66", "69", "72", "78")

### Alaska
alaska_state <- state_map[state_map$STATEFP=="02",]
alaska_state <- elide(alaska_state, rotate=-50)
alaska_state <- elide(alaska_state, scale=max(apply(bbox(alaska_state), 1, diff)) / 1.8)
alaska_state <- elide(alaska_state, shift=c(-2400000, -2800000))
proj4string(alaska_state) <- proj4string(state_map)

### Hawaii
hawaii_state <- state_map[state_map$STATEFP=="15",]
hawaii_state <- elide(hawaii_state, rotate=-35)
hawaii_state <- elide(hawaii_state, shift=c(5800000, -1900000))
proj4string(hawaii_state) <- proj4string(state_map)

### Puerto Rico
puertorico_state <- state_map[state_map$STATEFP=="72",]
puertorico_state <- elide(puertorico_state, rotate=+13)
puertorico_state <- elide(puertorico_state, scale=max(apply(bbox(puertorico_state), 1, diff)) / 0.5)
puertorico_state <- elide(puertorico_state, shift=c(+600000, -2600000))
proj4string(puertorico_state) <- proj4string(state_map)

### US Virgin Islands
usvi_state <- state_map[state_map$STATEFP=="78",]
usvi_state <- elide(usvi_state, rotate=+13)
usvi_state <- elide(usvi_state, scale=max(apply(bbox(usvi_state), 1, diff)) / 0.25)
usvi_state <- elide(usvi_state, shift=c(+1500000, -2600000))
proj4string(usvi_state) <- proj4string(state_map)

### Guam
guam_state <- state_map[state_map$STATEFP=="66",]
guam_state <- elide(guam_state, rotate=-65)
guam_state <- elide(guam_state, scale=max(apply(bbox(guam_state), 1, diff)) / 0.15)
guam_state <- elide(guam_state, shift=c(+1200000, -3200000))
proj4string(guam_state) <- proj4string(state_map)

### Northern Mariana Islands
noma_state <- state_map[state_map$STATEFP=="69",]
noma_state <- elide(noma_state, rotate=-55)
noma_state <- elide(noma_state, scale=max(apply(bbox(noma_state), 1, diff)) / 0.85)
noma_state <- elide(noma_state, shift=c(+300000, -3400000))
proj4string(noma_state) <- proj4string(state_map)

### American Samoa
amsam_state <- state_map[state_map$STATEFP=="60",]
amsam_state <- elide(amsam_state, rotate=-55)
amsam_state <- elide(amsam_state, scale=max(apply(bbox(amsam_state), 1, diff)) / 0.25)
amsam_state <- elide(amsam_state, shift=c(-2300000, -3400000))
proj4string(amsam_state) <- proj4string(state_map)

### Add the moved states/territories back to the CONUS map
state_map <- state_map[!state_map$STATEFP %in% non_conus_fips,]
state_map <- rbind(state_map, 
                   alaska_state, 
                   hawaii_state, 
                   puertorico_state,
                   usvi_state, 
                   noma_state, 
                   guam_state, 
                   amsam_state
) %>%
  st_as_sf()

return(state_map)

}