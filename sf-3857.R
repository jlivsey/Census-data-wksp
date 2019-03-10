#' Quick snip-it of code to show conversion from Census NAD=1983 
#' (EPSG: 4269) to EPSG: 3857

library(tigris)
library(sf)
library(dbplyr)
library(ggplot2)

c.4269 = counties("Texas", year=2016) 
  
  
c.3857 = c.4269 %>%
         st_as_sf() %>%
         st_transform(crs = 3857)
  
  
