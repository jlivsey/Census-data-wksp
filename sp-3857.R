#' Quick snip-it of code to show conversion from Census NAD=1983 
#' (EPSG: 4269) to EPSG: 3857

library(tigris)
library(sp)
library(blscrapeR)


c.4269 = counties("Texas", year=2016) 
  
  
c.3857 = c.4269 %>%
         st_as_sf() %>%
         st_transform(crs = 3857)
  
  
g <- ggplot(acs5.2017) +
  geom_sf(colour = "black", size = 0.05, aes(fill = DirectEst)) +
  ggtitle("Median Household Income for Boone County") + 
  geom_sf(data = c.3857, border = "red") + # NOT Sure if border="red" is valid!! 
  theme_bw()