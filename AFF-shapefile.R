#' This script shows how to read in a custom shape file downloaded from the
#' Census Bureau's American Fact Finder.
#' 
#' You will need the .dsn and .shp files associated with your shape file.

library(sf)
library(ggplot2)

# setwd() # to data directory

# ---- Canadian Boarder ----

border.states = read_sf(dsn = ".", layer = "04000") 

ggplot(border.states) +
	geom_sf(colour = "black", size = 0.05) +
	theme_bw()


# ---- Alamo Square ----

alamo <- read_sf(dsn = ".", layer = "10000") %>%
         st_transform(crs = 3857)

ggplot(alamo) +
	geom_sf(colour = "black", size = 0.05) +
	theme_bw()
