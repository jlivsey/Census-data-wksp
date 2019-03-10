library(dplyr)
library(sf)
library(ggplot2)

# ---- direct download data from AFF (South Carolina Marinas) ----

# read in data from AFF 
csv_dat = read.csv("C:/Users/livse301/Dropbox/jim/USF-wksp/data/southcarolina-marinas/BP_2016_00A1.csv")

dat = csv_dat %>% 
  select(GEO.id2, county = GEO.display.label, ESTAB, EMP, PAYANN) %>%
  mutate(county = as.character(county)) %>%
  mutate(county = gsub(",.*","",county)) %>%
  mutate(county = gsub(county, pattern = " County", replacement = "")) %>%
  mutate(GEO.id2 = as.character(GEO.id2))
head(dat)

# Read in shape file from Census tigris and transform to sf 
sc = counties("South Carolina", year = 2016) %>%
  st_as_sf() %>%
  st_transform(crs = 3857) 


#' # Join AFF data and Shape data
sc_dat <- dat %>%
  inner_join(sc, by = c('GEO.id2' = 'GEOID'))
head(sc_dat)


#' Quick plots of the merged data.
g1 <- ggplot(sc_dat) +
  geom_sf(colour = "black", size = 0.05, aes(fill = ESTAB)) +
  scale_fill_distiller("ESTAB", palette = "RdYlBu") +
  ggtitle("Number of Marinas", subtitle = "2016 South Carolina Counties") +
  theme_bw()
print(g1)


# ---- direct download data from AFF (South Carolina Population) ----

##### 2000 ####

# read in data from AFF 
csv_dat = read.csv("C:/Users/livse301/Dropbox/jim/USF-wksp/data/southcarolina-population/DEC_00_SF1_DP1.csv")

dat = csv_dat %>% 
  select(GEO.id2, GEO.display.label, pop = HC01_VC01) %>% 
  tidyr::separate(GEO.display.label, c("county", "state"), sep=", ") %>%
  tidyr::separate(county, c("county", "extra"), sep=" ") %>%
  select(-extra, -state) %>%
  mutate(county = as.character(county)) %>%
  mutate(GEO.id2 = as.character(GEO.id2))

# Read in shape file from Census tigris and transform to sf 
sc = counties("South Carolina", year = 2016) %>%
  st_as_sf() %>%
  st_transform(crs = 3857)

#' # Join AFF data and Shape data
sc_dat <- dat %>%
  inner_join(sc, by = c('GEO.id2' = 'GEOID'))

#' Quick plots of the merged data.
g2000 <- ggplot(sc_dat) +
  geom_sf(colour = "black", size = 0.05, aes(fill = pop)) +
  scale_fill_distiller("pop", palette = "RdYlBu") +
  ggtitle("Total Population 2000 Census", subtitle = "2016 South Carolina Counties") +
  theme_bw()
# print(g1)


##### 2010 ####


# read in data from AFF 
csv_dat = read.csv("C:/Users/livse301/Dropbox/jim/USF-wksp/data/southcarolina-population/DEC_10_SF1_P1.csv")

dat = csv_dat %>% 
  select(GEO.id2, GEO.display.label, pop = D001) %>% 
  tidyr::separate(GEO.display.label, c("county", "state"), sep=", ") %>%
  tidyr::separate(county, c("county", "extra"), sep=" ") %>%
  select(-extra, -state) %>%
  mutate(county = as.character(county)) %>%
  mutate(GEO.id2 = as.character(GEO.id2))

# Read in shape file from Census tigris and transform to sf 
sc = counties("South Carolina", year = 2016) %>%
  st_as_sf() %>%
  st_transform(crs = 3857)

#' # Join AFF data and Shape data
sc_dat <- dat %>%
  inner_join(sc, by = c('GEO.id2' = 'GEOID'))

#' Quick plots of the merged data.
g2010 <- ggplot(sc_dat) +
  geom_sf(colour = "black", size = 0.05, aes(fill = pop)) +
  scale_fill_distiller("pop", palette = "RdYlBu") +
  ggtitle("Total Population 2010 Census", subtitle = "2016 South Carolina Counties") +
  theme_bw()
# print(g1)

# 2000 and 2010 South Carolina Census Population
k <- gridExtra::grid.arrange(g2000, g2010, nrow = 1, ncol = 2)

#' with labels
g3 <- g2010 + geom_sf_text(aes(label = county), colour = "black")
print(g3)


