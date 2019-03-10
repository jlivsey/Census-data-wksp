library(jsonlite)
library(dplyr)

#'
#' ACS data via API call
#' 

# ---- Maryland (State=24) Median Household Income (DP03_0062E) ----
data_url2 <- "https://api.census.gov/data/2017/acs/acs5/profile?get=DP03_0062E,NAME&for=county:*&in=state:24&key=25488af6841e29e4b76b5e8fb877ea0d4ae2a017"
json_data <- fromJSON(data_url2)

acs_dat <- data.frame(json_data[-1,])
colnames(acs_dat) <- json_data[1,]

head(acs_dat)

dat <- acs_dat %>%
  tidyr::unite(GEOID, state,county, sep="", remove=TRUE) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(DP03_0062E = as.numeric(as.character(DP03_0062E))) %>%
  mutate(NAME = as.character(NAME)) %>%
  mutate(NAME = substring(NAME, 1, nchar(NAME)-10)) %>%
  mutate(NAME = gsub(NAME, pattern = " County", replacement = ""))
head(dat)

# read in Maryland counties shape file
md = counties("Maryland", year = 2016) %>%
  st_as_sf() %>%
  st_transform(crs = 3857) 

#' # Join data and Shape data
md_dat <- dat %>%
  inner_join(md, by = c('GEOID' = 'GEOID'))

#' Quick plots of the merged data.
g1 <- ggplot(md_dat) +
  geom_sf(colour = "black", size = 0.05, aes(fill = DP03_0062E)) +
  scale_fill_distiller("HHI", palette = "RdYlBu") +
  ggtitle("Median Household Income", subtitle = "2016 Maryland Counties") +
  theme_bw()
print(g1)

#' add labels
g2 <- g1 + geom_sf_text(aes(label = NAME.y), colour = "black")
print(g2)



# ---- South Carolina (State 45) Internet --------------------------
data_url2 <- "https://api.census.gov/data/2017/acs/acs5/profile?get=DP02_0152PE,NAME&for=county:*&in=state:45&key=25488af6841e29e4b76b5e8fb877ea0d4ae2a017"
json_data <- fromJSON(data_url2)

acs_dat <- data.frame(json_data[-1,])
colnames(acs_dat) <- json_data[1,]

dat <- acs_dat %>%
  tidyr::unite(GEOID, state,county, sep="", remove=TRUE) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(DP02_0152PE = as.numeric(as.character(DP02_0152PE))) %>%
  mutate(NAME = as.character(NAME)) %>%
  mutate(NAME = substring(NAME, 1, nchar(NAME)-16)) %>%
  mutate(NAME = gsub(NAME, pattern = " County", replacement = ""))
head(dat)

# read in South Carolina counties shape file
sc = counties("South Carolina", year = 2016) %>%
  st_as_sf() %>%
  st_transform(crs = 3857) 

#' # Join data and Shape data
sc_dat <- dat %>%
  inner_join(sc, by = c('GEOID' = 'GEOID'))

#' Quick plots of the merged data.
g1 <- ggplot(sc_dat) +
  geom_sf(colour = "black", size = 0.05, aes(fill = DP02_0152PE)) +
  scale_fill_distiller("Percent", palette = "RdYlBu") +
  ggtitle("Percent HH with Broadband Internet", subtitle = "2016 Maryland Counties") +
  theme_bw()
print(g1)

#' Plot with labels
g2 <- g1 + geom_sf_text(aes(label = NAME.y), colour = "black")
print(g2)


# ---- California Internet (State 06) ------------------------------------------
data_url2 <- "https://api.census.gov/data/2017/acs/acs5/profile?get=DP02_0152PE,NAME&for=county:*&in=state:06&key=25488af6841e29e4b76b5e8fb877ea0d4ae2a017"
json_data <- fromJSON(data_url2)

acs_dat <- data.frame(json_data[-1,])
colnames(acs_dat) <- json_data[1,]

dat <- acs_dat %>%
  tidyr::unite(GEOID, state,county, sep="", remove=TRUE) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(DP02_0152PE = as.numeric(as.character(DP02_0152PE))) %>%
  mutate(NAME = as.character(NAME)) %>%
  # mutate(NAME = substring(NAME, 1, nchar(NAME)-16)) %>%
  mutate(NAME = gsub(",.*","",NAME)) %>%
  mutate(NAME = gsub(NAME, pattern = " County", replacement = ""))
head(dat)

# read in California counties shape file
ca = counties("California", year = 2016) %>%
  st_as_sf() %>%
  st_transform(crs = 3857) 

#' # Join data and Shape data
ca_dat <- dat %>%
  inner_join(ca, by = c('GEOID' = 'GEOID'))

#' Quick plots of the merged data.
g1 <- ggplot(ca_dat) +
  geom_sf(colour = "black", size = 0.05, aes(fill = DP02_0152PE)) +
  scale_fill_distiller("Percent", palette = "RdYlBu") +
  ggtitle("Percent HH with Broadband Internet", subtitle = "2016 California    Counties") +
  theme_bw()
print(g1)

#' Plot with labels
g2 <- g1 + geom_sf_text(aes(label = NAME.y), colour = "black")
print(g2)




# ---- New York (State 36) Workforce ------------------------------------------
data_url2 <- "https://api.census.gov/data/2017/acs/acs5/profile?get=DP03_0003PE,NAME&for=county:*&in=state:36&key=25488af6841e29e4b76b5e8fb877ea0d4ae2a017"
json_data <- fromJSON(data_url2)

acs_dat <- data.frame(json_data[-1,])
colnames(acs_dat) <- json_data[1,]

dat <- acs_dat %>%
  tidyr::unite(GEOID, state,county, sep="", remove=TRUE) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(DP03_0003PE = as.numeric(as.character(DP03_0003PE))) %>%
  mutate(NAME = as.character(NAME)) %>%
  mutate(NAME = gsub(",.*","",NAME)) %>%
  mutate(NAME = gsub(NAME, pattern = " County", replacement = ""))
head(dat)

# read in California counties shape file
ny = counties("New York", year = 2016) %>%
  st_as_sf() %>%
  st_transform(crs = 3857) 

#' # Join data and Shape data
ny_dat <- dat %>%
  inner_join(ny, by = c('GEOID' = 'GEOID'))

#' Quick plots of the merged data.
g1 <- ggplot(ny_dat) +
  geom_sf(colour = "black", size = 0.05, aes(fill = DP03_0003PE)) +
  scale_fill_distiller("Percent", palette = "RdYlBu") +
  ggtitle("Percent 16+ in workforce", subtitle = "2016 New York Counties") +
  theme_bw()
print(g1)

#' Plot with labels
g2 <- g1 + geom_sf_text(aes(label = NAME.y), colour = "black")
print(g2)

