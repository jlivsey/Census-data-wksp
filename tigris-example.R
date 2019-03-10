library(tigris)

# ---- States ----

s = states()
plot(s)

# ---- Counties ----
ny_counties = counties("New York", year = 2016) 
ny_counties_simple = counties("New York", year = 2016, cb=TRUE, resolution = "20m")
plot(ny_counties)
plot(ny_counties_simple, border='red', add=TRUE)


# ---- Tracts ----
ca_tracts = tracts("California", year = 2016) 
ca_tracts_simple = tracts("California", year = 2016, cb = TRUE)

plot(ca_tracts)
plot(ca_tracts_simple, border='red', add=TRUE)


# ---- Block Groups ----
ny_blockgroup = block_groups("New York", 029, year = 2016) 
plot(ny_blockgroup)


# ---- Blocks, Erie County, NY ----
ny_blocks = blocks("New York", 029, year = 2016) 
plot(ny_blocks)


# ---- Zip Code Tabulation Area ----
dc_zip = zctas(cb='TRUE', starts_with = "200")
plot(dc_zip)


# ---- And Many others! ----