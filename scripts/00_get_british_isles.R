##### Get outlines for the British Isles


## Load necessary libraries and define helper variables/functions

library(dplyr) # for data manipulation
library(sf) # for spatial data formats and operations
library(tmap) # for mapping and visualizing spatial data

# Set proj4string for WGS84 Lat Long datum
wgs84 <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# Download the components of the British Isles as 'sf' R objects
# Source is https://gadm.org/data.html

# UK is a domain, so level 1 will give us individual countries
download.file('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_GBR_1_sf.rds','./carto_data/gadm36_GBR_1_sf.rds')

# Download Republic of Ireland as 'sf'  RT object.
# Since Ireland is a single country, we want level 0
download.file('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IRL_0_sf.rds','./carto_data/gadm36_IRL_0_sf.rds')

# And the Isle of Man, which is a crown territory but not in ther UK...geez!
download.file('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IMN_0_sf.rds', './carto_data/gadm36_IMN_0_sf.rds') 

# Read in the downloaded files
uk_sf <- readRDS("./carto_data/gadm36_GBR_1_sf.rds") %>%
dplyr::select(NAME_1) %>% rename(country = NAME_1)

ir_sf <- readRDS("./carto_data/gadm36_IRL_0_sf.rds") %>%
dplyr::select(NAME_0) %>% rename(country = NAME_0)
                  
im_sf <- readRDS("./carto_data/gadm36_IMN_0_sf.rds") %>%
dplyr::select(NAME_0) %>% rename(country = NAME_0)
 
# Join the two countries
bis_sf <- rbind(uk_sf,ir_sf,im_sf)
 
# Save it for future use as a single RDS file
saveRDS(bis_sf,'./carto_data/british_isles.rds')


# View results
tm_shape(bis_sf) + tm_polygons(bis_sf)
```