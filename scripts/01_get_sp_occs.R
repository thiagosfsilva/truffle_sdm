#### Download and clean Tuber aestivum occurence data


## Load required packages
library(spocc) # to obtain species occurrence records
library(dplyr) # for data manipulation
library(CoordinateCleaner) # cleaning up species records
library(tmap)
library(sf)
library(sp)


# Define target species
sp_names <- c('Tuber aestivum','Tuber uncinatum')

# Query records from GBIF
gbif_query <- occ(query = sp_names,
                 from =  'gbif',
                 has_coords = TRUE,
                 limit = 1e6)

gbif_occ <- gbif_query %>% occ2df() %>% # get occurrences as a data.frame (table)
             mutate(longitude = as.numeric(longitude), # convert coords to numeric
                    latitude = as.numeric(latitude), # convert coords to numeric
                    year = lubridate::year(date)) %>% # get year of record from date
             dplyr::select(name, longitude, latitude, year) # select only useful columns

all_occ <- gbif_occ %>%
    dplyr::filter(year >= 1970) %>%
    st_as_sf(coords=c('longitude','latitude'), crs=wgs84)
                                                 

summary(gbif_occ)
head(gbif_occ)

shapefile(as_Spatial(all_occ),file = "occ_data/all_occs_post1970.shp")

### Coordinates were cleaned manually, because the automated
### cleaning was too hard on the data

# # flag bad data
# flags_spatial <- clean_coordinates(x = gbif_occ,
#                                 species = "name",
#                                 lon = "longitude",
#                                 lat = "latitude",
#                                 tests = c(#"capitals", # radius around capitals
#                                           #"centroids", # radius around country and province centroids
#                                           "duplicates", # records from one species with identical coordinates
#                                           #"equal", # equal coordinates
#                                           #"gbif", # radius around GBIF headquarters
#                                           #"institutions", # radius around biodiversity institutions
#                                           "seas")#, # in the sea
#                                           #"urban", # within urban area
#                                           #"validity", # outside reference coordinate system
#                                           #"zeros" # plain zeros and lat = lon
#                                           #)
#                                 )
# 
# # exclude records flagged by any test, exclude old records and
# # convert to spatial object

clean_occ <- st_read('occ_data/EU_clean_occs_post1970.shp')

# take only occurrences in the British Isles
bis_sf <- readRDS('carto_data/british_isles.rds')
bis_occ <- clean_occ %>% st_intersection(bis_sf)

tmap_mode("view")
data(World)
tm_shape(World) + tm_borders() +
    tm_shape(sp_gbif_all) + tm_dots(size=0.25, col='red') +
    tm_shape(clean_occ) + tm_dots(size=0.25, col = 'blue') +
    tm_shape(bis_occ) + tm_dots(size=0.25, col = 'green')



# How many observations?
nrow(clean_occ)
nrow(uk_occ)

saveRDS(clean_occ,"./occ_data/EU_occs.rds")
saveRDS(uk_occ,'./occ_data/uk_occ.rds')