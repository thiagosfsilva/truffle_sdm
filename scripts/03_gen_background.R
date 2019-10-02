### Generate background points

# Load required packages
library(dismo) # tools for distribution modelling
library(sp) # spatial data viz
library(tmap)


setwd('D:/Projects/truffle_sdm/')

# Get Raster Mask
eu_mask <- raster('carto_data/EUmask.tif')
eu_mask[eu_mask==0] <- NA 

# Generate points
set.seed(1979) # Fix random generator
bg <- randomPoints(mask = eu_mask, # define the mask
                   n = 20000, # number of bg samples
                   ext = extent(eu_mask), # extent of the sampling
                   lonlatCorrection = FALSE) # Accouts for different cell sizes when sampling in lat/long

# Convert to spatial data
sp_bg <- st_as_sf(SpatialPoints(bg,proj4string = crs(eu_mask)))

tmap_mode("view")
tm_shape(sp_bg) + tm_dots()

# Save as RDS
saveRDS(sp_bg, './occ_data/EU_background_points.rds')
