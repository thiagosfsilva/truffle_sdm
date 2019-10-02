#### Make raster mask of the EU and UK

library(raster)
library(sf)
library(tmap)
# Reference raster

rref <- raster('env_data/CHELSA/present/CHELSA_bio10_01.tif')

# EU shape
eeuu <- shapefile('carto_data/Europe.shp')
eeuu_sf <- st_as_sf(eeuu)

tmap_mode('plot')

# Get EU bounding box
bbx <- bb(eeuu,output='extent')

# Crop EU raster
subras <-!is.na(crop(rref,bbx))

tm_shape(subras) + tm_raster() #+ tm_shape(eeuu) + tm_borders()

subras <- raster('carto_data/EUmask.tif')

mask(subras,eeuu, file='carto_data/EUmask.tif',
     datatype='INT1U',
     overwrite=TRUE)





