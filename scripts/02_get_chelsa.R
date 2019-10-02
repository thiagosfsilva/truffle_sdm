##### Obtaining Chelsa data for SDM

### Load Required Libraries

library(raster) # For reading and manipulating rasters
library(sf) # For managing vector data
library(tmap) # For map visualization

### Download CHELSA data 

# Using wget on the command line:
setwd('"D:/Projects/truffle_sdm/env_data/CHELSA/present')
system('wget -r --no-parent -e robots=off --accept *.tif https://www.wsl.ch/lud/chelsa/data/bioclim/integer/')

setwd('D:/Projects/truffle_sdm/env_data/CHELSA/future/')
system('wget -r --no-parent -e robots=off --accept *.tif https://www.wsl.ch/lud/chelsa/data/cmip5/2061-2080/bio/')



### Loading and cutting rasters

# First, we get the vector file with the British Isles

bis_sf <- readRDS('carto_data/british_isles.rds')

# Then, we read the global CHELSA files from disk as a stack:
ch_files <- list.files('env_data/CHELSA/www.wsl.ch/lud/chelsa/data/bioclim/integer/',
                       pattern = 'tif$',
                       full.names = TRUE)

ch_glo <- stack(ch_files)

uk_env <- mask(ch_glo,as_Spatial(bis_sf))

writeRDS(uk_env,file = 'env_data/CHELSA/chelsa_present.rds')
unlink('./env_data/CHELSA/www.wsl.ch/', recursive = TRUE)