##### Obtaining Chelsa data for SDM

### Load Required Libraries

library(raster) # For reading and manipulating rasters
library(sf) # For managing vector data
library(tmap) # For map visualization
library(rstudioapi) # For interaction with the WSL terminal

### Download CHELSA data 

# Using wget on the command line:
tlist <- terminalList() # Get the terminal ID

terminalSend(tlist[1],'cd env_data/CHELSA/present\n') # Change directory on terminal
# Send download command to terminal
terminalSend(tlist[1],
             'wget -r --no-parent -e robots=off --accept *.tif https://www.wsl.ch/lud/chelsa/data/bioclim/integer/\n')

terminalSend(tlist[1],'cd env_data/CHELSA/future/\n')
terminalSend(tlist[1],
             'wget -r --no-parent -e robots=off --accept *.tif https://www.wsl.ch/lud/chelsa/data/cmip5/2061-2080/bio/\n')

### Loading and cutting rasters

# First, we get the vector file with the British Isles
# bis_sf <- readRDS('carto_data/british_isles.rds')

# Get raster file with mask for Europe
eu_mask <- raster('carto_data/EUmask.tif')
plot(eu_mask)

# Then, we read the global CHELSA files from disk as a stack:
ch_files <- list.files('env_data/CHELSA/present/',
                       pattern = 'tif$',
                       full.names = TRUE)

ch_glo <- stack(ch_files)

uk_env <- mask(ch_glo,as_Spatial(bis_sf))

writeRDS(uk_env,file = 'env_data/CHELSA/chelsa_present.rds')
unlink('./env_data/CHELSA/www.wsl.ch/', recursive = TRUE)