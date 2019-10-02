### Distribution modelling with ENMeval


# Load necessary packages 
#library(dplyr) # for data manipulation
library(sf) # for spatial data formats and operations
library(sp) # for spatial data formats and operations
library(raster) # to read and manupulate raster data
library(dismo) # tools for distribution modelling
library(ENMeval) # more tools for distribution modelling
library(tmap) # for spatial visualization

# Load required data
eu_mask <- raster('carto_data/EUmask.tif')
env_pres <- readRDS('env_data/CHELSA/chelsa_bioclim_present.rds')
bg_occ <- readRDS('occ_data/EU_background_points.rds')
sp_occ <- readRDS('occ_data/EU_occs.rds')
map_bounds <- readRDS('carto_data/europe_coastline.rds')

# extract points coordinates only from sp locations
occ = st_coordinates(sp_occ)
bg = st_coordinates(bg_occ)

opts <- c("L", "Q", "H","P","T")

cmbns <- unlist(lapply(1:5, function(x) combn(opts, x, FUN = paste, collapse = "")))

eu_model_pres <- ENMevaluate(occ,env_pres, bg,
                          method='randomkfold',
                          kfolds = 10,
                          overlap = TRUE,
                          RMvalues = seq(from = 1, to = 3, by = 0.5),
                          fc = cmbns,
                          algorithm = 'maxent.jar',
                          parallel=TRUE,
                          rasterPreds = FALSE,
                          numCores=8)

saveRDS(eu_model_pres, './results/eu_present_maxent_results_nopreds.rds')

