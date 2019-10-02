### Make future rasters

library(raster)

setwd('D:/Projects/truffle_sdm/')

rcp_45_ACCESS1_list <- list.files('env_data/CHELSA/future/rcp_45_2090/ACCESS1/',
                     pattern='tif$',
                     full.names = TRUE)

rcp_45_ACESS1_stack <- raster::stack(list.files,quick=TRUE)
