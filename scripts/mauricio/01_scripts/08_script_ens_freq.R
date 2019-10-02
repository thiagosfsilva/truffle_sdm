# -------------------------------------------------------------------------
# ensemble - frequency
# mauricio vancine - mauricio.vancine@gmail.com
# 26-06-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(landscapetools)
library(raster)
library(rgdal)
library(tidyverse)

# directory
path <- "/home/mude/data/curso_mde_9cbh"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("05_sdm_multiple")

# list files
csv <- dir(pattern = "eval_", recursive = TRUE)
csv

# import models
eva <- purrr::map_dfr(csv, readr::read_csv)
eva

# frequency ensemble  -----------------------------------------------------
# tss
tss_limit <- .5

# algorithms
alg <- eva$algorithm %>% unique
alg

# ensemble
for(i in eva$species %>% unique){
  
  # information
  print(paste("Ensemble to", i))
  
  # selection
  eva_i <- eva %>% 
    dplyr::filter(species == i, 
                  tss_spec_sens >= tss_limit, 
                  algorithm %in% alg)
  
  # raster
  ens <- eva_i %>% 
    dplyr::slice(1) %>% 
    dplyr::select(file) %>% 
    dplyr::mutate(file = paste0(i, "/00_replicas/", file)) %>% 
    dplyr::pull() %>% 
    raster::raster()
  ens[] <- 0
  
  # information
  print("The sum can take a looong time...")
  
  for(j in eva_i %>% nrow %>% seq){
    
    # import raster
    enm_j <- eva_i %>% 
      dplyr::slice(j) %>% 
      dplyr::select(file) %>% 
      dplyr::mutate(file = paste0(i, "/00_replicas/", file)) %>% 
      dplyr::pull() %>% 
      raster::raster()
    
    # select threshold
    thr_j <- eva_i %>% 
      dplyr::slice(j) %>% 
      dplyr::select(thr_max_spec_sens) %>% 
      dplyr::pull()
    
    # cut
    enm_thr <- enm_j >= thr_j
    
    # frequency sum 
    ens <- sum(ens, enm_thr)
    
  }
  
  # information
  print("All right, finish!")
  
  # ensemble frequency
  ens_f <- ens / nrow(eva_i)
  ens_f  
  
  # directory
  setwd(i)
  dir.create("03_ensemble_frequency")
  setwd("03_ensemble_frequency")
  
  # export
  raster::writeRaster(x = ens_f, 
                      filename = paste0("ensemble_frequency_", i), 
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      overwrite = TRUE)
  
  # back directory
  setwd(".."); setwd("..")
  
  # notification sound
  beepr::beep(3)
  
}

# end ---------------------------------------------------------------------