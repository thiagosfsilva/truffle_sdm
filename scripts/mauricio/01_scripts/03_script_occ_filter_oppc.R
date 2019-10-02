# -------------------------------------------------------------------------
# occ - spatial filter - oppc
# mauricio vancine - mauricio.vancine@gmail.com
# 27-06-2019
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

# import data -------------------------------------------------------------
# occ
setwd("02_occ")
occ <- readr::read_csv("occ_data_spocc_data_filter_taxonomic_date_spatial.csv")
occ

# var
setwd(path); setwd("03_var/02_pca"); 

var <- dir(pattern = "tif$", recursive = TRUE) %>% 
  raster::stack()
var

landscapetools::show_landscape(var$wc20_masknebrazil_res05g_pc01) +
  geom_point(data = occ, aes(longitude, latitude), size = 3, alpha = .7)

# one point per cell - oppc -----------------------------------------------
# raster id from cell
var_id <- var$wc20_masknebrazil_res05g_pc01
var_id

var_id[!is.na(var_id)] <- raster::cellFromXY(var_id, raster::rasterToPoints(var_id)[, 1:2])
landscapetools::show_landscape(var_id)

# oppc
occ_oppc <- occ %>% 
  dplyr::mutate(oppc = raster::extract(var_id, dplyr::select(., longitude, latitude))) %>% 
  dplyr::distinct(species, oppc, .keep_all = TRUE) %>% 
  dplyr::filter(!is.na(oppc)) %>% 
  dplyr::add_count(species) %>% 
  dplyr::arrange(species)
occ_oppc

# verify
table(occ$species)
table(occ_oppc$species)

# map
landscapetools::show_landscape(var$wc20_masknebrazil_res05g_pc01) +
  geom_point(data = occ, aes(longitude, latitude), size = 3, alpha = .7) +
  geom_point(data = occ_oppc, aes(longitude, latitude), size = 3, pch = 20, alpha = .7, color = "red")

# export ------------------------------------------------------------------
setwd(path); setwd("02_occ")
readr::write_csv(occ_oppc, paste0("occ_data_spocc_data_filter_taxonomic_date_spatial_oppc.csv"))

# end ---------------------------------------------------------------------