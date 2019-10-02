# -------------------------------------------------------------------------
# maps
# mauricio vancine - mauricio.vancine@gmail.com
# 27-06-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ggspatial)
library(raster)
library(rgdal)
library(rnaturalearth)
library(tidyverse)

# directory
path <- "/home/mude/data/curso_mde_9cbh"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
setwd("02_occ")
occ <- readr::read_csv("occ_data_spocc_data_filter_taxonomic_date_spatial_oppc.csv")
occ

# brazil
br <- rnaturalearth::ne_states(country = "Brazil", returnclass = "sf")
br

# names
na <- paste(occ$species %>% unique %>% stringr::str_to_title() %>% stringr::str_replace("_", " "), 
            c("Ensemble weighted average", "Ensemble weighted average - LPT", "Ensemble weighted average - P10", 
              "Ensemble weighted average - P20", "Ensemble frequency", "Ensemble frequency - LPT", 
              "Ensemble frequency - P10", "Ensemble frequency - P20"), sep = " - ")
na

# figs --------------------------------------------------------------------
# plot
for(i in occ$species %>% unique){}
  
  # information
  print(i)
  
  # ensembles
  setwd(path)
  setwd(paste0("05_sdm_multiple/", i))
  
  # import
  ens <- dir(pattern = "ensemble_", recursive = TRUE) %>% 
    raster::stack()
  
  # directory
  dir.create("04_maps")
  setwd("04_maps")
  
  # map
  for(j in ens %>% raster::nlayers() %>% seq){
    
    map <- ggplot() +
      geom_raster(data = raster::rasterToPoints(ens[[j]]) %>% tibble::as_tibble() %>% dplyr::rename(ens = names(ens[[j]])),
                  aes(x, y, fill = ens)) +
      geom_sf(data = br, fill = NA, color = "black") +
      geom_point(data = occ, aes(longitude, latitude), size = 2, alpha = .7) +
      scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", n = 100, type = "continuous")) +
      labs(x = "Longitude", y = "Latitude", fill = "ade", title = na[[j]]) +
      theme_bw() +
      coord_sf(xlim = c(-74, -35)) +
      theme(title = element_text(size = 10, face = "bold.italic"),
            legend.title = element_text(size = 15, face = "bold"),
            axis.title = element_text(size = 15, face = "plain"),
            legend.position = c(.92, .15)) +
      annotation_scale(location = "bl", width_hint = .3) +
      annotation_north_arrow(location = "bl", which_north = "true", 
                             pad_x = unit(1.35, "cm"), pad_y = unit(.8, "cm"),
                             style = north_arrow_fancy_orienteering)
    
    # export
    ggsave(paste0("map_", names(ens[[j]]), ".tiff"), map, wi = 20, he = 20, un = "cm", dpi = 300, comp = "lzw")
    
  }
  
}

# end ---------------------------------------------------------------------