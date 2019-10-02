# -------------------------------------------------------------------------
# sdm - multiple algorithm
# mauricio vancine - mauricio.vancine@gmail.com
# 25-06-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(beepr)
library(dismo)
library(mgcv)
library(kernlab)
library(randomForest)
library(raster)
library(rgdal)
library(rJava)
library(rnaturalearth)
library(sf)
library(tidyverse)

# information
# https://cran.r-project.org/web/packages/dismo/index.html
# https://biodiversityinformatics.amnh.org/open_source/maxent/
# https://rspatial.org/sdm/
# https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf

# directory
path <- "/home/mude/data/curso_mde_9cbh"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
setwd("02_occ")
occ <- readr::read_csv("occ_data_spocc_data_filter_taxonomic_date_spatial_oppc.csv")
occ

# var
setwd(path); setwd("03_var/02_pca"); 
var <- dir(pattern = "tif$", recursive = TRUE) %>% 
  raster::stack() %>% 
  raster::brick()
var
landscapetools::show_landscape(var$wc20_masknebrazil_res05g_pc01)

# map
ggplot() +
  geom_raster(data = var$wc20_masknebrazil_res05g_pc01 %>% 
                raster::rasterToPoints() %>% 
                tibble::as_tibble() %>% 
                dplyr::rename(var = wc20_masknebrazil_res05g_pc01), 
              aes(x, y, fill = var)) +
  geom_point(data = occ, aes(longitude, latitude), size = 2.5, alpha = .7) +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_bw()

# verify maxent -----------------------------------------------------------
# copy maxent.jar in "~\dismo\java"
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

# enms --------------------------------------------------------------------
# diretory
setwd(path)
dir.create("05_sdm_multiple")
setwd("05_sdm_multiple")

# parameters
replica <- 10
partition <- .7

# enms
for(i in occ$species %>% unique){ # for to each specie
  
  # directory
  dir.create(i)
  setwd(i)
  
  # information
  paste0("Preparing data for modeling ", i, " in ", getwd()) %>% print
  
  # object for evaluation
  eval_species <- tibble::tibble()
  
  # selecting presence and absence data
  pr_specie <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  bg_specie <- dismo::randomPoints(mask = var, n = nrow(pr_specie)) %>% 
    tibble::as_tibble() %>%
    dplyr::rename(longitude = x, latitude = y) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  # directory
  dir.create("00_replicas")
  setwd("00_replicas")
  
  # replicates
  for(r in replica %>% seq){	# number of replicas
    
    # object for evaluation
    eval_algorithm <- tibble::tibble()
    
    # partitioning data	
    pr_sample_train <- pr_specie %>% 
      dplyr::sample_frac(partition) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    bg_sample_train <- bg_specie %>% 
      dplyr::sample_frac(partition) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    # train and test data
    train <- dismo::prepareData(x = var, 
                                p = pr_specie %>% dplyr::filter(id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                                b = bg_specie %>% dplyr::filter(id %in% bg_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    test <- dismo::prepareData(x = var, 
                               p = pr_specie %>% dplyr::filter(!id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                               b = bg_specie %>% dplyr::filter(!id %in% bg_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    
    ### model fitting ###
    # information
    print(paste("Models fitting to", i, "replica", r, "of", replica))
    
    # algorithms
    # presence-only - envelope
    BIO <- dismo::bioclim(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-only - distance-based
    DOM <- dismo::domain(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    MAH <- dismo::mahal(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-absence - regression 
    GLM <- glm(formula = pb ~ ., family = "binomial", data = train)
    
    # presence-absence - machine learning
    RFR <- randomForest::randomForest(formula = pb ~ ., data = train)
    SVM <- kernlab::ksvm(x = pb ~ ., data = train)
    
    # presence-background
    MAX <- dismo::maxent(x = train %>% dplyr::select(-pb), p = train %>% dplyr::select(pb))
    
    # lists
    fit <- list(bioclim = BIO, domain = DOM, mahalanobis = MAH, glm = GLM, randomforest = RFR, svm = SVM, maxent = MAX)
    
    # predict
    for(a in seq(fit)){
      
      # information
      print(paste("Model predict algorithm", fit[a] %>% names))
      
      # model predict
      model_predict <- dismo::predict(var, fit[[a]], progress = "text")
      
      # model export
      raster::writeRaster(x = model_predict, 
                          filename = paste0("enm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r)), 
                          format = "GTiff", 
                          options = c("COMPRESS=DEFLATE"), 
                          overwrite = TRUE)
      
      # model evaluation
      eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                              a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                              model = fit[[a]])
      
      # indices
      id_eval_spec_sens <- which(eval@t == dismo::threshold(eval, "spec_sens"))
      tss_spec_sens <- eval@TPR[id_eval_spec_sens] + eval@TNR[id_eval_spec_sens] - 1
      
      # evaluation data
      eval_data <- tibble::tibble(species = i, 
                                  replica = r, 
                                  algorithm = fit[a] %>% names, 
                                  thr_max_spec_sens = dismo::threshold(eval, "spec_sens"),
                                  tss_spec_sens = tss_spec_sens,
                                  auc = eval@auc, 
                                  file = paste0("enm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r), ".tif"))
      
      # combine evaluation
      eval_algorithm <- dplyr::bind_rows(eval_algorithm, eval_data)
      
    } # ends for "a"
    
    # combine evaluation
    eval_species <- dplyr::bind_rows(eval_species, eval_algorithm)
    
  } # ends for "r"
  
  # export evaluation
  setwd("..")
  
  dir.create("01_evaluation")
  setwd("01_evaluation")
  dir.create("00_raw")
  setwd("00_raw")
  
  readr::write_csv(eval_species, paste0("eval_", i, ".csv"))
  
  # directory
  setwd(".."); setwd(".."); setwd("..") 

  # notification sound
  beepr::beep(3)
  
} # ends for"i"

# end ---------------------------------------------------------------------