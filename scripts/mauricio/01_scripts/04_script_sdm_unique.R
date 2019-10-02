# -------------------------------------------------------------------------
# sdm - unique algorithms
# mauricio vancine - mauricio.vancine@gmail.com
# 27-06-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(dismo)
library(raster)
library(rgdal)
library(rJava)
library(tidyverse)
library(wesanderson)

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
  raster::stack()
var

# map
landscapetools::show_landscape(var$wc20_masknebrazil_res05g_pc01) +
  geom_point(data = occ, aes(longitude, latitude), size = 3, alpha = .7)

# verify maxent -----------------------------------------------------------
# copy maxent.jar in "~\dismo\java"
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

# enms --------------------------------------------------------------------
# diretory
setwd(path)
dir.create("04_sdm_unique")
setwd("04_sdm_unique")

# preparate data ----------------------------------------------------------
# selecting presence and background data
pr_specie <- occ %>% 
  dplyr::select(longitude, latitude) %>% 
  dplyr::mutate(id = seq(nrow(.)))
pr_specie

bg_specie <- dismo::randomPoints(mask = var, n = nrow(pr_specie)) %>% # limite de amostragem e numero de pontos de backgound
  tibble::as_tibble() %>%
  dplyr::rename(longitude = x, latitude = y) %>% 
  dplyr::mutate(id = seq(nrow(.)))
bg_specie

landscapetools::show_landscape(var$wc20_masknebrazil_res05g_pc01) +
  geom_point(data = pr_specie, aes(longitude, latitude), size = 3, alpha = .7, color = "red") +
  geom_point(data = bg_specie, aes(longitude, latitude), size = 3, alpha = .7, color = "blue")

# selecting train and test coordinates	
pr_sample_train <- pr_specie %>% 
  dplyr::sample_frac(.7) %>% 
  dplyr::select(id) %>% 
  dplyr::pull()
pr_sample_train

bg_sample_train <- bg_specie %>% 
  dplyr::sample_frac(.7) %>% 
  dplyr::select(id) %>% 
  dplyr::pull()
bg_sample_train

landscapetools::show_landscape(var$wc20_masknebrazil_res05g_pc01) +
  geom_point(data = pr_specie %>% dplyr::filter(id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 20) +
  geom_point(data = pr_specie %>% dplyr::filter(!id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 8) +
  geom_point(data = bg_specie %>% dplyr::filter(id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "blue", pch = 20) +
  geom_point(data = bg_specie %>% dplyr::filter(!id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "blue", pch = 8)

# preparating train and test data
train <- dismo::prepareData(x = var, 
                            p = pr_specie %>% dplyr::filter(id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                            b = bg_specie %>% dplyr::filter(id %in% bg_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
train
head(train)

test <- dismo::prepareData(x = var, 
                           p = pr_specie %>% dplyr::filter(!id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                           b = bg_specie %>% dplyr::filter(!id %in% bg_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
test
head(test)

# fit models --------------------------------------------------------------
#  presence only
# 1 bioclim
# 1.1 fit
bioclim_fit <- dismo::bioclim(x = train[train$pb == 1, -1])	
plot(bioclim_fit)
dismo::response(bioclim_fit)

# 1.2 projection
bioclim_proj <- dismo::predict(var, bioclim_fit, progress = "text")	
bioclim_proj

# model export
raster::writeRaster(x = bioclim_proj, 
                    filename = "enm_haddadus_binotatus_bioclim", 
                    format = "GTiff", 
                    options = c("COMPRESS=DEFLATE"), 
                    overwrite = TRUE)

# map
landscapetools::show_landscape(bioclim_proj)

# 1.3 evaluation
bioclim_eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-1),  
                                a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-1), 
                                model = bioclim_fit)
bioclim_eval 

# roc
plot(bioclim_eval, "ROC")

# auc
bioclim_eval@auc

# threshold
bioclim_eval@t
bioclim_eval_thr <- dismo::threshold(bioclim_eval)
bioclim_eval_thr

# tss
bioclim_eval_thr_id_spec_sens <- which(bioclim_eval@t == dismo::threshold(bioclim_eval, "spec_sens"))
bioclim_eval_thr_id_spec_sens

bioclim_eval_tss_spec_sens <- bioclim_eval@TPR[bioclim_eval_thr_id_spec_sens] + bioclim_eval@TNR[bioclim_eval_thr_id_spec_sens] - 1
bioclim_eval_tss_spec_sens

# TPR: True positive rate
# TNR: True negative rate

# threshold cut
# sum of the sensitivity and specificity
bioclim_eval_thr$spec_sens

bioclim_proj_thr_spec_sens <- bioclim_proj >= bioclim_eval_thr$spec_sens
bioclim_proj_thr_spec_sens

landscapetools::show_landscape(bioclim_proj_thr_spec_sens, discrete = TRUE) +
  geom_point(data = pr_specie %>% dplyr::filter(id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 20) +
  geom_point(data = pr_specie %>% dplyr::filter(!id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 8)

#  presence absence -----------------------------------------------------
# 2 glm
# 2.1 fit
glm_fit <- glm(pb ~ ., family = binomial, data = train)	
summary(glm_fit)
dismo::response(glm_fit)

# 2.2 projection
glm_proj <- dismo::predict(var, glm_fit, progress = "text")	
glm_proj

# model export
raster::writeRaster(x = glm_proj, 
                    filename = "enm_haddadus_binotatus_glm", 
                    format = "GTiff", 
                    options = c("COMPRESS=DEFLATE"), 
                    overwrite = TRUE)

# map
landscapetools::show_landscape(glm_proj)

# 2.3 evaluation
glm_eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-1),  
                            a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-1), 
                            model = glm_fit)
glm_eval 

# roc
plot(glm_eval, "ROC")

# auc
glm_eval@auc

# threshold
glm_eval@t
glm_eval_thr <- dismo::threshold(glm_eval)
glm_eval_thr

# tss
glm_eval_thr_id_spec_sens <- which(glm_eval@t == dismo::threshold(glm_eval, "spec_sens"))
glm_eval_thr_id_spec_sens

glm_eval_tss_spec_sens <- glm_eval@TPR[glm_eval_thr_id_spec_sens] + glm_eval@TNR[glm_eval_thr_id_spec_sens] - 1
glm_eval_tss_spec_sens

# threshold cut
# sum of the sensitivity and specificity
glm_eval_thr$spec_sens

glm_proj_thr_spec_sens <- glm_proj >= glm_eval_thr$spec_sens
glm_proj_thr_spec_sens

landscapetools::show_landscape(glm_proj_thr_spec_sens, discrete = TRUE) +
  geom_point(data = pr_specie %>% dplyr::filter(id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 20) +
  geom_point(data = pr_specie %>% dplyr::filter(!id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 8)

#  presence-background --------------------------------------------------
# 3 maxent
# 3.1 fit
maxent_fit <- dismo::maxent(x = train[, -1], p = train[, 1])
plot(maxent_fit)
response(maxent_fit)
maxent_fit # browser da internet

# 3.1.2 projection
maxent_proj <- dismo::predict(var, maxent_fit, progress = "text")	
maxent_proj

# model export
raster::writeRaster(x = maxent_proj, 
                    filename = "enm_haddadus_binotatus_maxent", 
                    format = "GTiff", 
                    options = c("COMPRESS=DEFLATE"), 
                    overwrite = TRUE)

# map
landscapetools::show_landscape(maxent_proj)

# 3.1.3 evaluation
maxent_eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-1),  
                               a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-1), 
                               model = maxent_fit)
maxent_eval 

# roc
plot(maxent_eval, "ROC")

# auc
maxent_eval@auc

# threshold
maxent_eval@t
maxent_eval_thr <- dismo::threshold(maxent_eval)
maxent_eval_thr

# tss
maxent_eval_thr_id_spec_sens <- which(maxent_eval@t == dismo::threshold(maxent_eval, "spec_sens"))
maxent_eval_thr_id_spec_sens

maxent_eval_tss_spec_sens <- maxent_eval@TPR[maxent_eval_thr_id_spec_sens] + maxent_eval@TNR[maxent_eval_thr_id_spec_sens] - 1
maxent_eval_tss_spec_sens

# thresholds
# sum of the sensitivity and specificity
maxent_eval_thr$spec_sens

maxent_proj_thr_spec_sens <- maxent_proj >= maxent_eval_thr$spec_sens
maxent_proj_thr_spec_sens

landscapetools::show_landscape(maxent_proj_thr_spec_sens, discrete = TRUE) +
  geom_point(data = pr_specie %>% dplyr::filter(id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 20) +
  geom_point(data = pr_specie %>% dplyr::filter(!id %in% pr_sample_train), 
             aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 8)

# maps --------------------------------------------------------------------
sdm <- raster::stack(bioclim_proj, bioclim_proj_thr_spec_sens, 
                     glm_proj, glm_proj_thr_spec_sens, 
                     maxent_proj, maxent_proj_thr_spec_sens)
sdm

names(sdm) <- c("bioclim", "bioclim_thr", "glm", "glm_thr", "maxent", "maxent_thr")
sdm

landscapetools::show_landscape(sdm, n_col = 2, unique_scales = TRUE, discrete = TRUE)

# end ---------------------------------------------------------------------