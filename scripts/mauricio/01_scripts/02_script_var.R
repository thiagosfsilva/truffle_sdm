# -------------------------------------------------------------------------
# var - download, adjust extention, resolution, correlation and pca
# mauricio vancine - mauricio.vancine@gmail.com
# 27-06-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(factoextra)
library(FactoMineR)
library(landscapetools)
library(psych)
library(raster)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(RStoolbox)
library(tidyverse)
library(wesanderson)

# informations
# https://ropensci.org/
# https://github.com/ropensci/rnaturalearth
# https://www.naturalearthdata.com/
# https://github.com/r-spatial/sf
# https://www.worldclim.org/

# directory
path <- "/home/mude/data/curso_mde_9cbh"
setwd(path)
dir()

# download ----------------------------------------------------------------
# directory
dir.create("03_var")
setwd("03_var")

# download
download.file(url = "http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_10m_bio.zip",
              destfile = "wc2.0_10m_bio.zip")

# unzip
unzip("wc2.0_10m_bio.zip")

# delete zip file
unlink("wc2.0_10m_bio.zip")

# bioclimates
# BIO01 = Temperatura media anual
# BIO02 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO03 = Isotermalidade (BIO02/BIO07) (* 100)
# BIO04 = Sazonalidade da temperatura (desvio padrao deviation *100)
# BIO05 = Temperatura maxima do mes mais quente
# BIO06 = Temperatura minima do mes mais frio
# BIO07 = Variacao da temperatura anual (BIO5-BIO6)
# BIO08 = Temperatura media do trimestre mais chuvoso
# BIO09 = Temperatura media do trimestre mais seco
# BIO10 = Temperatura media do trimestre mais quente
# BIO11 = Temperatura media do trimestre mais frio
# BIO12 = Precipitacao anual
# BIO13 = Precipitacao do mes mais chuvoso
# BIO14 = Precipitacao do mes mais seco
# BIO15 = Sazonalidade da precipitacao (coeficiente de variacao)
# BIO16 = Precipitacao do trimestre mais chuvoso
# BIO17 = Precipitacao do trimestre mais seco
# BIO18 = Precipitacao do trimestre mais quente
# BIO19 = Precipitacao do trimestre mais frio

# adjust extention --------------------------------------------------------
# limit
br <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
  filter(name == "Brazil")
br

# plot
ggplot() +
  geom_sf(data = br) +
  theme_bw()

# import bioclimates
# list files
tif <- dir(pattern = "tif$")
tif

# import
var <- raster::stack(tif)
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)
var

# plot
plot(var$bio01)

# adust extention
# crop = adjust extention
var_crop_br <- raster::crop(x = var, y = br)
var_crop_br

# plot
plot(var_crop_br$bio01)

# adjust to limite
# mask = adjust to mask
var_mask_br <- raster::mask(x = var_crop_br, mask = br)
var_mask_br

# plot
plot(var_mask_br$bio01)

# adjust resolution -------------------------------------------------------
# plot
landscapetools::show_landscape(var_mask_br$bio01) 

# resolution
raster::res(var_mask_br)

# aggregation factor 
res_actual <- raster::res(var_mask_br)[1]
res_actual

res_adjust <- 0.5
res_adjust

agg_fac <- res_adjust/res_actual
agg_fac

# aggregation
var_mask_br_05 <- raster::aggregate(var_mask_br, fact = agg_fac)
var_mask_br_05

# new resolution
raster::res(var_mask_br_05)[1]

# plot
landscapetools::show_landscape(var_mask_br$bio01)
landscapetools::show_landscape(var_mask_br_05$bio01)

# export
dir.create("00_var")
setwd("00_var")
raster::writeRaster(x = var_mask_br_05, 
                    filename = paste0("wc20_masknebrazil_res05g_", names(var_mask_br)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# exclude
setwd("..")
dir(pattern = ".tif") %>% 
  unlink()

# correlation -------------------------------------------------------------
# directory
dir.create("01_correlation") 
setwd("01_correlation")
getwd()

# extract values
var_da <- var_mask_br_05 %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  tidyr::drop_na()
var_da

# verify
head(var_da)
dim(var_da)

# correlation
cor_table <- corrr::correlate(var_da, method = "spearman") 
cor_table

# preparate table
cor_table_summary <- cor_table %>% 
  corrr::shave() %>%
  corrr::fashion()
cor_table_summary

# export
readr::write_csv(cor_table_summary, "correlation_table.csv")

# plot
cor_table %>% 
  corrr::shave() %>% 
  corrr::rplot(print_cor = TRUE, 
             colors = wesanderson::wes_palette(name = "Zissou1", n = 10, type = "continuous"))

# export
ggsave(filename = "correlation.tiff", wi = 20, he = 15, units = "cm", dpi = 300, comp = "lzw")

# select variables
# correlated variables
fi_07 <- cor_table %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)
fi_07

# select
var_da_cor07 <- var_da %>% 
  dplyr::select(-fi_07)
var_da_cor07

# verify
var_da_cor07 %>% 
  corrr::correlate(method = "spearman") %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)

# graphic
pairs.panels(var_da_cor07 %>% 
               dplyr::sample_n(1e3), 
             method = "spearman",
             pch = 20, 
             ellipses = FALSE, 
             density = FALSE, 
             stars = TRUE, 
             hist.col = "gray",
             digits = 2,
             rug = FALSE,
             breaks = 10,
             ci = TRUE)

# pca ---------------------------------------------------------------------
# directory
setwd("..")
dir.create("02_pca") 
setwd("02_pca") 

# pca
pca <- FactoMineR::PCA(var_da, scale.unit = TRUE, graph = FALSE)
pca

# eigenvalues
eig <- factoextra::get_eig(pca) %>% 
  tibble::as_tibble() %>% 
  round(2) %>% 
  dplyr::mutate(id = rownames(factoextra::get_eig(pca))) %>% 
  dplyr::select(id, everything())
eig
readr::write_csv(eig, "pca_eigenvalues.csv")

# eigenvalues plot
factoextra::fviz_eig(pca, addlabels = TRUE, ggtheme = theme_classic())
ggsave("pca_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300, comp = "lzw")

# contributions
loa <- pca$var$contrib %>% round(2)
loa
write.csv(loa, "pca_contributions.csv")

# biplot
factoextra::fviz_pca(pca, geom = "point", alpha.ind = .5, repel = TRUE, ggtheme = theme_bw())
ggsave("pca_biplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300, comp = "lzw")

# raster pca
pca_raster <- RStoolbox::rasterPCA(var_mask_br_05, spca = TRUE) 
pca_raster

# plot pca map
landscapetools::show_landscape(pca_raster$map$PC1)

# export
raster::writeRaster(x = pca_raster$map[[1:6]], 
                    filename = paste0("wc20_masknebrazil_res05g_pc0", 1:6), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# end ---------------------------------------------------------------------