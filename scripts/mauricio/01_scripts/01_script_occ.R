# -------------------------------------------------------------------------
# occ - download and filter
# mauricio vancine - mauricio.vancine@gmail.com
# 27-06-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(CoordinateCleaner)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(spocc)
library(taxize)
library(tidyverse)

# informations
# https://ropensci.org/
# https://ropensci.github.io/spocc/
# https://cloud.r-project.org/web/packages/spocc/index.html
# https://cloud.r-project.org/web/packages/spocc/vignettes/spocc_vignette.html
# https://ropensci.github.io/taxize/
# https://cloud.r-project.org/web/packages/taxize/index.html
# https://cloud.r-project.org/web/packages/taxize/vignettes/taxize_vignette.html
# https://ropensci.github.io/CoordinateCleaner/
# https://cloud.r-project.org/web/packages/CoordinateCleaner/index.html
# https://ropensci.github.io/CoordinateCleaner/articles/Tutorial_Cleaning_GBIF_data_with_CoordinateCleaner.html
# https://github.com/ropensci/rnaturalearth
# https://www.naturalearthdata.com/
# https://github.com/r-spatial/sf

# directory
path <- "/home/mude/data/curso_mde_9cbh"
setwd(path)
dir()

# download occurrences ----------------------------------------------------
# species
sp <- c("Hypsiboas albopunctatus")
sp

# bases for download
db <- c("gbif",       # Global Biodiversity Information Facility (https://www.gbif.org/)
        "ecoengine",  # Berkeley Initiative for Global Change Biology (https://ecoengine.berkeley.edu/)
        "inat",       # iNaturalist (https://www.inaturalist.org/)
        "vertnet",    # VertNet (http://vertnet.org/)
        "ebird",      # eBird (https://ebird.org/)
        "idigbio",    # Integrated Digitized Biocollections (https://www.idigbio.org/)
        "obis",       # Ocean Biogeographic Information System (www.iobis.org)
        "ala",        # Atlas of Living Australia (https://www.ala.org.au/)
        "bison"       # Biodiversity Information Serving Our Nation (https://bison.usgs.gov)
        )
db

# occ download
occ <- spocc::occ(query = sp, 
                     from = db, 
                     ebirdopts = list(key = "t3bbcnthdbjs"), # make key in https://ebird.org/api/keygen
                     has_coords = TRUE, 
                     limit = 1e6)
occ

# get data
occ_data <- occ %>%
  spocc::occ2df() %>% 
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = lubridate::year(date),
                base = prov) %>% 
  dplyr::select(name, longitude, latitude, year, base)
occ_data

# limit brazil
br <- rnaturalearth::ne_states(country = "Brazil", returnclass = "sf")
br

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_data, aes(x = longitude, y = latitude)) +
  theme_bw()

# taxonomic filter --------------------------------------------------------
# gnr names
gnr <- taxize::gnr_resolve(sp)
gnr

# adjust names
gnr_tax <- gnr %>% 
  dplyr::mutate(species = submitted_name %>% 
                  stringr::str_to_lower() %>% 
                  stringr::str_replace(" ", "_")) %>% 
  dplyr::select(species, matched_name) %>% 
  dplyr::distinct()
gnr_tax

# confer
occ_data %>%
  dplyr::select(name) %>% 
  table %>% 
  tibble::as_tibble()

# taxonomic filter
occ_data_tax <- dplyr::inner_join(occ_data, gnr_tax, c(name = "matched_name")) %>% 
  dplyr::arrange(name) %>% 
  dplyr::select(name, species, everything())
occ_data_tax

# confer
occ_data$name %>% 
  table %>% 
  tibble::as_tibble()

occ_data_tax$name %>% 
  table %>% 
  tibble::as_tibble()

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_data, aes(x = longitude, y = latitude)) +
  geom_point(data = occ_data_tax, aes(x = longitude, y = latitude), color = "red") +
  theme_bw()

# date filter -------------------------------------------------------------
# verify
occ_data_tax$year %>% 
  table(useNA = "always")

# year > 1960
occ_data_tax_date <- occ_data_tax %>% 
  dplyr::filter(year > 1960, year <= 2019, !is.na(year)) %>% 
  dplyr::arrange(year)
occ_data_tax_date

# verify
occ_data_tax$year %>% 
  table(useNA = "always")

occ_data_tax_date$year %>% 
  table(useNA = "always")

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_data, aes(x = longitude, y = latitude)) +
  geom_point(data = occ_data_tax_date, aes(x = longitude, y = latitude), color = "red") +
  theme_bw()

# spatial filter ----------------------------------------------------------
# remove na
occ_data_na <- occ_data_tax_date %>% 
  tidyr::drop_na(longitude, latitude)
occ_data_na

# flag data
flags_spatial <- CoordinateCleaner::clean_coordinates(
  x = occ_data_na, 
  species = "species",
  lon = "longitude", 
  lat = "latitude",
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            "seas", # in the sea
            "urban", # within urban area
            "validity", # outside reference coordinate system
            "zeros", # plain zeros and lat = lon
            
  )
)

# results
flags_spatial %>% head
summary(flags_spatial)
flags_spatial$.summary

# exclude records flagged by any test
occ_data_tax_date_spa <- occ_data_na %>% 
  dplyr::filter(flags_spatial$.summary)
occ_data_tax_date_spa

# resume data
occ_data_na$species %>% 
  table

occ_data_tax_date_spa$species %>% 
  table

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_data_na, aes(x = longitude, y = latitude)) +
  geom_point(data = occ_data_tax_date_spa, aes(x = longitude, y = latitude), color = "red", alpha = .7) +
  theme_bw()

# verify filters ----------------------------------------------------------
occ_data_tax$species %>% table
occ_data_tax_date$species %>% table
occ_data_tax_date_spa$species %>% table

# export ------------------------------------------------------------------
# directory
dir.create("02_occ")
setwd("02_occ")

# export
readr::write_csv(occ_data, paste0("occ_spocc_raw_", lubridate::today(), ".csv"))
readr::write_csv(occ_data_tax_date_spa, paste0("occ_data_spocc_data_filter_taxonomic_date_spatial.csv"))

# end ---------------------------------------------------------------------