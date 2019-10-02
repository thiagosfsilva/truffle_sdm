# -------------------------------------------------------------------------
# install packages
# mauricio vancine - mauricio.vancine@gmail.com
# 27-06-2019
# -------------------------------------------------------------------------

# occurrences -------------------------------------------------------------
# manipulation and visualization
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(lubridate)) install.packages("lubridate", dependencies = TRUE)

# occurrences download
if(!require(spocc)) install.packages("spocc", dependencies = TRUE)

# taxonomy clear
if(!require(taxize)) install.packages("taxize", dependencies = TRUE)

# occurrences clear
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner", dependencies = TRUE)

# variables ------------------------------------------------------
# manipulation and visualization
if(!require(ggspatial)) install.packages("ggspatial", dependencies = TRUE)
if(!require(landscapetools)) install.packages("landscapetools", dependencies = TRUE)
if(!require(raster)) install.packages("raster", dependencies = TRUE)
if(!require(rgdal)) install.packages("rgdal", dependencies = TRUE)
if(!require(wesanderson)) devtools::install_github("karthik/wesanderson")

# limits
if(!require(rnaturalearth)) install.packages("rnaturalearth", dependencies = TRUE)
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata", dependencies = TRUE)
if(!require(rnaturalearthhires)) install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")

# selection - correlation
if(!require(corrr)) install.packages("corrr", dependencies = TRUE)
if(!require(caret)) install.packages("caret", dependencies = TRUE)
if(!require(psych)) install.packages("psych", dependencies = TRUE)

# selection - pca
if(!require(factoextra)) install.packages("factoextra", dependencies = TRUE)
if(!require(FactoMineR)) install.packages("FactoMineR", dependencies = TRUE)
if(!require(RStoolbox)) install.packages("RStoolbox", dependencies = TRUE)

# algorithms --------------------------------------------------------------
# bioclim, domain, and mahalanobis
if(!require(dismo)) install.packages("dismo", dependencies = TRUE)

# svm
if(!require(kernlab)) install.packages("kernlab", dependencies = TRUE)

# random forest
if(!require(randomForest)) install.packages("randomForest", dependencies = TRUE)

# maxent
if(!require(rJava)) install.packages("rJava", dependencies = TRUE) # download and install java: https://www.java.com/en/download/

# notification sound ------------------------------------------------------
if(!require(beepr)) install.packages("beepr", dependencies = TRUE)

# end ---------------------------------------------------------------------