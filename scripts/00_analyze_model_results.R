#' #### Analyze model results

# Load necessary packages
library(dplyr) # for data manipulation
library(sf) # for spatial data formats and operations
library(sp) # for spatial data formats and operations
library(raster) # to read and manupulate raster data
library(dismo) # tools for distribution modelling
library(ENMeval) # more tools for distribution modelling
library(sdmpredictors) # for downloading environmental data
library(spocc) # to obtain species occurrence records
library(CoordinateCleaner) # cleaning up species records
library(tmap) # for spatial data viz

wgs84 <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')


#' Let's look at the model results for present climate. The overall results table is:

bis_model_pres <- readRDS('./results/bis_present_maxent_results.rds')

bis_model_pres@results


#' We can first inspect the Area Under the Curve (AUC) for the testing dataset.
#' A higher AUC means a model that best explains the data. As a reference, a model that is no better than randomly choosing probabilities has an AUC of 0.5. Values above 0.8  are considered good, and above 0.9, very good.

eval.plot(bis_model_pres@results, value = 'avg.test.AUC', legend.position="bottomright" )


#' We can see that all models performed well overall, with single-feature models (linear, hinge and quadratic) models outperforming most combined feature models, except for LH (linear + hinge) models, which performed second best overall. The next step is to evaluate model overfitting. We ca do that by looking at difference in AUC between the testing data and the trainign data. If the model is overfitting, yopu would expect higher AUCs for training and lower for testing. Similar AUCs indicate model robustness.

eval.plot(bis_model_pres@results, value = 'avg.diff.AUC' )


#' We can see here the importance of regularization to reduce overfitting. LQH, LH and H models seem to overfit more than others.

#' Now, we can compare the quality of the model fit using Akaike's Information Criteria. A lower AIC value implies in a model with a better fit (similar to a regression model having a higher R-squared). The algorithm calculates the difference in AIC between all models and the model with the lowest AIC, so we can quicly glance wich ones are best:
    
eval.plot(bis_model_pres@results, value = 'delta.AICc' )

#' L, Q and LQ seem to be best. Despite the good performance before, H only models have higher dAIC values. We can also rank them by value, when we then see that LQ and Q also appear to be good models.

bis_model_pres@results %>% arrange(delta.AICc) %>% dplyr::select(c(settings,delta.AICc))

#' Considering the above results, we may consider taking a look at the predictions from L_3, Q_3, LH_3 and LQ_3 

best_preds <- subset(bis_model_pres@predictions, c('L_3','LH_3','LQ_3','Q_3'))

crs(best_preds) <- wgs84

bestmod <- bis_model_pres@results[which(bis_model_pres@results$delta.AICc==0.4676720),]

varimp <- var.importance(bestmod)

barplot(varimp$percent.contribution, names.arg=varimp$variable, las=2, ylab="Percent Contribution")

# Export predictions

exp_fun <- function(x){
    fname = paste0(names(x), '.tif')
    writeRaster(x,filename=fname,datatype='FLT4S' , format='GTiff', overwrite=TRUE)
}

lapply(unstack(best_preds),exp_fun)

## Thresholding

paul_points <- read.csv("./occ_data/new_sites.csv")

head(paul_points)

sp_pp <- SpatialPointsDataFrame(as.matrix(paul_points[,c(8,9)]), paul_points, proj4string = wgs84)

mapview(sp_pp)

cutoffs <- apply(extract(best_preds,sp_pp, df=TRUE),2,min)

sp_occ <- readRDS('occ_data/uk_occ.rds')
under_points <- extract(best_preds$LQ_3, sp_occ, df=TRUE)

bin_L3 <- best_preds[[1]] >= cutoffs[2]
bin_LH3 <- best_preds[[2]] >= cutoffs[3]
bin_LQ3 <- best_preds[[3]] >= cutoffs[4]
bin_Q3 <- best_preds[[4]] >= cutoffs[5]

bin_all <- stack(bin_L3,bin_LH3,bin_LQ3,bin_Q3)

mapview(bin_all) + mapview(sp_pp) + mapview(uk_occ)

