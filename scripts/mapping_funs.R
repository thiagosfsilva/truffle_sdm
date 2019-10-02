### Plotting functions

library(tmap)
library(tmaptools)
data(Word)
tmap_mode('plot')

my_points <- function(pointdata, pngname){
    bbox <- bb(pointdata)
    tm <- tm_shape(World, projection = "latlong", bbox=bbox) +
    tm_fill(col='#FFCC66') +
    tm_borders() +    
    tm_layout(bg.color = "skyblue", inner.margins = c(0, .02, .02, .02)) +
    tm_shape(pointdata) +
    tm_dots(size=0.25) +
    tm_graticules(projection = "longlat",ticks = TRUE, lines =FALSE) +
    tm_scale_bar(position=c("left", "bottom")) +
    tm_compass(position=c("right", "top"))
    tm
   # tmap_save(tm, filename = pngame)
}
