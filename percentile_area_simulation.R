## simulate area occupied (km2) based on a range of habitat suitability percentiles from gkr species disribution model

## update 8/14/18: replaced 'gArea(rasterToPolygons)' with MUCH faster 'cellStats * res'
##    (old code: area.i <- gArea(rasterToPolygons(raster.i, fun = function(x){x == 1})) / 1e06)

#install.packages('rgeos') #install if on vlab computer
#install.packages('raster')
#install.packages('rgdal')
library(raster)
library(rgeos)
library(rgdal)

## load SDM raster:

sdm <- raster('C:/Users/cla236/Documents/sdm') ## download from email; create folder 'sdm'
plot(sdm)

## calculate raster values (suitability) at each percentile from 0.1% to 100%:

percentiles <- quantile(sdm, probs = seq(0.001, 1, 0.001))
percentiles[379] # indexing e.g.

## set up dataframe and plot area:

area.pct <- NULL
par(mfrow = c(3,2))

for (i in 1:999){
  threshold <- percentiles[i] # hab suit value at the ith percentile
  raster.i <- sdm > threshold
  area.i <- (cellStats(raster.i, 'sum') * res(raster.i)[1]*res(raster.i)[2]) / 1e06
  df <- data.frame('percentile' = names(threshold), 'suitability' = threshold[[1]], 
                   'km2' = area.i)
  area.pct <- rbind(area.pct, df)
  
  ## visualize some of the range contractions/expansions:
  
  if(i %in% c(100, 250, 500, 750, 900, 990)){
    plot(raster.i, main = paste('> ', i/10, '%', ' (area = ', round(area.i, 0), ' km2)', 
                                sep = ''))
  }
}

area.pct

write.csv(area.pct, 'gkr_percentile_areas_001.csv')


########################################
#### example of a 25% range contraction:

area.pct <- read.csv('gkr_percentile_areas.csv')
head(area.pct)

## year1: e.g., everything over 1st percentile is occupied (70,127 km2)
y1.area <- area.pct$km2[1] 
y1.suit <- area.pct$suitability[1]

## 25% range contraction
change <- 0.25

## year2: now only 52,595 km2 is occupied
y2.area <- y1.area * (1 - change) 

## what's the closest habitat suitability threshold matching that area occupied?

area.pct.sort <- area.pct[order(area.pct$km2),] #sort ascending for 'findInterval' to work
index <- findInterval(y2.area, area.pct.sort$km2) #returns the index of the matching row
area.pct.sort[index,]
y2.suit <- area.pct.sort$suitability[index]

## plot:

par(mfrow = c(1,2))
plot(sdm > y1.suit, main = 'year1 occupied')
plot(sdm > y2.suit, main = 'year2 occupied')
