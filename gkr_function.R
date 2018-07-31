
library(raster)
library(rgeos)

## load SDM raster

  sdm_original <- raster('C:/Users/Cara/Documents/__GKR/shapefiles/sdm')
  sdm <- projectRaster()
  plot(sdm)

## define variables
  
  thresh.y1 <- 0.1  #threshold for a "good year"
  change <- 0.1     #eventually generate a bunch of these
  nlocs = 100       #eventually generate a bunch of these


## calculate area occupied in yr 1
  
  y1 <- sdm > thresh.y1
  area.y1 <- tapply(area(y1), y1[], sum) ## doesn't work for UTM; fix this
  
  sdm.ply <- rasterToPolygons(sdm)
  gArea(sdm.ply[1,])


## QUANTILE PRACTICE
  
t <- gArea(rasterToPolygons(sdm, fun = function(x){x > 0})) / 1000000 
#total area of range (70,835.7 km2) (slightly different from ArcMap, 70,828.5 km2)
  
q <- quantile(sdm, probs = seq(0.1, 1, 0.1))

suitable <- 0.1 #percentile occupied in a "good" year, yr1 (e.g., everything > 10%)

q.i <- q[suitable*10] #habitat suitability threshold corresponding with the desired percentile

#yr1 raster
yr1 <- sdm > q.i
plot(yr1, main = 'yr1 occupied')

yr1.area <- gArea(rasterToPolygons(yr1, fun = function(x){x == 1})) / 1e06
#area occupied in year 1 = 63,751.83 km2 (slightly diff from ArcMap, 63,870.5 km2)
#divide by 1e06 to convert m2 to km2
#rasterToPolygons is slow. alternatives? (can project to lat/lon and use raster::area?)

change #percent decrease between yr1 and yr2

yr2.area <- yr1.area * (1 - change)
#area occupied in yr2 after expansion/contraction (57,376.65 km2)

#what percentile of raster values (hab. suitability) corresponds w this new area?

########################################

## simulate a bunch of percentiles and corresponding areas (this will take a while...)

  # calculate raster values (hab suit) at each percentile from 1% to 100%
      percentiles <- quantile(sdm, probs = seq(0.01, 1, 0.01))
      percentiles[37] # indexing e.g.
      
      area.pct <- NULL
      par(mfrow = c(3,2))
      
      for (i in 1:99){
        threshold <- qs[i] # hab suit value at the ith percentile
        raster.i <- sdm > threshold
        area.i <- gArea(rasterToPolygons(raster.i, fun = function(x){x == 1})) / 1e06
        df <- data.frame('percentile' = names(threshold), 'suitability' = threshold[[1]], 
                         'km2' = area.i)
        area.pct <- rbind(area.pct, df)
        
         ## visualize some of the range contractions/expansions
          for(i in c(10, 25, 50, 75, 90, 99)){
            plot(raster.i, main = paste('> ', i, '%', ' (area = ', round(area.i, 0), ' km2)', 
                                        sep = ''))
          }
      }
      

      
      rasters <- list()
      par(mfrow = c(3,2))
      
      for (i in c(10, 25, 50, 75, 90, 99)){
        threshold <- qs[i]
        raster.i <- sdm > threshold
        rasters[[i]] <- raster.i
        plot(raster.i, main = paste('> ', i, '%', ' (area = ', round(area.i, 0), ' km2)', sep = ''))
      }
      














#########################################

## simulate range contraction (yr 2)
  
  
  thresh.y2 <- thresh.y1 + change #modify if this should be percentile instead of hab suit value
  
  y2 <- sdm > thresh.y2
  area.y2 <- tapply(area(y2), y2[], sum) ## fix for UTM (or just project sdm)
  

## generate sampling scheme  

  
  
  grid <- spsample(range, n = nlocs, type = 'regular')
  random <- spsample(range, n = nlocs, type = 'random') ## eventually generate a bunch of these


## sample yr 1 with grid & random schemes
  
  grid$pres1 <- extract(y1, grid)
  pres1.grid <- sum(grid$pres1, na.rm = TRUE) / length(!is.na(grid$pres1))
  
  random$pres1 <- extract(y1, random)
  pres1.rand <- sum(random$pres1, na.rm = TRUE) / length(random$pres1)
  
  ## so 10% - 11% of traps detected gkr presence even in a good year
    plot(y1)
    plot(grid[grid$pres1 == 1,], add = TRUE) ## out of 100 traps
    plot(random[random$pres1 == 1,], add = TRUE, col = 'red')


## sample yr 2 with grid & random schemes
  
  grid$pres2 <- extract(y2, grid)
  pres2.grid <- sum(grid$pres2, na.rm = TRUE) / length(grid$pres2)  
  
  random$pres2 <- extract(y2, random)
  pres2.rand <- sum(random$pres2, na.rm = TRUE) / length(random$pres2)
  
  ## 9% - 10% of traps detected gkr presence


## what's the difference?
  
  change ## this will be the x-axis value (e.g., 10% decrease)

    ## grid scheme
      pres1.grid #in yr1, we detected gkr at 11% of traps
      pres2.grid #in yr2, we detected gkr at 9% of traps
      
      actual.grid <- pres1.grid * (1 - change) # with a -10% change, we should have detected gkr 
                                               # at 9.9% of traps
    
      diff.grid <- pres2.grid / actual.grid # this is the value that will be plotted
      
    ## random scheme
      pres1.rand #yr1: 10%
      pres2.rand #yr2: 10.5% (this went up but I assume it will fluctuate with diff random pts)
      
      actual.rand <- pres1.rand * (1 - change) ## should have detected gkr at 9%
    
      diff.rand <- pres2.rand / actual.rand
  
## plot
      
      yvals <- c(diff.grid, diff.rand)
      plotvals <- data.frame('x' = rep(nlocs, length(yvals)), 'y' = yvals)
      
      plot(plotvals, xlab = '# trap locations', ylab = 'ratio gkr detections / actual change',
           main = '10% range contraction')
      
      ## basic idea. this has only 1 iteration of trap locations, and 1 randomly generated 
      ## sampling scheme (plus the grid scheme)
      
        
###############  
### OLD CODE
 
  
  range <- readOGR('C:/Users/Cara/Documents/__GKR/shapefiles', layer = 'sdm_outline_shp')
  plot(range, add = TRUE)
  
  rs <- (sdm > 0)
  plot(rs)
  tapply(area(rs), rs[], sum)
  
  pol <- rasterToPolygons(rs, fun = function(x) {x > 0}, dissolve = TRUE)
  plot(pol, add = TRUE)
  

## simulate range expansion/contraction
  
  ## decrease 10% (actually simulate this lots of diff ways)
  
  pct <- quantile(sdm, probs = seq(0, 1, 0.1), names = FALSE)
  
  pct[10] ## represents 90th percentile
  
  dec10 <- sdm > pct[10]
  plot(dec10)    

  ## area of distribution:
    tapply(area(dec10), dec10[], sum) ## doesn't work with UTM; fix this

  ## area if everywhere (> X percentile) is occupied:
    tapply(area(sdm), sdm[], sum)
  
  ## area if 