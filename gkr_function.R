
library(raster)

## load SDM raster

  sdm <- raster('C:/Users/Cara/Documents/__GKR/shapefiles/sdm')
  plot(sdm)

  
## calculate area occupied in yr 1

  thresh.y1 <- 0.1 #threshold for a "good year" (OR SHOULD THIS BE PERCENTILE INSTEAD OF HAB SUIT VALUE?)
  
  y1 <- sdm > thresh.y1
  area.y1 <- tapply(area(y1), y1[], sum) ## doesn't work for UTM; fix this
    

## simulate range contraction (yr 2)
  
  change <- 0.1 #eventually generate a bunch of these
  thresh.y2 <- thresh.y1 + change #modify if this should be percentile instead of hab suit value
  
  y2 <- sdm > thresh.y2
  area.y2 <- tapply(area(y2), y2[], sum) ## fix for UTM (or just project sdm)
  

## generate sampling scheme  

  nlocs = 100 ## eventually generate a bunch of these
  
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