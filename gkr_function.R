
library(raster)
library(rgdal)
library(rgeos) #?
library(maptools)
library(ggplot2)
library(reshape)

## load SDM raster

  sdm <- raster('C:/Users/Cara/Documents/__GKR/shapefiles/sdm')
  plot(sdm)

  ## not reproducible bc I did this in ArcMap but this seems complicated in R  
    range <- readOGR('C:/Users/Cara/Documents/__GKR/shapefiles', layer = 'sdm_outline_shp')
    plot(range, add = TRUE)

## load table of habitat suitability values at different percentiles & corresponding areas occupied
  
  area.suit <- read.csv('gkr_percentile_areas.csv')
  area.suit <- area.suit[order(area.suit$km2),]   #need to sort ascending by area
  head(area.suit)

## define variables
  
  y1.thresh <- 0.1  #threshold for a "good year" (will set this in the function, or use prev yr) **should be a value from the suit table, like at 10th percentile, not 0.1
  change <- 0.1     #eventually generate a bunch of these: seq(-0.95, 0.95, 0.1)
  nlocs = 100       #eventually generate a bunch of these: 
    # Q: what's a reasonable range of sites to sample in the field?

## calculate area occupied in yr 1 (if 'gArea(rasterToPolygons())' is slow, 'tapply(area(y1), y1[], sum)' does
  ## work although it gives an error about lat/lon)
  
  y1 <- sdm > y1.thresh
  y1.area <- gArea(rasterToPolygons(y1, fun = function(x){x == 1})) / 1e06

## calculate area occupied in yr 2
  
  y2.area <- y1.area * (1 - change)
  
## find corresponding habitat suitability threshold matching the area occupied in yr 2
  
  index <- findInterval(y2.area, area.suit$km2) #returns the index of the matching row
  y2.thresh <- area.suit$suitability[index]
  
## create y2 raster
  
  y2 <- sdm > y2.thresh
  
## plot change
  
  par(mfrow = c(1,2))
  plot(y1, main = paste('y1', ' (area = ', round(y1.area, 0), ' km2)', sep = ''))
  plot(y2, main = paste('y2', ' (area = ', round(y2.area, 0), ' km2)', sep = ''))  
  title(sub = paste('change = ', change))
  
  

## generate sampling scheme  
  
  grid <- spsample(range, n = nlocs, type = 'regular')
  random <- spsample(range, n = nlocs, type = 'random') ## eventually generate a bunch of these
  #strat <- 
  
## sample yr 1 with grid & random schemes
  
  grid$pres1 <- extract(y1, grid)
  pres1.grid <- sum(grid$pres1, na.rm = TRUE) / length(!is.na(grid$pres1))
  
  random$pres1 <- extract(y1, random)
  pres1.rand <- sum(random$pres1, na.rm = TRUE) / length(random$pres1)
  
  ## so 10% - 15% of traps detected gkr presence in yr 1
  plot(y1, main = 'y1')
  plot(grid[grid$pres1 == 1,], add = TRUE) ## out of 100 traps
  plot(random[!is.na(random$pres1) & random$pres1 == 1,], add = TRUE, col = 'red')
  
  
## sample yr 2 with grid & random schemes
  
  grid$pres2 <- extract(y2, grid)
  pres2.grid <- sum(grid$pres2, na.rm = TRUE) / length(grid$pres2)  
  
  random$pres2 <- extract(y2, random)
  pres2.rand <- sum(random$pres2, na.rm = TRUE) / length(random$pres2)
  
  ## 9% - 13% of traps detected gkr presence
  plot(y2, main = 'y2')
  plot(grid[grid$pres2 == 1,], add = TRUE)
  plot(random[!is.na(random$pres2) & random$pres2 == 1,], add = TRUE, col = 'red')
    
## what's the difference?
  
  change ## this will be the title (e.g., 10% decrease)
  
  ## grid scheme
    pres1.grid #in yr1, we detected gkr at 15% of traps
    pres2.grid #in yr2, we detected gkr at 13% of traps
  
    actual.grid <- pres1.grid * (1 - change) # with a -10% change, we should have detected gkr at 14% of traps
  
    diff.grid <- pres2.grid / actual.grid # this is the value that will be plotted
  
  ## random scheme
    pres1.rand #yr1: 10%
    pres2.rand #yr2: 9% (this went up but I assume it will fluctuate with diff random pts)
    
    actual.rand <- pres1.rand * (1 - change) ## should have detected gkr at 9%
    
    diff.rand <- pres2.rand / actual.rand
  
  ## plot
  
    yvals <- c(diff.grid, diff.rand)
    plotvals <- data.frame('x' = rep(nlocs, length(yvals)), 'y' = yvals)
  
    par(mfrow = c(1,1))  
    plot(plotvals, xlab = '# trap locations', ylab = 'ratio gkr detections / actual change',
         main = '10% range contraction')
    
  ## basic idea. this has only 1 iteration of trap locations, and 1 randomly generated 
  ## sampling scheme (plus the grid scheme) ** need diff colors/symbols for the diff schemes
  
  
########### 
## now turn it into a function!
  
    ## do first, outside function: generate sampling scheme  
    
      grid <- spsample(range, n = nlocs, type = 'regular')
      random <- spsample(range, n = nlocs, type = 'random') ## eventually generate a bunch of these
      #strat <- 
    
    ## define parameters:
      change <- 0.5
      thresh.y1 ## input thresh.y1 or area.y1?
      nlocs
    
    ## function 'range simulation':
      
    rangesim <- function(sdm, change, thresh.y1, area.suit, range, nlocs){
      
      y1 <- sdm > y1.thresh
      y1.area <- gArea(rasterToPolygons(y1, fun = function(x){x == 1})) / 1e06
      
      y2.area <- y1.area * (1 - change)
      index <- findInterval(y2.area, area.suit$km2) #returns the index of the matching row
      y2.thresh <- area.suit$suitability[index]
      
      y2 <- sdm > y2.thresh
      
      grid <- spsample(range, n = nlocs, type = 'regular')
      random <- spsample(range, n = nlocs, type = 'random')
      #strat <- 
      
      grid$pres1 <- extract(y1, grid)
      pres1.grid <- sum(grid$pres1, na.rm = TRUE) / length(!is.na(grid$pres1))
      
      random$pres1 <- extract(y1, random)
      pres1.rand <- sum(random$pres1, na.rm = TRUE) / length(random$pres1)
      
      grid$pres2 <- extract(y2, grid)
      pres2.grid <- sum(grid$pres2, na.rm = TRUE) / length(grid$pres2)  
      
      random$pres2 <- extract(y2, random)
      pres2.rand <- sum(random$pres2, na.rm = TRUE) / length(random$pres2)
      
      actual.grid <- pres1.grid * (1 - change)
      diff.grid <- pres2.grid / actual.grid
      
      actual.rand <- pres1.rand * (1 - change)
      diff.rand <- pres2.rand / actual.rand
      
      yvals <- data.frame('grid' = diff.grid, 'random' = diff.rand)
      
      rangesim <- yvals
      return(rangesim)
    }
    

### TEST!
    
    test1 <- rangesim(sdm = sdm, change = 0.5, thresh.y1 = 0.1, area.suit = area.suit, 
                      range = range, nlocs = 200)
    
###
    
### NOW LOOP THRU 
    
## set these first (we'll do 1 loop per 'change' value, then create another loop to run thru change values)
  
  range_changes <- c(0.1, 0.5)
  thresh.y1 <- 0.1
  nlocs <- seq(50, 300, 10) ## e.g.
  
  sim_vals <- list()
  
  for (j in range_changes){
    yvals_j <- NULL
    change <- j
      for (i in nlocs){
          yvals <- rangesim(sdm, change, thresh.y1, area.suit, range, i)
          yvals_df <- data.frame('nlocs' = i, yvals) #add 's' for stratified
          yvals_j <- rbind(yvals_j, yvals_df)
      }
    sim_vals[[j*100]] <- yvals_j
  }

sim_vals[[10]]

  ## plot: 

  par(mfrow = c(2,2)) 
  
  for (j in range_changes){
    df <- sim_vals[[j*100]]  
  
    for (k in 2:3){
      df.k <- df[,c(1,k)]
      
      plot(df.k, xlab = '# trap locations', ylab = 'GKR detections / range change',
         main = paste(j*100, '% change (', names(df)[k], ')', sep = ''))
      abline(h = 1, lty = 2)
    
    }
  } 
  
  ## export 650 x 800
  
  ## try as a ggplot panel:
  
  sim_vals_10 <- melt(sim_vals[[10]], id = c('nlocs'))
  colnames(sim_vals_10) <- c('nlocs', 'scheme', 'ratio')
  
  ggplot(sim_vals_10, aes(x = nlocs, y = ratio)) +
    geom_point() +
    facet_wrap('scheme') +
    geom_hline(yintercept = 1, linetype = 'dashed', lwd = 0.8) +
    coord_cartesian(ylim = c(0,2)) +
    xlab('# trapping locations') + ylab('GKR detections / range change') +
    theme(axis.text.x = element_text(size = 14, colour = 'black'),
          axis.text.y = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 14, colour = 'black'),
          axis.ticks = element_line(colour = 'black', size = 0.8),
          axis.line.x = element_line(size = 0.5, colour = 'black'),
          axis.line.y = element_line(size = 0.5, colour = 'black'),
          strip.text = element_text(size= 16))
  
  
  
    ylab(expression(paste('Selection Ratio (', italic('w'[i]), ')')))

  
  
    
#####################
  ## starting year?
  
  par(mfrow = c(1,2))
  
  range.area <- gArea(range) / 1e06 ## 
  
  ## 90% occupied
  
    y1.area.90pct <- range.area * 0.9 ## assume 90% of range AREA is occupied

      index <- findInterval(y1.area.90pct, area.suit$km2) #returns the index of the matching row
      y1.90.thresh <- area.suit$suitability[index]
      
      y1.90.sdm <- sdm > y1.90.thresh  
      plot(y1.90.sdm, main = '90% occupied (by area)')
      
    
    y1.90.thresh2 <- area.suit$suitability[90] ## assume everything over 10th pctle is occupied 
    
      y1.area.90pct2 <- area.suit$km2[90] ## this is the corresponding area (637745.69 vs 62751.83)
      y1.90.sdm2 <- sdm > y1.90.thresh2  
      plot(y1.90.sdm2, main = '90% occupied (by percentile)')

    
    
  ## 10% occupied  
      
    y1.area.10pct <- range.area * 0.1
  
      index <- findInterval(y1.area.10pct, area.suit$km2)
      y1.10.thresh <- area.suit$suitability[index]

      y1.10.sdm <- sdm > y1.10.thresh
      plot(y1.10.sdm, main = '10% occupied')    
      
  
      par(mfrow = c(1,2))
  
      
      pctle10 <- area.suit$suitability[10]
      testsdm <- sdm > pctle10
      plot(testsdm)    
          
      
  ## unnecessary after 8/8/18
  
####################################################################
####################################################################
  

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
        threshold <- percentiles[i] # hab suit value at the ith percentile
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