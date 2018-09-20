
library(raster)
library(rgdal)

## load SDM raster & convert to SPDF (which is faster & necessary for spsample function)
    sdm <- raster('inputs_ignore/sdm')
    sdm.spdf <- as(sdm, 'SpatialPixelsDataFrame')
  
## calculate percentiles of habitat suitability
    sdm.pctle <- quantile(sdm.spdf@data$sdm, probs = seq(0.001, 1, 0.001))

## remove lowest 5% of habiat suitability
    sdm.spdf <- sdm.spdf[sdm.spdf@data$sdm > sdm.pctle[50],]

## define function
  
  # thresh: breakpoint btwn strata, e.g., 0.90 = 90th percentile of habitat suitability values
  # n.upper: number of sampling locations in upper strata
  # n.lower: number of sampling locations in lower strata
  
  strat.sample <- function(sdm.spdf, sdm.pctle, thresh, n.upper, n.lower){
  
    # create SPDFs of upper/lower strata based on 'thresh' value (these appear to overlap; why?)
      strat.upper <- sdm.spdf[sdm.spdf@data$sdm >= sdm.pctle[thresh*1000],]
      strat.lower <- sdm.spdf[sdm.spdf@data$sdm < sdm.pctle[thresh*1000],]
    
    # sample within upper/lower strata based on n.upper/n.lower
      sample.upper <- spsample(strat.upper, n = n.upper, type = 'regular')
      sample.lower <- spsample(strat.lower, n = n.lower, type = 'regular')
      
    ## combine (should we add attributes to identify upper/lower?)
      strat.both <- rbind(sample.upper, sample.lower)  
    
    strat.sample <- strat.both
    return(strat.sample)
  }
  
  
  # example:
      a <- Sys.time()
      test <- strat.sample(sdm.spdf, sdm.pctle, thresh = 0.50, n.upper = 100, n.lower = 100)
      b <- Sys.time()
      b-a # < 1 second (~5 seconds when including raster->SPDF and percentiles steps in function)
      
        plot(sdm)
        plot(test, add = TRUE)


## loop through to generate sampling schemes:  
        
      thresh <- seq(0.10, 0.90, 0.10) # for example. (is it realistic to use anything < 0.50?)
      n.upper <- seq(50, 300, 10)      
      n.lower <- seq(50, 300, 10) # or, n could be the same, or one a function of the other, etc.
      
      strat.schemes <- list()
      
    for (a in thresh){
      for (b in n.upper){
        for (c in n.lower){
            strat.c <- strat.sample(sdm, thresh = a, n.upper = b, n.lower = c)
            strat.schemes[paste('stratified_', a*100, 'pctle_', b, '-',  c, sep = '')] <- strat.c
        }
      }
    }
      
      
      

########## OLD CODE ##########
    
  ## original function (much slower: ~26 sec)  
      
    strat.sample.old <- function(sdm, thresh, n.upper, n.lower){
      
      ## calculate percentiles of habitat suitability
        percentiles <- quantile(sdm, probs = seq(0.001, 1, 0.001)) 
        #percentiles[250] # indexing e.g.
        
      ## make raster based on 'thresh' value, then convert to polygons
        strat1 <- sdm >= percentiles[thresh*1000]
        strat1_spdf <- as(strat1, 'SpatialPolygonsDataFrame')
        
      ## create outlines of upper + lower strata
        strat1_poly <- strat1_spdf[strat1_spdf@data$layer == 1,]
        strat2_poly <- strat1_spdf[strat1_spdf@data$layer == 0,]
        
      ## generate sampling scheme within each area (upper/lower)
        sample.upper <- spsample(strat1_poly, n = n.upper, type = 'regular')
        sample.lower <- spsample(strat2_poly, n = n.lower, type = 'regular')
      
      ## combine  
        strat.both <- rbind(sample.upper, sample.lower) 
      
      strat.sample.old <- strat.both
      return(strat.sample.old)
    }
      
