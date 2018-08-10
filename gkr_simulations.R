########### 
library(raster)

    
    ## define function 'range simulation':
    
      rangesim <- function(sdm, p1, change, area.suit, range, nlocs){
        
        area1 <- 70828.54 * p1 #70828.54km2 is the entire area of the range (this will be constant)
          thresh1 <- area.suit$suitability[findInterval(area1, area.suit$km2)]
          year1 <- sdm > thresh1
        
        area2 <- area1 * (1 + change)
          thresh2 <- area.suit$suitability[findInterval(area2, area.suit$km2)]
          year2 <- sdm > thresh2
        
        grid <- spsample(range, n = nlocs, type = 'regular')
        random <- spsample(range, n = nlocs, type = 'random')
        #strat <- 
        
        grid$pres1 <- extract(year1, grid)
        random$pres1 <- extract(year1, random)
        presence1 <- data.frame('grid' = sum(grid$pres1, na.rm = TRUE) / length(!is.na(grid$pres1)),
                                'random' = sum(random$pres1, na.rm = TRUE) / length(random$pres1))
        
        grid$pres2 <- extract(year2, grid)
        random$pres2 <- extract(year2, random)
        presence2 <- data.frame('grid' = sum(grid$pres2, na.rm = TRUE) / length(grid$pres2),
                                'random' = sum(random$pres2, na.rm = TRUE) / length(random$pres2))
        
        ideal <- presence1 * (1 + change)
        ratios <- presence2 / ideal

        rangesim <- ratios
        return(rangesim)
      }
    
    
    ### TEST!
    
    test.df <- NULL
    test1 <- rangesim(sdm = sdm, p1 = 0.5, change = 0.1, area.suit = area.suit, range = range, nlocs = 300)
    test.df <- rbind(test.df, data.frame(nlocs = '300', test1))
    
#########################
    
  ## LOOP:
    
    #import sdm, range, and area.suit
    
    ## define parameters:
    
   # p1 <- seq(0.1, 0.9, 0.1)         #proportion occupied in year 1
    p1 <- 0.9
    change <- seq(-0.9, 0.9, 0.1)    #range changes from 90% contraction to 90% expansion
    nlocs <- seq(50, 300, 10)        #number of trapping locations
    
    sim_vals <- list()

    for (j in change){
      yvals_j <- NULL
      change <- j
      for (i in nlocs){
        ratios <- rangesim(sdm, p1, j, area.suit, range, i)
        ratios_df <- data.frame('nlocs' = i, ratios)
        ratios_j <- rbind(yvals_j, ratios_df)
      }
      name_j <- paste(j*100)
      sim_vals[[name_j]] <- ratios_j
    }
    
    
    
sim_vals


    
    

