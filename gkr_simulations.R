
library(raster)
library(sp)
library(reshape)
library(ggplot2)


## define function 'range simulation':
    
  rangesim <- function(sdm, p1, change, area.suit, range, nlocs){
    
    if ((p1 + change) <= 1) {  ##either use this test here or test denominator between 0 and 1 below
    
      area1 <- 70828.54 * p1    #70828.54km2 is the entire area of the range (so this will be constant)
        thresh1 <- area.suit$suitability[findInterval(area1, area.suit$km2)]
        year1 <- sdm > thresh1
      
      area2 <- area1 * (1 + change)
        thresh2 <- area.suit$suitability[findInterval(area2, area.suit$km2)]
        year2 <- sdm > thresh2
      
    ## could also test if area2 > 70828.54 here and stop if so (i.e., it expanded beyond the original range area)
        
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
      
      expected <- presence1 * (1 + change)
      ratios <- data.frame('grid' = NA, 'random' = NA) ##if testing p1 + change <= 1 above, remove this 'ifelse' loop
      
        for (e in 1:length(expected)) {
              ratios.e <- ifelse(expected[e] >= 0 && expected[e] <= 1, presence2[e]/expected[e], NA)
              ratios[e] <- ratios.e
        }

    } else {
      
      ratios <- data.frame('grid' = NA, 'random' = NA)
        
    }
    
    rangesim <- ratios
    return(rangesim)
  }
    
    
  
#########################
    
  ## LOOP
    
    ## FIRST: import sdm, range boundary, and area.suit table *any way to save these w the function?
    
      sdm <- raster('') #import SDM raster
      range <- readOGR('') #import shapefile outline of sdm extent (ideally, make this more reproducible)
      area.suit <- read.csv('gkr_percentile_areas.csv')
        area.suit <- area.suit[order(area.suit$km2),]
  
    ## define parameters:

      p1 <- 0.2                        #proportion occupied in starting year
      changes <- seq(-0.9, 0.9, 0.1)   #range changes from 90% contraction to 90% expansion
      nlocs <- seq(50, 300, 10)        #number of trapping locations
      
      sim_vals <- list()

    for (j in changes){
      change <- j
      ratios_j <- NULL
      
      for (i in nlocs){
        ratios <- rangesim(sdm, p1, j, area.suit, range, i)
        ratios_df <- data.frame('nlocs' = i, ratios)
        ratios_j <- rbind(ratios_j, ratios_df)
      }
      
      name_j <- paste(j*100)
      sim_vals[[name_j]] <- ratios_j
    }
    
      
sim_vals  ## run time ~15 min


##### PLOT #####

    sim_vals_melt <- NULL    #first, need to put all data in a dataframe (instead of list of dataframes)
    
      for (c in 1:length(sim_vals)){
        sim_vals_c <- melt(sim_vals[[c]], id = c('nlocs'))
        colnames(sim_vals_c) <- c('nlocs', 'scheme', 'ratio')
        sim_vals_c$change <- paste(names(sim_vals)[c], '%', sep = '')
        sim_vals_melt <- rbind(sim_vals_melt, sim_vals_c)
      }

      #assign factor levels in order from -90 to 90
        sim_vals_melt$change <- factor(sim_vals_melt$change, levels = unique(sim_vals_melt$change)) 
          
      #remove NA rows (e.g. where 'expected' > 1)
        sim_vals_melt <- sim_vals_melt[!is.na(sim_vals_melt$ratio),]
          
      ## then plot:
    
        ## (if from saved dataframe): 
            #p1 <- 0.2
            #sim_vals_melt <- sim_vals_20
        
      ggplot(sim_vals_melt, aes(x = nlocs, y = ratio, col = scheme)) +
            geom_point() +
            facet_wrap('change', scales = 'free') + 
            geom_hline(yintercept = 1, linetype = 'dashed', lwd = 0.8) +
            coord_cartesian(ylim = c(0,2), xlim = c(50,300)) +
            xlab('# trapping locations') + ylab('Estimated vs. expected trap detections') +
            labs(title = paste('starting year = ', p1*100, '% occupied', sep = '')) +
            theme(axis.text.x = element_text(size = 10, colour = 'black'),
                  axis.text.y = element_text(size = 10, colour = 'black'),
                  axis.title = element_text(size = 14, colour = 'black'),
                  axis.ticks = element_line(colour = 'black', size = 0.8),
                  axis.line.x = element_line(size = 0.5, colour = 'black'),
                  axis.line.y = element_line(size = 0.5, colour = 'black'),
                  strip.text = element_text(size= 14),
                  legend.text = element_text(size = 12), legend.title = element_text(size = 14),
                  plot.title = element_text(size = rel(1.5), face = 'bold', hjust = 0.5))
     
      #export 800 x 500
        
      ## After making figure for each starting year scenario, save dataframe so we can reproduce figure
          sim_vals_20 <- sim_vals_melt
          write.csv(sim_vals_20, 'spreadsheets/sim_vals_20_081018.csv') 
        
        
  ## Figure thoughts:
        
    ## We really DON'T want scales = 'free' but it adds y-axes to all plots. 
    ## I think should be OK as long as we also have coord_cartesian().
    ## Actually, with this many panels, it might be OK to not have axes on them all. 
    ## Can also do scales = 'free_x' or 'free_y' for just axes in 1 direction
    
    ## Insead of having grid & random on the same plot, another option is to remove col = scheme from aes,
    ## and instead use facet_wrap(change ~ scheme, scales = 'free'); see below
        
        
   
################# OLD CODE ####################
        
  ## plots for for just 1 change value at a time (to compare grid/random side by side):
    
    sim_vals_c <- melt(sim_vals[[1]], id = c('nlocs')) #replace 1 with c if we're looping; see below
    colnames(sim_vals_c) <- c('nlocs', 'scheme', 'ratio')
    
    ggplot(sim_vals_c, aes(x = nlocs, y = ratio)) +
      geom_point() +
      facet_wrap('scheme', scales = 'free') + #we really DON'T want scales='free' but it adds y-axes to all plots. should be OK if we have coord_cartesian()
      geom_hline(yintercept = 1, linetype = 'dashed', lwd = 0.8) +
      coord_cartesian(ylim = c(0,2)) +
      xlab('# trapping locations') + ylab('GKR detections / range change') +
      labs(title = paste(changes[c], '% change', sep = '')) +
      theme(axis.text.x = element_text(size = 14, colour = 'black'),
            axis.text.y = element_text(size = 14, colour = 'black'),
            axis.title = element_text(size = 14, colour = 'black'),
            axis.ticks = element_line(colour = 'black', size = 0.8),
            axis.line.x = element_line(size = 0.5, colour = 'black'),
            axis.line.y = element_line(size = 0.5, colour = 'black'),
            strip.text = element_text(size= 14),
            plot.title = element_text(size = rel(1.5), face = 'bold', hjust = 0.5))
    
    
