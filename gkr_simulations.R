
library(raster)
library(rgdal)
library(sp)
library(reshape)
library(ggplot2)

## first, download from Google Drive: 'sdm', 'sdm_outline_shp', 'gkr_percentile_areas_001.csv'
## (and if skipping step 1, download & unzip 'schemes.zip')


## 1. generate sampling schemes (skip if using existing shapefiles)

    range <- readOGR('inputs_ignore', layer = 'sdm_outline_shp')  
    # import shapefile outline of SDM extent 
    # (ideally, make this reproducible in R, but 'rasterToPolygon' takes forever)
    
    nlocs <- seq(50, 300, 10)  # number of trapping locations
    nrandom <- 50 # number of random grids
    
    schemes <- list()
  
      for (i in nlocs){
        grid.i <- spsample(range, n = i, type = 'regular')
        schemes[[paste('grid_', i, sep = '')]] <- grid.i
        
          rand.schemes <- list()
          
          for (h in 1:nrandom){
            random.h <- spsample(range, n = i, type = 'random')
           # rand.schemes[[paste('random_', i, '_', h, sep = '')]] <- random.h
            schemes[[paste('random', i, '_', h, sep = '')]] <- random.h
          }
        
        #strat.i <- 
        
        #schemes[[paste('random_', i, sep = '')]] <- rand.schemes # add a list to a list?
      }

    ## save (optional):
    ## ** this takes a long time. just do it when we have a final set of sampling schemes. **
      
     #   lapply(1:length(schemes), function(x){
     #     spdf <- SpatialPointsDataFrame(coords = schemes[[x]]@coords,
     #                                    data = as.data.frame(schemes[[x]]))
     #     writeOGR(spdf, dsn = 'inputs_ignore/schemes', layer = paste(names(schemes)[x]),
     #              driver = 'ESRI Shapefile')
     #   })
  
        
## 2. load sampling scheme shapefiles (skip if step 1 was run)
        
    schemes.files <- list.files('inputs_ignore/schemes', pattern ='\\.shp$',
                                full.names = TRUE)
    schemes <- lapply(schemes.files, shapefile)
    schemes.names <- sub('.shp.*', '', list.files('inputs_ignore/schemes', pattern ='\\.shp$'))
    names(schemes) <- schemes.names
    
    ## see an example:
    
      plot(range)
      plot(schemes$grid_50, add = TRUE)
      plot(schemes$random50_1, add = TRUE, col = 'red')
      plot(schemes$random50_25, add = TRUE, col = 'green')


## 3. define function 'range simulation':
      
    # DESCRIPTION OF INPUTS
      # sdm: raster of habitat suitablility for GKR 
      # p1: proportion of range occupied in year 1
      # change: range expansion/contraction
      # area.suit: table of habitat suitability thresholds corresponding w/ area occupied
      # schemes: list of previously generated sampling schemes
      # nlocs: number of trapping locations

rangesim <- function(sdm, p1, change, area.suit, schemes, nlocs){
  
  if ((p1 + change) <= 1) {  
    
    area1 <- 70828.54 * p1    #70828.54km2 is the entire area of the range (so this will be constant)
    thresh1 <- area.suit$suitability[which(abs(area.suit$km2 - area1) == min(abs(area.suit$km2 - area1)))]
    year1 <- sdm > thresh1
    
    area2 <- area1 * (1 + change)
    thresh2 <- area.suit$suitability[which(abs(area.suit$km2 - area2) == min(abs(area.suit$km2 - area2)))]
    year2 <- sdm > thresh2
    
  grid <- schemes[[paste('grid_', nlocs, sep = '')]]
    grid_pres1 <- sum(extract(year1, grid), na.rm = TRUE) / length(!is.na(extract(year1, grid)))
    grid_pres2 <- sum(extract(year2, grid), na.rm = TRUE) / length(!is.na(extract(year1, grid)))
  
  randoms <- list()
    for (h in 1:nrandom){
        r <- schemes[[paste('random', nlocs, '_', h, sep = '')]]
        randoms[[paste('random', nlocs, '_', h, sep = '')]] <- r
    }
  
    rand_pres1 <- sapply(1:length(randoms), simplify = TRUE, function(x){
                            sum(extract(year1, randoms[[x]]), na.rm = TRUE) / 
                            length(!is.na(extract(year1, randoms[[x]])))})
    rand_pres2 <- sapply(1:length(randoms), simplify = TRUE, function(x){
                            sum(extract(year2, randoms[[x]]), na.rm = TRUE) / 
                            length(!is.na(extract(year2, randoms[[x]])))})
    
    presence <- data.frame('scheme' = c('grid', rep('random', length(rand_pres1))), 
                            'pres1' = c(grid_pres1, rand_pres1),
                            'pres2' = c(grid_pres2, rand_pres2))
    
    presence$expected <- presence$pres1 * (1 + change)
    presence$ratio <- presence$pres2 / presence$expected  
    
  ratios <- presence[,c('scheme', 'ratio')]
    
  } else {
    
    ratios <- data.frame('scheme' = c('grid', 'random'), 'ratio' = c(NA, NA))
    
  }
  
  rangesim <- ratios
  return(rangesim)
}

### TEST:

rangesim(sdm, p1 = 0.9, change = -0.25, area.suit, schemes, nlocs = 100)



#########################

## 4. simulate!

    ## import sdm and area.suit table (any way to save these w the function?)
    
      sdm <- raster('inputs_ignore/sdm')
      area.suit <- read.csv('gkr_percentile_areas_001.csv')
    
    ## define parameters:
    
      p1 <- seq(0.1, 1, 0.1)           #proportion occupied in starting year
      changes <- seq(-0.9, 0.9, 0.1)   #range changes from 90% contraction to 90% expansion
      nlocs <- seq(50, 300, 10)        #number of trapping locations


    ## loop -- this will take a while (~ 30 min): 
      ## (alternatively, set individual p1 to do by hand)
      p1 <- 0.9
    
    sim_vals <- list()
    
    for (k in p1) {
      ratios_k <- NULL
      
      for (j in changes){
        
        for (i in nlocs){
          ratios <- rangesim(sdm, p1 = k, change = j, area.suit, schemes, nlocs = i)
          ratios$nlocs <- rep(i, nrow(ratios))
          ratios$change <- paste(j*100, '%', sep = '')
          ratios_k <- rbind(ratios_k, ratios)
        }
      }
      sim_vals[[paste(k*100, '% occupied', sep = '')]] <- ratios_k
    }  

    sim_vals


#########################
    
## 5. plot & export

    for (a in 1:length(sim_vals)){
      names(sim_vals)[a]
      sim_vals_melt <- sim_vals[[a]]
      sim_vals_melt$change <- factor(sim_vals_melt$change, 
                                     levels = unique(sim_vals_melt$change)) #reorder -90 to -10
      sim_vals_melt <- sim_vals_melt[!is.na(sim_vals_melt$ratio),] #remove NAs
      
      g <- ggplot(sim_vals_melt, aes(x = nlocs, y = ratio, col = scheme)) +
                  geom_point() +
                  facet_wrap('change', scales = 'free') + 
                  geom_hline(yintercept = 1, linetype = 'dashed', lwd = 0.8) +
                  coord_cartesian(ylim = c(0,2), xlim = c(50,300)) +
                  xlab('# trapping locations') + ylab('Estimated vs. expected trap detections') +
                  labs(title = paste('starting year = ', names(sim_vals)[a], sep = '')) +
                  theme(axis.text.x = element_text(size = 10, colour = 'black'),
                        axis.text.y = element_text(size = 10, colour = 'black'),
                        axis.title = element_text(size = 14, colour = 'black'),
                        axis.ticks = element_line(colour = 'black', size = 0.8),
                        axis.line.x = element_line(size = 0.5, colour = 'black'),
                        axis.line.y = element_line(size = 0.5, colour = 'black'),
                        strip.text = element_text(size= 14),
                        legend.text = element_text(size = 12), legend.title = element_text(size = 14),
                        plot.title = element_text(size = rel(1.5), face = 'bold', hjust = 0.5))
      
      tiff(paste('sim_figures/081518/starting_', sub('%.*', '', names(sim_vals)[a]), 
                 '.tif', sep = ''), width = 800, height = 500)
      plot(g)
      dev.off()
      
      write.csv(sim_vals_melt, paste('spreadsheets/081518/simvals_', names(sim_vals)[a], 
                                     '.csv', sep = ''))
    }


#################

## Figure thoughts:

## We really DON'T want scales = 'free' but it adds y-axes to all plots. 
## I think should be OK as long as we also have coord_cartesian().
## Actually, with this many panels, it might be OK to not have axes on them all. 
## Can also do scales = 'free_x' or 'free_y' for just axes in 1 direction

## Insead of having grid & random on the same plot, another option is to remove col = scheme from aes,
## and instead use facet_wrap(change ~ scheme, scales = 'free'); see below



################# ALTERNATIVE PLOT ####################

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

