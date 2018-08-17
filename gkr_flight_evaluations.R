
## GKR range expansions/contractions with Carrizo flight data

library(raster)
library(rgeos)
library(rgdal)
library(reshape2)
library(ggplot2)


## 1. load range shapefiles from Carrizo flights

  flights.files <- list.files('C:/Users/Cara/Documents/__GKR/shapefiles/gkr_czo_flights', 
                             pattern ='\\.shp$', full.names = TRUE)
  flights.shp <- lapply(flights.files, shapefile)
  flights.names <- sub('.shp.*', '', list.files('C:/Users/Cara/Documents/__GKR/shapefiles/gkr_czo_flights', 
                                              pattern ='\\.shp$'))
  names(flights.shp) <- flights.names
  
  ## project to match sdm raster, grids, etc. (& only keep ones clipped to the National Monument)
    flights <- list()
    for (i in c(2,4,6,8)){
        flights.i <- spTransform(flights.shp[[i]], crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        name.i <- names(flights.shp)[[i]]
        flights[[name.i]] <- flights.i
    }
  
  
## 2. generate new sampling schemes within Carrizo NM
  
  #load Carrizo NM outline
    carrizo <- readOGR('C:/Users/Cara/Documents/__GKR/shapefiles', 
                       layer = 'CarrizoNationalMonument')  
    carrizo <- spTransform(carrizo, crs(flights[[1]]))
    
      nlocs <- seq(50, 300, 10)  # number of trapping locations
      schemes.carrizo <- list()
    
      for (i in nlocs){
          grid.i <- spsample(carrizo, n = i, type = 'regular')
          random.i <- spsample(carrizo, n = i, type = 'random') # eventually generate a bunch of these
          #strat.i <-
        
          schemes.carrizo[[paste('grid_', i, sep = '')]] <- grid.i
          schemes.carrizo[[paste('random_', i, sep = '')]] <- random.i
      }
    
    ## save sampling schemes (optional):
    
      lapply(1:length(schemes.carrizo), function(x){
            spdf <- SpatialPointsDataFrame(coords = schemes.carrizo[[x]]@coords,
                                           data = as.data.frame(schemes.carrizo[[x]]))
            writeOGR(spdf, dsn = 'carrizo_flights/schemes', 
                     layer = paste(names(schemes)[x]), driver = 'ESRI Shapefile')
             })
      
    ## see an example:
      
        plot(carrizo)
        plot(flights$`2010_flight_clip`, add = TRUE, col = 'green')
        plot(schemes.carrizo$grid_100, add = TRUE)
        plot(schemes.carrizo$random_100, add = TRUE, col = 'red')    


## 2. define new function:
        
    rangesim_czo <- function(y, flights, schemes.carrizo, nlocs){ 
          ## define: year1, nlocs
          ## input: flights (list), schemes.carrizo (list)
  
      # year1
        range1 <- flights[[y]]
        area1 <- gArea(range1) / 1e06
  
      # year2      
        range2 <- flights[[y+1]]
        area2 <- gArea(range2)  / 1e06
    
          # visualize:
            plot(range1)  
            plot(range2, add = TRUE, bor = 'red')  
            
      # calculate change:
        change <- (area2 - area1) / area1
      
      # 'survey' for GKR:
        grid <- schemes.carrizo[[paste('grid_', nlocs, sep = '')]]
        random <- schemes.carrizo[[paste('random_', nlocs, sep = '')]]
        #strat <- 
        
        pres1.grid <- over(grid, range1)
        pres1.random <- over(random, range1)
        
          presence1 <- data.frame('grid' = (table(pres1.grid)['0'] / length(grid)),
                                'random' = (table(pres1.random)['0'] / length(random)))
        
        pres2.grid <- over(grid, range2)
        pres2.random <- over(random, range2)
        
          presence2 <- data.frame('grid' = (table(pres2.grid)['0'] / length(grid)),
                                'random' = (table(pres2.random)['0'] / length(random)))
        
     # compare estimated vs. expected:   
        expected <- presence1 * (1 + change)
        ratios <- presence2 / expected
        
      rangesim_czo <- data.frame(ratios, change)
      return(rangesim_czo)
    }

    
    ## test
  
      test <- rangesim_czo(y = 1, flights, schemes.carrizo, nlocs = 50)    
        ## starting year = 1 (list index for 'flights') & nlocs = 50
        ## try manually above & compare 'ratios'
    
      
## 3. loop:

      nlocs <- seq(50, 300, 10)
      sim_vals_czo <- list()
          
        for (j in 1:(length(flights)-1)){
          ratios.j <- NULL
          
          for (n in nlocs){
                ratios <- rangesim_czo(y = j, flights, schemes.carrizo, nlocs = n)
                ratios_df <- melt(data.frame('nlocs' = n, ratios), id = c('nlocs', 'change'))
                colnames(ratios_df) <- c('nlocs', 'change', 'scheme', 'ratio')
                ratios_df$change <- paste(round(ratios_df$change, 2)*100, '%', sep = '')
                ratios.j <- rbind(ratios.j, ratios_df)
          }
          listname <- paste(substr(names(flights)[j], 0, 4), 'to', substr(names(flights)[j+1], 0, 4))
          sim_vals_czo[[listname]] <- ratios.j
          
        }
      
    ## did it work correctly? pick an example to test (e.g., y = 2, nlocs = 150)
    ## should match '2006 to 2010', nlocs = 150
        rangesim_czo(y = 2, flights, schemes.carrizo, nlocs = 150)
        sim_vals_czo$`2006 to 2010`[sim_vals_czo$`2006 to 2010`$nlocs == 150,]
            ## yes, it works!
  
        
## 4. plot & export:
        
  sim_vals_czo_melt <- NULL
        
    for (a in 1:length(sim_vals_czo)){
      sim_vals_czo_a <- sim_vals_czo[[a]]
      sim_vals_czo_a$name <- names(sim_vals_czo)[[a]]
      sim_vals_czo_melt <- rbind(sim_vals_czo_melt, sim_vals_czo_a)
    } 

        ggplot(sim_vals_czo_melt, aes(x = nlocs, y = ratio, col = scheme)) +
                geom_point() +
                facet_wrap(c('name', 'change'), scales = 'free') + 
                geom_hline(yintercept = 1, linetype = 'dashed', lwd = 0.8) +
                coord_cartesian(ylim = c(0.5,1.5), xlim = c(50,300)) +
                xlab('# trapping locations') + ylab('Estimated vs. expected trap detections') +
                theme(axis.text.x = element_text(size = 10, colour = 'black'),
                      axis.text.y = element_text(size = 10, colour = 'black'),
                      axis.title = element_text(size = 14, colour = 'black'),
                      axis.ticks = element_line(colour = 'black', size = 0.8),
                      axis.line.x = element_line(size = 0.5, colour = 'black'),
                      axis.line.y = element_line(size = 0.5, colour = 'black'),
                      strip.text = element_text(size= 14),
                      legend.text = element_text(size = 12), legend.title = element_text(size = 14),
                      plot.title = element_text(size = rel(1.5), face = 'bold', hjust = 0.5))
        
        
      ## save
      ## export
        
        