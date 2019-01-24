## layers for filtering GKR monitoring locations

library(tigris)
library(raster)
library(rgdal)
library(rgeos)


## import sdm raster & convert to SPDF
  sdm <- raster('inputs_ignore/sdm')
  sdm.spdf <- as(sdm, 'SpatialPixelsDataFrame')

  range <- readOGR('inputs_ignore', layer = 'sdm_outline_shp')  

## ROADS

  # what counties do we need?
      ca_counties <- counties(state = 'CA', cb = TRUE) #cb = generalized (smaller file size)
      ca_counties <- spTransform(ca_counties, crs(sdm))

      counties_extr <- over(sdm.spdf, ca_counties)$NAME
      counties_list <- unique(counties_extr) #these are the ones we need layers for
      
  # download roads layers for these counties
      roads_ca <- rbind_tigris(
        lapply(
          counties_list, function(x) roads(state = 'CA', county = x)
        )
      )
      
  # crop to 'range' shapefile
      #roads_crop <- gIntersection(roads_ca, range)
      roads_ca <- spTransform(roads_ca, crs(range))
      roads_crop <- crop(roads_ca, range)

## PUBLIC LANDS
      
    # import 'super units' layer, downlaoded from CA protected areas database website
      ca_sunits <- readOGR('inputs_ignore/CPAD_2017a', layer = 'CPAD_2017a_SuperUnits')  
      
    # crop to 'range' shapefile
      
      