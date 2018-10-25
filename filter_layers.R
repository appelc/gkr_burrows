## layers for filtering GKR monitoring locations

library(raster)
library(rgdal)
library(rgeos)


## import sdm raster & range outline
  sdm <- raster('VLAB_INPUTS/sdm_historical/model-0.5-5-nb.tif')
  sdm.spdf <- as(sdm, 'SpatialPixelsDataFrame')
  range <- readOGR(dsn = 'VLAB_INPUTS/sdm_outline_hist', layer = 'sdm_out_hist_poly_buff')  

      ## to create the 'range' shapefile in ArcMap:
      ##  - load SDM raster
      ##  - 'Raster Calculator': SDM raster > 0
      ##  - 'Raster to Polygon': convert raster from prev step to polygon
      ##  - 'Buffer': buffer polygon from prev step by 10km to be safe
  
  
## \/\/ SKIP THESE STEPS ONCE LAYERS HAVE BEEN DOWNLOADED/BUFFERED \/\/

  ## ROADS:
  
  library(tigris)
  
  # what counties do we need?
    ca_counties <- counties(state = 'CA', cb = TRUE) #cb = generalized (smaller file size)
    ca_counties <- spTransform(ca_counties, crs(sdm))
    counties_extr <- over(sdm.spdf, ca_counties)$NAME
    counties_list <- unique(counties_extr)
    
  # download roads layers for these counties
    roads_ca <- rbind_tigris(lapply(counties_list, function(x) roads(state = 'CA', county = x)))
    writeOGR(roads_ca, dsn = 'VLAB_INPUTS', layer = 'roads_ca', driver = 'ESRI Shapefile')
  
  # crop to 'range' shapefile (ACTUALLY, DO THIS IN ARCMAP)
    #roads_ca <- spTransform(roads_ca, crs(range))
    #roads_crop <- crop(roads_ca, range)
    #writeOGR(roads_crop, dsn = 'VLAB_INPUTS', layer = 'roads_crop', driver = 'ESRI Shapefile')
  
  
  ## LAND OWNERSHIP:
  
  # import 'super units' layer, downlaoded from CA protected areas database website
  ca_sunits <- readOGR('inputs_ignore/CPAD_2017a', layer = 'CPAD_2017a_SuperUnits')  
  
  # crop to 'range' shapefile
  ca_sunits <- spTransform(ca_sunits, crs(range))
  land_crop <- crop(ca_sunits, range)
  writeOGR(land_crop, dsn = 'inputs_ignore/vlab', layer = 'land_crop', driver = 'ESRI Shapefile')    
  
  ## /\/\ SKIP THESE STEPS ONCE LAYERS HAVE BEEN DOWNLOADED/BUFFERED /\/\


## buffer roads according to land ownership

## crop, gInstersection, & gBuffer take a long time in R, so I did these steps in ArcMap instead
## Description of process in ArcMap:

#1. CLIP 'roads_crop' to 'land_crop' -> roads on public lands ('roads_public')
#2. ERASE 'roads_public' from 'roads_crop' -> roads not on public lands ('roads_private')
#3. BUFFER 'roads_public' by 250m (side type = FULL, end type = ROUND) -> 'roads_public_buf250'
#4. BUFFER 'roads_private' by 5m (side type = FULL, end type = ROUND) -> 'roads_private_buf5'
#5. CLIP 'roads_public_buf250' to 'land_crop' -> 'roads_public_buf250_clip' 
#   (this step ensures that buffers around public roads are actually on public lands)
#6. MERGE 'roads_public_buf250_clip' & 'roads_private_buf5' -> 'roads_both_buffered'
#7. DISSOLVE 'roads_both_buffered' -> 'roads_both_buff_diss' (*optional*)

## Then convert to raster:
  
## 1. POLYGON TO RASTER 
  
## import buffered layer:
roads_buffered <- readOGR(dsn = 'inputs_ignore/Arc_outputs', layer = 'roads_both_buff_dis')


## create sampling schemes within the buffered area (switch to 'stratified_sampling.R')

