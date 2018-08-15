## practicing loading and working with imagery

library(raster)
library(rgdal)
library(zoom) # interactive plot zoom capability
library(rpart) # recursive partitioning and regression trees


# can use 'brick' or 'stack' to import multi-band raster data ('brick' should be faster but less flexible)
# ('raster' is for single-band imagery)
  
  # start with Elkhorn Plain ('eh') 2016
  naip_eh16 <- brick('C:/Users/Cara/Documents/bda/Bulk Order 887779/NAIP GEOTIFF/m_3511960_nw_11_h_20160713/m_3511960_nw_11_h_20160713.tif')
 
  naip_eh16 # view properties
  nlayers(naip_eh16) # 4 bands

  plotRGB(naip_eh16, r = 1, g = 2, b = 3) # true color
  plotRGB(naip_eh16, r = 4, g = 1, b = 2) # near-infrared (NIR) / 'false color' (omits blue band)

# these are quarter quad (QQ) tiles at roughly 7 km x 7 km
  naip_eh16@extent[2] - naip_eh16@extent[1] # xmax - xmin (dimensions in meters)
  naip_eh16@extent[4] - naip_eh16@extent[3] # ymax - ymin (dimensions in meters)
  
  xres(naip_eh16) # 0.6 m (60 cm) -- this is 2016; previous years are 1 m
  yres(naip_eh16) # 0.6 m (60 cm)
  

## spatial subsetting (cropping):

  # read 1-km2 grid from rangewide project
  grid1km <- readOGR(dsn = 'C:/Users/Cara/Documents/__GKR/RangeWide_Spring2017/grids',
                     layer = 'new1km')
  
  grid1km <- spTransform(grid1km, naip_eh16@crs) ## re-project grid layer in same CRS as NAIP

  plotRGB(naip_eh16, r = 1, g = 2, b = 3) 
  plot(grid1km, add = TRUE)
  
  # subset specific cell I want (figure out how to crop rest later) -- I found the GridID in ArcMap
  # (eh for Elkhorn)
  eh_cell_a <- grid1km[grid1km$GridID == 'BB43JG213',]
  plot(eh_cell_a, add = TRUE)

  # crop NAIP to 1-km2 cell
  naip_eh16_a <- crop(naip_eh16, eh_cell_a)

  plotRGB(naip_eh16_a, r = 1, g = 2, b = 3)
  
  naip_eh16_a@extent[2] - naip_eh16_a@extent[1] # 1057.8 m (why not exactly 1000?)
  naip_eh16_a@extent[4] - naip_eh16_a@extent[3] 
  
# also import Elkhorn Plain 2014 NAIP
  naip_eh14 <- brick('C:/Users/Cara/Documents/bda/Bulk Order 887779/NAIP GEOTIFF/m_3511960_nw_11_1_20140602/m_3511960_nw_11_1_20140602.tif')

    xres(naip_eh14)  # 1 m res
    yres(naip_eh14)  
    
  naip_eh14_a <- crop(naip_eh14, eh_cell_a) # crop to same 1-km2 cell
  
  plotRGB(naip_eh14_a, r = 1, g = 2, b = 3)
      
# plot 2014/2016 side-by-side

  par(mfrow = c(1,2))
  plotRGB(naip_eh16_a, r = 1, g = 2, b = 3, axes = TRUE, main = 'Elkhorn 2016')
  plotRGB(naip_eh14_a, r = 1, g = 2, b = 3, axes = TRUE, main = 'Elkhorn 2014')

# can use stretch = 'lin'  to increase contrast for visualization (or stretch = 'hist')
    # stretches pixel values to utilize full range (0-255)

  par(mfrow = c(1,2))
  plotRGB(naip_eh16_a, r = 1, g = 2, b = 3, axes = TRUE, main = 'Elkhorn 2016')
  plotRGB(naip_eh16_a, r = 1, g = 2, b = 3, stretch = 'lin', axes = TRUE, main = 'Elkhorn 2016')
  
  par(mfrow = c(1,2))
  plotRGB(naip_eh16_a, r = 1, g = 2, b = 3, stretch = 'lin', axes = TRUE, main = 'Elkhorn 2016')
  plotRGB(naip_eh14_a, r = 1, g = 2, b = 3, stretch = 'lin', axes = TRUE, main = 'Elkhorn 2014')


## 
## export 1-km2 cells as geotiffs
  
  writeRaster(naip_eh14_a, 'C:/Users/Cara/Documents/__GKR/burrow_practice/GeoTIFFs/naip_eh14_a.tif', 'GTiff')
  writeRaster(naip_eh16_a, 'C:/Users/Cara/Documents/__GKR/burrow_practice/GeoTIFFs/naip_eh16_a.tif', 'GTiff')
  
  ## and regular tiffs (for imagej)
  
  ## NOT WORKING... supposed to remove geo for programs that can't read geotiff (but I don't think that's my problem with imageJ... file properties doesn't have bit info)
    #writeRaster(naip_eh14_a, 'C:/Users/Cara/Documents/__GKR/burrow_practice/naip_eh14_a.tif',
    #            options = c("PROFILE=BASELINE"))

  ## instead just plot and export image as TIFF    
    plotRGB(naip_eh14_a, r = 1, g = 2, b = 3)  # use axes = true to calibrate in imagej then crop out axes
    plotRGB(naip_eh16_a, r = 1, g = 2, b = 3)

    ###
    
    
  
### calculate NDVI
  
  # write function:
   # (i and k are the index of bands to be used for the indices computation)
  
    vi <- function(img, i, k){
            bi <- img[[i]]
            bk <- img[[k]]
            vi <- (bk-bi)/(bk+bi)
            return(vi)
          }
    
    # for NAPI: NIR = 4, red = 1
      ndvi_14 <- vi(naip_eh14_a, 1, 4)
        plot(ndvi_14, col = rev(terrain.colors(30)), main = 'NDVI from NAIP 2014')
    
      ndvi_16 <- vi(naip_eh16_a, 1, 4)
        plot(ndvi_16, col = rev(terrain.colors(30)), main = 'NDVI from NAIP 2016')
        
        ## neither is very green!!
        

####
#### try some unsupervised classification (k-means clustering)

  # based on NDVI layer (prob not very helpful)
      nr <- getValues(ndvi_14)
      nr.km <- kmeans(na.omit(nr), centers = 5, iter.max = 500, nstart = 3, algorithm = 'Lloyd') # look into parameters and algorithms more
      
      knr <- ndvi_14
      knr[] <- nr.km$cluster
    
      plotRGB(naip_eh14_a, r = 1, g = 2, b = 3, stretch = 'lin', axes = TRUE, main = 'Elkhorn 2014')
      plot(knr, main = 'Unsupervised classification of NAIP (NVDI) data')

  # based on true color NAIP  
      nr <- getValues(naip_eh16_a)
      nr.km <- kmeans(na.omit(nr), centers = 10, iter.max = 500, nstart = 3, algorithm = 'Lloyd')  
      knr <- naip_eh16_a    
      knr[] <- nr.km$cluster  
    
      plot(knr, main = 'unsup class of NAIP 2014')  
  
      zm()
  

####
#### try supervised classification: decision tree / classifcation and regression tree (package 'rpart')
  
  ## need to create a training set in GIS first
  

  plotRGB(Naip_eh14, r = 2, g = 3)