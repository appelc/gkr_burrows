## compare burrow counts from Feature Analyst with Tim's SDM

library(raster)
library(rgdal)
library(sp)

sdm <- raster('C:/Users/Cara/Documents/__GKR/shapefiles/sdm')
  sdm
  plot(sdm)

burrows <- readOGR(dsn = 'C:/Users/Cara/Documents/__GKR/_carrizo_counts', 
                   layer = 'counts_berkeley_2018')
  burrows
  points(burrows) ## need to re-project

crs(sdm)

burrows_proj <- spTransform(burrows, CRS('+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
  points(burrows_proj)

burrows_proj$sdm <- extract(sdm, burrows_proj)

hist(burrows_proj$sdm, xlab = 'SDM value', main = NULL, ylab = '# burrows')


## but what are the range of SDM values in areas where the burrows are? are they just all really high anyway?

outline <- readOGR(dsn = 'C:/Users/Cara/Documents/__GKR/_Carrizo_counts', 
                          layer = 'counts_berkeley_2018_polygons')
burrow_outline <- spTransform(outline, CRS('+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))

plot(burrow_outline, add = TRUE)
plot(sdm, add = TRUE)

burrow_outline$sdm <- extract(sdm, burrow_outline)

hist(burrow_outline$sdm)

sdm_burrows <- crop(sdm, extent(burrow_outline))
sdm_burrows <- mask(sdm_burrows, burrow_outline)
plot(sdm_burrows)
points(burrows_proj, cex = 0.2)

hist(sdm_burrows, main = NULL, xlab = 'sdm value')

## lm

table <- data.frame(table(burrows_proj@data$sdm)) ## number of burrows at each SDM value
table$suitability <- as.numeric(as.character(table$Var1))
table$burrows <- as.numeric(table$Freq)
table <- table[,c(3:4)]

lm1 <- lm(burrows ~ suitability, dat = table)
summary(lm1)

plot(table$burrows ~ table$suitability)
abline(lm1)

