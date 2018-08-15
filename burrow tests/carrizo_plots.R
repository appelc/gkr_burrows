library(rgdal)

carrizo <- readOGR(dsn = 'C:/Users/Cara/Documents/__GKR/shapefiles', layer = 'carrizo_noedges')

plot(carrizo)

rand.pts <- spsample(carrizo, n = 100, type = 'regular')

points(rand.pts)

rand.spdf <- SpatialPointsDataFrame(data.frame(rand.pts$x1, rand.pts$x2), data = data.frame(rand.pts),
                                    proj4string = CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
plot(rand.spdf)

writeOGR(rand.spdf, dsn = 'C:/Users/Cara/Documents/__GKR/shapefiles', layer = 'carrizo_rand_100', driver = 'ESRI Shapefile')
