#### trying image classification using ORFEO toolbox

## based on workflow from http://faculty.wwu.edu/wallin/envr442/ENVI/442_segmentation_ENVI__Orfeo_acme4.htm

## I already did LMS (1-smoothing, 2-segmentation, 3-min size, 4-shapefile export) in ORFEO
## and opened .dbf in Excel: deleted 'label' column, added 'ID' column, exported as .CSV

setwd('C://Users/Cara/Documents/__GKR/burrow_practice/ORFEO/LMS/lms_5_20_min5')

## import data:

dat <- read.csv(file = 'lms_5_20_min5.csv')

head(dat)

## convert raw data to z-scores: (X - mean) / sd
 
  datz <- data.frame('id' = dat$id) ## data as z-score

    for (i in 2:10){
          mean.col <- mean(dat[[i]])
          sd.col <- sd(dat[[i]])
          z.col <- (dat[[i]] - mean.col) / sd.col
          datz <- cbind(datz, z.col)
    }
  
colnames(datz)[2:10] <- paste(colnames(dat[2:10]), 'z', sep = '.')
  
  
## K- MEANS CLUSTERING
  # this runs the kmeans routine on our data using everything except the ID as predictor variables
  # it will produce 50 spectral classes, run 100 iterations and use 25 random starting points
  # the output goes to an output data matrix called srun.km

srun.km <- kmeans(datz[, -1], 10, iter.max = 100, nstart = 25) # 10 spectral classes

print(srun.km) #output

cluster.df <- data.frame('id' = rep(0:(length(srun.km$cluster)-1)), 'spec' = srun.km$cluster)

  # writes an excel file (use a different name if you prefer) containing a single columns of numbers
  # representing the predicted spectral class number

write.csv(cluster.df, file = 'lms_5_20_min5_cluster_10cl.csv')

