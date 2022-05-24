##Author Kochulem Edwin
## Script to perform clustering/Unsupervised classification
## on prepared indices or satellite bands using CLARA algorithm

##Required Packages/libraries

library(raster)
library(sf)
library(stats)
library(NbClust)
library(clusterCrit)
library(mvcluster)
library(cluster)

##Set working directory
setwd("E:/United Nations/R Code Test")


##Read Raster data i.e DBSI, UI,True color bands.
rst=raster('DBSI_WGS84_2010.tif')


rstDF <- as.data.frame(rst)

# Check NA's in the data
idx <- complete.cases(rstDF)

# Initiate the raster datasets that will hold all clustering solutions 
# from 2 groups/clusters up to 20
rstCLARA <- raster(rst[[1]])


##Set the number of classes
for(nClust in 2:15){
  
  cat("-> Clustering data for nClust =",nClust,"......")
  
  
  # Perform CLARA's clustering (using manhattan distance)
  cla <- clara(rstDF[idx, ], k = nClust, metric = "manhattan")
  
  # Create a temporary integer vector for holding cluster numbers
  claClust <- vector(mode = "integer", length = ncell(rst))
  
  
  # Generate the temporary clustering vector for CLARA (keeps track of NA's too ;-)
  claClust[!idx] <- NA
  claClust[idx] <- cla$clustering
  
  # Create a temporary raster for holding the new clustering solution
  # CLARA
  tmpRstCLARA <- raster(rst[[1]])
  
  # Set raster values with the cluster vector
  # CLARA
  values(tmpRstCLARA) <- claClust
  
  # Stack the temporary rasters onto the final ones
  if(nClust==2){
    rstCLARA <- tmpRstCLARA
  }else{
    rstCLARA <- stack(rstCLARA, tmpRstCLARA)
  }
  
  cat(" done!\n\n")
}

# Write the clustering solutions for the input raster file in Output folder inside the working directory
writeRaster(rstCLARA,"Outputs/Montreal_CLARA_classes_DBSI_2000.tif", overwrite=TRUE)

