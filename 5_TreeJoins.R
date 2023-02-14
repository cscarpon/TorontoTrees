library(sf)
library(tidyverse)
library(dplyr)
library(svMisc)
library(ggplot2)
library(terra)

##Extracting heights for trees from the CHM

#Directories
out_dir <- file.path("E:", "LIDAR","Data", "TreesJoin")
out_dir2 <- file.path("E:", "LIDAR","Data", "Missing")

out_dirSeg <- file.path("E:", "LIDAR","Data", "TreeSegmentation", "Merge")


in_dirTreeSeg <- file.path("E:", "LIDAR","Data", "TreeSegmentation")
treesSeg <- list.files(path = in_dirTreeSeg , full.names = TRUE, pattern = ".shp")


in_dirCHM <- file.path("E:", "LIDAR","Data", "CHM")
chmFiles <- list.files(path = in_dirCHM, full.names = TRUE, pattern = ".tif")


in_dirSteet <- file.path("E:", "LIDAR", "Data", "StreetTrees")
streetTrees <- list.files(path = in_dirSteet, full.names = TRUE, pattern = ".shp")

treeSegVect <- vect(treesSeg[1])

#Needed for the script
testDF <- data.frame()

#Script to extract the maxium height from the CHM by segmented trees

for (i in 1:length(treesSeg)){
  
  treeSegVect <- vect(treesSeg[i])
  chmRast <- rast(chmFiles[i])
  treeDF <- data.frame(Height= NA, x = NA, y = NA)
  treeDF <- na.omit(treeDF)
  
  for (j in 1:nrow(treeSegVect)) {
    treeExtract<- extract(chmRast, treeSegVect[j], xy =TRUE, ID = FALSE)
    tree <- treeExtract[which.max(treeExtract[,1]),]
    colnames(tree) <- colnames(treeDF)
    treeDF <- rbind(tree,treeDF)
  }
  #create a pt shp file of the max heights for the trees segmentation to intersect
  maxVect <- vect(treeDF, geom=c("x","y"), crs = "epsg:26917")
  
  #intersect only returns points for some reason...
  treeSegInt <- terra::intersect(treeSegVect, maxVect)
  treeSegInt <- treeSegInt[,c(1,5)]
  segDF <- as.data.frame(treeSegInt)
  testDF <- rbind(segDF,testDF)
  
  treeSegMerge <- terra::merge(treeSegVect, treeSegInt, by.x = "treeID", by.y = "treeID")
  
  name <- sub(".*TreeSegmentation","",treesSeg[i])
  name <- gsub('.{4}$', '', name)
  
  
  writeVector(treeSegMerge, paste0(out_dirSeg, name, ".shp"))
}

#This loop joins missing tree data
#

#Updating the tree segmentation to the new segmentation
in_dirSeg <- file.path("E:", "LIDAR","Data", "TreeSegmentation", "Merge")
treesSeg <- list.files(path = in_dirSeg , full.names = TRUE, pattern = ".shp")

for (i in 1:length(streetTrees)) {
  
  trees1 <- st_read(treesSeg[i])
  
  #Test to see if segments are valid, only keep valid segments
  trees1 <- trees1[st_is_valid(trees1), ]
  
  streetTrees1 <- st_read(streetTrees[i])
  emptySF<- st_sf(st_sfc(crs = "epsg:26917"))
  
  join <- st_intersection(streetTrees1, trees1)
  emptySF <- rbind(join, emptySF)
  
  #Find the trees that were not initially joined
  diff <- streetTrees1[!lengths(st_intersects(streetTrees1, trees1)), ]
  join2 <-  st_intersection(diff, trees1)
  emptySF <- rbind(join2, emptySF)
  
  diff2 <- streetTrees1[!lengths(st_intersects(streetTrees1, emptySF)), ]
  
  name <- sub(".*Trees","",streetTrees[i])
  name <- gsub('.{4}$', '', name)
  
  #Extract Heights from CHM
  treeVect <- vect(emptySF)
  chmRast <- rast(chmFiles[i])
  
  
  
  treeExtract<- extract(chmRast, treeVect, fun= max, bind = TRUE, xy = TRUE)
  names(treeExtract) <- c("Hood","Name","DBH", "treeID","Z", "nPoints","Area", "chmHeight")
  #Take Raster Height, if not available, take the segmented heights
  treeExtract$Heights <- ifelse(is.na(treeExtract$chmHeight),
                                     treeExtract$Z,(treeExtract$chmHeight))
  writeVector(treeExtract,paste0(out_dir, name, ".shp"))
  st_write(diff2, paste0(out_dir2, name, ".shp"), overwrite = TRUE)
  
  
}
  