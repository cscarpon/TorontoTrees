library(sf)
library(lidR)
library(tidyverse)
library(terra)


#Directories
tmp_dir <- file.path("E:", "LIDAR", "Processed")
out_dir <- file.path("E:", "LIDAR", "Normalised")

#Lidar Files
lasFile<- list.files(path = tmp_dir, full.names = TRUE, pattern = ".las")

for (i in 1:length(lasFile)) {
  
  #Read las
  las <- readLAS(lasFile[i])
  
  #Generate Names
  name <- sub(".*Processed","",lasFile[i])
  name <- gsub('.{4}$', '', name)
  
  #Normalize
  nlas <- normalize_height(las, knnidw())
  
  #Filter
  poi = filter_poi(nlas, 0 <= Z)
  writeLAS(poi, paste0(out_dir,name, ".las"), index = FALSE)
}


#nlas <- segment_shapes(nlas, shp_hline(th1 = 0.5, th2 = 0.5, k = 5), "hline")
#nlas <- segment_shapes(nlas, shp_plane(th1 = 5, th2 = 10, k = 15), "plane")
#

lasFile<- list.files(path = out_dir, full.names = TRUE, pattern = ".las")

las <- readLAS(lasFile[1])

plot(las)
