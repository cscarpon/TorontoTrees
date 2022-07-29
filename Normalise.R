library(sf)
library(lidR)
library(tidyverse)


#Directories
tmp_dir <- file.path("E:", "LIDAR", "Processed")
out_dir <- file.path("E:", "LIDAR", "Normalised")
lasFile<- list.files(path = tmp_dir, full.names = TRUE, pattern = ".las")

#1:153
for (i in 1:length(lasFile)) {
  las <- readLAS(lasFile[i])
  nlas <- normalize_height(las, knnidw())
  nlas <- segment_shapes(nlas, shp_hline(th1 = 0.5, th2 = 0.5, k = 5), "hline")
  nlas <- segment_shapes(nlas, shp_plane(th1 = 5, th2 = 10, k = 15), "plane")
  poi = filter_poi(nlas, 0 < Z & Z <= 25 & hline == FALSE & plane == FALSE)
  name <- sub(".*Processed","",lasFile[i])
  writeLAS(poi, paste0(out_dir,name), index = FALSE)
}
