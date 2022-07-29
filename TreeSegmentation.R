library(sf)
library(lidR)
library(tidyverse)
library(terra)


#Directories
dir.create(file.path("E:", "LIDAR", "Trees"))
dir.create(file.path("E:", "LIDAR", "TreeLas"))
dir.create(file.path(("E:", "LIDAR", "CHM")))

tmp_dir <- file.path("E:", "LIDAR", "Normalised")
out_dirCHM <- file.path("E:", "LIDAR", "CHM")
out_dirTreeLas <- file.path("E:", "LIDAR", "TreeLas")
out_dir <- file.path("E:", "LIDAR", "Trees")
lasFile<- list.files(path = tmp_dir, full.names = TRUE, pattern = ".las")

#Creating a search window for the trees
f <- function(x) {x * 0.1 + 3}
heights <- seq(0,30,5)
ws <- f(heights)


for (i in 1:length(lasFile)) {
  
  nlas <- readLAS(lasFile[i])
  name <- sub(".*Normalised","",lasFile[i])
  name <- gsub('.{4}$', '', name)
  
  #Create a CHM
  chm <- grid_canopy(nlas, res = 0.5, algorithm = p2r())
  chm <- rast(chm)
  writeRaster(chm, paste0(out_dirCHM, name, ".tif"), overwrite=TRUE)
  
  #Locate Tree Heights
  ttops <- locate_trees(nlas, lmf(ws = ws, shape = "circular", hmin = 3))

  #Point Cloud Segmentation
  algo <- dalponte2016(chm, ttops)
  nlas <- segment_trees(nlas, algo) # segment point cloud
  writeLAS(nlas, paste0(out_dirTreeLas,name, ".las"), index = FALSE)
  
  #Crowns
  crowns <- crown_metrics(nlas, func = .stdtreemetrics, geom = "convex")
  st_write(crowns, paste0(out_dir, name, ".shp"))
}