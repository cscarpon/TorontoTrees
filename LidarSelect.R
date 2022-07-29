library(sf)
library(lidR)
library(tidyverse)


#Roads
roads <- st_read(file.path("Data", "Shape" , "Roads", "Toronto_Roads.shp"))
roads<- st_transform(roads , 26917)
roadBuffer <- st_buffer(roads,15)


#cityTrees

trees <-  st_read(file.path("Data", "Shape" , "Trees", "Street Tree Data.shp"))
trees<- st_transform(trees, 26917)
roadTrees <- st_intersection(roadBuffer, trees)
treesBuffer <- st_buffer(roadTrees, 6.5)

str(treesBuffer)
plot(treesBuffer[1])


#Neighbourhoods
hoods <-  st_read(file.path("Data", "Shape" , "Neighbourhoods", "Neighbourhoods.shp")) 
hoods <- st_transform(hoods , 26917)
hoods <-  hoods[7]
names(hoods)[1] <- "Neighbourhoods"
hoods$Neighbourhoods[70] <- "Maryvale" 


hoods %>% 
  as_tibble() %>% 
  print(n=158)

#NeighbourhoodTiles
tiles <- st_read(file.path("Data", "Shape" , "Lidar", "Tiles.shp")) 
tiles<- st_transform(tiles , 26917)


tmp_dir <- file.path("E:", "LIDAR", "2014-15", "LAS_v1.2_ASPRS")
out_dir <- file.path("E:", "LIDAR", "Processed")
lasFile<- list.files(path = tmp_dir, full.names = TRUE, pattern = ".las")


for (i in 133:nrow(hoods)) {
  hood <- hoods[i,]
  hoodJoin <- st_join(hood,tiles)
  hoodRoads <- st_intersection(hood, roadBuffer)
  hoodRoads <- st_union(hoodRoads)
  hoodDF <- as.data.frame(hoodJoin)
  hoodList<- as.list(hoodDF$Label)
  hoodList2 <- do.call(c, hoodList)
  hoodLas <- sapply(hoodList2, function(y){lasFile[grepl(pattern = y, x = lasFile)]})
  lidar_catalog <- readLAScatalog(hoodLas,select = "xyzic")
  opt_independent_files(lidar_catalog) <- TRUE
  crs(lidar_catalog) <- crs(roads)
  roi <- clip_roi(lidar_catalog, hoodRoads)
  hoodname <-  st_drop_geometry(hoods[i,1])
  writeLAS(roi, paste0(out_dir,"/", hoodname, ".las"), index = FALSE)
}


