library(sf)
library(lidR)
library(tidyverse)
library(terra)


hoods <-  st_read(file.path("Data", "Shape" , "Neighbourhoods", "Neighbourhoods.shp")) 
hoods <- st_transform(hoods , 26917)
hoods <-  hoods[7]
names(hoods)[1] <- "Neighbourhoods"
hoods$Neighbourhoods[70] <- "Maryvale" 

houses <- st_read(file.path("Data","Shape","Mass"))
houses <- st_zm(houses, drop=T, what='ZM')
houses <- st_transform(houses, 26917)

roads <- st_read(file.path("Data", "Shape" , "Roads", "Toronto_Roads.shp"))
roads<- st_transform(roads , 26917)
roadBuffer <- st_buffer(roads,15)

hoodname <-  st_drop_geometry(hoods[1,1])

out_road <- file.path("E:", "LIDAR", "Data", "Roads")

for (i in 1:nrow(hoods)) {
  
  hood <- hoods[i,]
  hoodRoads <- st_intersection(hood, roadBuffer)
  hoodRoads <- st_union(hoodRoads)
  hoodHouses <- st_intersection(hood,houses)
  hoodHouses <- st_union(hoodHouses)
  diffPoly <- st_difference(hoodRoads,hoodHouses)
  hoodname <-  st_drop_geometry(hoods[i,1])
  st_write(diffPoly, paste0(out_road ,"/", hoodname, ".shp"))
  
}

in_norm <- file.path("E:", "LIDAR", "Normalised")
out_norm <- file.path("E:", "LIDAR", "Data", "Normalised")
lasFile<- list.files(path = in_norm , full.names = TRUE, pattern = ".las")


in_roads<- file.path("E:", "LIDAR","Data", "Roads")
roadFiles<- list.files(path = in_roads , full.names = TRUE, pattern = ".shp")


for (i in 1:length(lasFile)) {
  
  #Read las
  las <- readLAS(lasFile[i])
  
  #Read Roads
  roads <- st_read(roadFiles[i])
  
  #Generate Names
  name <- sub(".*Normalised","",lasFile[i])
  name <- gsub('.{4}$', '', name)
  
  roi <- clip_roi(las, roads)
  writeLAS(roi, paste0(out_norm, name, ".las"), index = FALSE)
}