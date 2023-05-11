library(sf)
library(tidyverse)
library(svMisc)
library(leaflet)

#Directories
dir.create(file.path("E:", "LIDAR", "TreesNoHouse"))
out_dir <- file.path("E:", "LIDAR", "TreesNoHouse")

in_dir <- file.path("E:", "LIDAR", "TreesJoin")
trees <- list.files(path = in_dir, full.names = TRUE, pattern = ".shp")

houses <- st_read(file.path("Data","Shape","Mass"))
houses <- st_transform(houses, 26917)


for (i in 1:length(trees)) {
  treesHood <- st_read(trees[i]) 
  treesNo <- treesHood[!lengths(st_intersects(treesHood, houses)), ]
  name <- sub(".*TreesJoin","",trees[i])
  name <- gsub('.{4}$', '', name)
  st_write(treesNo, paste0(out_dir, name, ".shp"), overwrite = TRUE)
  progress(i)
}


treesHood <- st_read(trees[2]) 
treesNo <- treeHood[!lengths(st_intersects(treesHood, houses)), ]
name <- sub(".*TreesJoin","",trees[i])
name <- gsub('.{4}$', '', name)
st_write(treesNo, paste0(out_dir, name, ".shp"), overwrite = TRUE)


ggplot(houses) + 
  geom_sf() + 
  coord_sf()

