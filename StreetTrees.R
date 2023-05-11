library(sf)
library(lidR)
library(tidyverse)
library(terra)


dir.create(file.path("E:", "LIDAR", "StreetTrees"))

#tmp_dir <- file.path("E:", "LIDAR", "2014-15", "LAS_v1.2_ASPRS")
out_dirStreetTree <- file.path("E:", "LIDAR", "StreetTrees")
out_dirHouseTree <- file.path("E:", "LIDAR", "Data", "HouseTrees")

#Roads

roads <- st_read(file.path("Data", "Shape" , "Roads", "Toronto_Roads.shp"))
roads<- st_transform(roads , 26917)
roadBuffer <- st_buffer(roads,15)

#cityTrees

trees <-  st_read(file.path("Data", "Shape" , "Trees", "Street Tree Data.shp"))
trees <- st_transform(trees, 26917)


trees$ID <- 1:nrow(trees)
 

roadTrees <- st_intersection(roadBuffer, trees)
roadTest <- roadTrees %>% 
              distinct(ID, .keep_all = TRUE)

nrow(roadTest)

roadTrees <- roadTrees[,40:41]
names(roadTrees) <- c("Common Name", "DBH (cm)", "Geometry")
st_geometry(roadTrees) <- "Geometry"


glimpse(roadTrees)
nrow(roadTrees)


houseTrees <- anti_join(as.data.frame(trees), as.data.frame(roadTrees), by = "ID")



houseTrees <- houseTrees[,13:15]
names(houseTrees) <- c("Common Name", "DBH (cm)", "Geometry")
st_geometry(houseTrees) <- "Geometry"








#Neighbourhoods
hoods <-  st_read(file.path("Data", "Shape" , "Neighbourhoods", "Neighbourhoods.shp")) 
hoods <- st_transform(hoods , 26917)
hoods <-  hoods[7]
names(hoods)[1] <- "Neighbourhoods"
hoods$Neighbourhoods[70] <- "Maryvale" 


hoods %>% 
  as_tibble() %>% 
  print(n=158)


for (i in 1:nrow(hoods)) {
  
  hood <- hoods[i,]
  hoodTrees <- st_intersection(hood, houseTrees)
  hoodname <-  st_drop_geometry(hoods[i,1])
  st_write(hoodTrees, paste0(out_dirHouseTree,"/", hoodname, ".shp"))
  
}



