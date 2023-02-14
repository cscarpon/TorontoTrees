library(sf)
library(lidR)
library(tidyverse)
library(terra)

#Creating directories to write to
dir.create(file.path("E:", "LIDAR","Data", "StreetTrees"))
dir.create(file.path("E:", "LIDAR","Data", "HouseTrees"))


out_dirStreetTree <- file.path("E:", "LIDAR","Data", "StreetTrees")
out_dirHouseTree <- file.path("E:", "LIDAR", "Data", "HouseTrees")

#Roads

#Reading in and Projecting roads
roads <- st_read(file.path("Data", "Shape" , "Roads", "Toronto_Roads.shp"))
roads<- st_transform(roads , 26917)
roadBuffer <- st_buffer(roads,15)

#cityTrees

#Reading in and Projecting trees
trees <-  st_read(file.path("Data", "Shape" , "Trees", "Street Tree Data.shp"))
trees <- st_transform(trees, 26917)

#Generating an ID column to separate later
trees$ID <- 1:nrow(trees)
 
#Select roads that are within 15 m of the road
roadTrees <- st_intersection(roadBuffer, trees)
glimpse(roadTrees)

#Keeping Unique Trees
roadTrees <- distinct(roadTrees, 
                      ID, 
                      .keep_all = TRUE)

#Selecting only the species and geometry column
roadTrees <- roadTrees[,40:41]
names(roadTrees) <- c("Common Name", "DBH (cm)", "Geometry")

#Making road trees a sf_object
st_geometry(roadTrees) <- "Geometry"




#Trees that were not found within 15m
houseTrees <- anti_join(as.data.frame(trees), as.data.frame(roadTrees), by = "ID")

#View count after
glimpse(houseTrees)

names(houseTrees) <- c("Common Name", "DBH (cm)", "Geometry")
st_geometry(houseTrees) <- "Geometry"

houseTrees <- st_transform(houseTrees, 26917)



#Neighbourhoods
hoods <-  st_read(file.path("Data", "Shape" , "Neighbourhoods", "Neighbourhoods.shp")) 
hoods <- st_transform(hoods , 26917)

#We only want the neighorhood name
hoods <-  hoods[7]
names(hoods)[1] <- "Neighbourhoods"

#Fixing a naming issues in the data
hoods$Neighbourhoods[70] <- "Maryvale" 


#Viewing the df of all data to check for names or errors

hoods %>% 
  as_tibble() %>% 
  print(n=158)

#Sort the Street Trees in neighborhoods for further analysis
for (i in 1:nrow(hoods)) {
  
  hood <- hoods[i,]
  hoodTrees <- st_intersection(hood, roadTrees)
  hoodname <-  st_drop_geometry(hoods[i,1])
  st_write(hoodTrees, paste0(out_dirStreetTree,"/", hoodname, ".shp"))
  
}



