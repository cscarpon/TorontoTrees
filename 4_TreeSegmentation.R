library(sf)
library(lidR)
library(tidyverse)
library(terra)
library(leaflet)



#Directories
in_dir <- file.path("E:", "LIDAR", "Data", "Normalised")
lasFile <- list.files(path = in_dir, full.names = TRUE, pattern = ".las")

out_dirHouseTree <- file.path("E:", "LIDAR", "Data", "HouseTrees")

in_dirSteet <- file.path("E:", "LIDAR", "Data", "StreetTrees")
streetTrees <- list.files(path = in_dirSteet, full.names = TRUE, pattern = ".shp")

out_dirCHM <- file.path("E:", "LIDAR", "Data",  "CHM")
out_dirTreeLas <- file.path("E:", "LIDAR", "Data", "TreeLas")


out_dir <- file.path("E:", "LIDAR","Data", "TreeSegmentation")

#Loop to read in each neighborhood and generate 

for (i in 1:length(lasFile)) {
  
  #Read in the las file and tree lcoation data
  nlas <- readLAS(lasFile[i])
  ttops <- st_read(streetTrees[i])
  ttops <- tibble::rowid_to_column(ttops, "treeID")
  
  #Get the name of the neighbourhood for the las fil
  name <- sub(".*Normalised","",lasFile[i])
  name <- gsub('.{4}$', '', name)
  
  #Filter out points over 25m 
  poi = filter_poi(nlas, Z <= 25 )
  
  #Create a CHM
  chm <- grid_canopy(poi, res = 1, algorithm = p2r())
  #writeRaster(chm, paste0(out_dirCHM, name, ".tif"), overwrite=TRUE)

  #Point Cloud Segmentation
  algo <- dalponte2016(chm, ttops)
  poi <- segment_trees(poi, algo) # segment point cloud
  writeLAS(poi, paste0(out_dirTreeLas,name, ".las"), index = FALSE)
  
  #Crown generation around the tree centre
  crowns <- crown_metrics(poi, func = .stdtreemetrics, geom = "convex")
  st_write(crowns, paste0(out_dir, name, ".shp"))
  
}



#Leaflet

#Read in the Annex neighbourhood CHM and Crown Segmentation

streetTrees <- st_read("E:/LIDAR/Data/StreetTrees/Annex.shp")
streetTrees <- st_transform(streetTrees, "epsg:4326")

streetTrees <- streetTrees %>%
  mutate(long = unlist(map(streetTrees$geometry,1)),
         lat = unlist(map(streetTrees$geometry,2)))

glimpse(streetTrees)

#CHM
chm <- rast(file.path("E:", "LIDAR", "Data",  "CHM","Annex.tif"))
chm <- project(chm, "EPSG:4326")

#Crowns
crowns <- st_read(file.path("E:", "LIDAR","Data", "TreeSegmentation", "Annex.shp"))
crowns_wgs <- st_transform(crowns, "epsg:4326")
glimpse(crowns_wgs)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(chm), na.color = "transparent")

trees <- leaflet(crowns_wgs) %>% 
          addPolygons(color = "green", group = "Tree Canopy") %>% 
          #addMarkers(lng=streetTrees$long, 
          #           lat=streetTrees$lat, 
          #           popup = ~as.character(streetTrees$Cmmn_Nm), 
          #           label = ~as.character(streetTrees$Cmmn_Nm)) %>% 
          addTiles() %>% 
          addRasterImage(raster::raster(chm), 
                         colors = pal, 
                         opacity = 0.8, 
                         group = "CHM") %>%
          # Layers control
          addLayersControl(
            overlayGroups = c("Street Trees", "Tree Canopy", "CHM"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>% 
          addLegend(pal = pal, values = values(chm),
                    title = "Canopy Heights (m)") 

htmlwidgets::saveWidget(trees, file="trees.html")


las <- readLAS("E:/LIDAR/Data/Normalised/Annex.las")

plot(las)
