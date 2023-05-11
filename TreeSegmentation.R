library(sf)
library(lidR)
library(tidyverse)
library(terra)
library(leaflet)

devtools::install_github("sfr/RStudio-Addin-Snippets")



#Directories

in_dir <- file.path("E:", "LIDAR", "Data", "Normalised")
out_dirCHM <- file.path("E:", "LIDAR", "Data",  "CHM")
out_dirTreeLas <- file.path("E:", "LIDAR", "Data", "TreeLas")
#out_dir <- file.path("E:", "LIDAR","Data", "Trees6m")
#out_dir <- file.path("E:", "LIDAR","Data", "TreeSegmentation")
out_dirTops <- file.path("E:", "LIDAR","Data", "Tops")
out_dir <- file.path("E:", "LIDAR","Data", "TreeSegmentation")
lasFile <- list.files(path = in_dir, full.names = TRUE, pattern = ".las")

#cityTrees




trees <-  st_read(file.path("Data", "Shape" , "Trees", "Street Tree Data.shp"))
trees <- st_transform(trees, 26917)


for (i in 1:length(lasFile)) {
  
  #Read in the las file
  nlas <- readLAS(lasFile[i])
  
  #Get the name of the neighbourhood
  name <- sub(".*Normalised","",lasFile[i])
  name <- gsub('.{4}$', '', name)
  
  #Filter out points over 25m 
  poi = filter_poi(nlas, Z <= 25 )
  
  #Create a CHM
  chm <- grid_canopy(poi, res = 1, algorithm = p2r())
  #writeRaster(chm, paste0(out_dirCHM, name, ".tif"), overwrite=TRUE)
  
  #Locate Tree Heights
  ttops <- locate_trees(poi, lmf(ws = 4, shape = "circular", hmin = 2))
  ttops <- st_zm(ttops, drop = TRUE, what = "ZM")
  st_write(ttops, paste0(out_dirTops, name, ".shp"))

  #Point Cloud Segmentation
  algo <- dalponte2016(chm, ttops)
  poi <- segment_trees(poi, algo) # segment point cloud
  #writeLAS(poi, paste0(out_dirTreeLas,name, ".las"), index = FALSE)
  
  #Crowns
  crowns <- crown_metrics(poi, func = .stdtreemetrics, geom = "convex")
  st_write(crowns, paste0(out_dir, name, ".shp"))
  
}




#Leaflet
#


crowns_wgs <- st_transform(crowns, "epsg:4326")

plot(crowns)

leaflet(crowns_wgs) %>% 
  addPolygons(color = "green") %>% 
  addTiles() %>% 
  addRasterImage(raster::raster(chm), colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(chm),
            title = "Canopy Heights")

out_dirCHM <- file.path("E:", "LIDAR", "Data",  "CHM")
out_dir <- file.path("E:", "LIDAR","Data", "TreeSegmentation4")

chm <- rast(file.path("E:", "LIDAR", "Data",  "CHM","Annex.tif"))
crs(chm)
chm <- project(chm, "EPSG:4326")


crowns <- st_read(file.path("E:", "LIDAR","Data", "TreeSegmentation4", "Annex.shp"))
crowns_wgs <- st_transform(crowns, "epsg:4326")

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(chm), na.color = "transparent")

leaflet(crowns_wgs) %>% 
  addPolygons(color = "green", group = "Trees") %>% 
  addTiles() %>% 
  addRasterImage(raster::raster(chm), colors = pal, opacity = 0.8, group = "CHM") %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Trees", "CHM"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(pal = pal, values = values(chm),
            title = "Canopy Heights")