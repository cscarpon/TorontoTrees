library(sf)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(tidyverse)



#Directories
dir.create(file.path("E:", "LIDAR", "Data", "CSV"))
out_dir <- file.path("E:", "LIDAR", "Data", "CSV")

in_dir <- file.path("E:", "LIDAR", "Data", "TreesJoin")
trees <- list.files(path = in_dir, full.names = TRUE, pattern = ".shp")



#Tree Inventory names

TreeNames<- read_csv(file.path("Data", "Shape", "itree", "TreeSpecies.csv"))
names(TreeNames)[4] <- "Common" 

TreeNames$Common <- gsub("class","" ,as.character(TreeNames$Common))
TreeNames$Common <- gsub("spp","" ,as.character(TreeNames$Common))
TreeNames$Common<- str_trim(TreeNames$Common)
TreeNames$Common <- tolower(TreeNames$Common)


treesMissing <- st_sf(st_sfc(crs = "epsg:26917"))



for(j in 1:length(trees)){
  
  treesNew <- list()
  treesHood <- st_read(trees[j])
  names(treesHood) <- c("Hood","Common", "DBH","TreeID", "Height","nPoints", "Area", "geometry")
  treesHood$long <- st_coordinates(treesHood)[,1]
  treesHood$lat <- st_coordinates(treesHood)[,2]
  
  name <- sub(".*TreesJoin","",trees[j])
  name <- gsub('.{4}$', '', name)
  

  for (i in 1:length(treesHood$Common)) {
    vect <- treesHood$Common[i]
    # split string by blank spaces
    string_split = strsplit(as.character(vect), split = ",")
    # how many split terms?
    string_length = length(string_split[[1]])
    # decide what to do
    if (string_length == 1) {
      # one word (do nothing)
      reversed_string = string_split[[1]]
    } else {
      # more than one word (collapse them)
      reversed_split = string_split[[1]][string_length:1]
      reversed_string = paste(reversed_split, collapse = " ")
    }
      # output
    reversed_string <- gsub(".*\\'","", reversed_string)
    treesNew[i]<- tolower(str_trim(reversed_string))
  } 

  treesHood$Common <- do.call(c, treesNew)
  join <- left_join(treesHood,TreeNames, by = "Common" )
  join <- st_drop_geometry(join)
  join <- as.data.frame(join)
  write_csv(join, paste0(out_dir,name,".csv"))
  
  missing <- join[is.na(join$Code),]
  treesMissing <- rbind(missing,treesMissing)
}


in_dir <- file.path("E:", "LIDAR", "Data", "CSV")
csv <- list.files(path = in_dir, full.names = TRUE, pattern = ".csv")

master <- list()

for(j in 1:length(csv)){
  csvTree <- read_csv(csv[j])  
  master <- rbind(csvTree,master) 
}

write_csv(master, paste0(out_dir,"/Master",".csv"))

missingTrees <- as.data.frame(unique(treesMissing$Common))
write_csv(missingTrees, paste0(out_dir,"/missing",".csv"))

in_dir <- file.path("E:", "LIDAR", "Data")

csvTree <- read_csv(paste0(in_dir, "/Master.csv")) 

csvTree <- distinct(csvTree)

write_csv(csvTree, paste0(in_dir, "/MasterClean.csv"))
csvTree <- read_csv(paste0(in_dir,"/MasterClean",".csv")) 

csvTree


#Calculating Neighbourhood area and Total trees per Neighborouhood
#

hoods <-  st_read(file.path("Data", "Shape" , "Neighbourhoods", "Neighbourhoods.shp")) 
hoods <- st_transform(hoods , 26917)
hoods <-  hoods[7]
names(hoods)[1] <- "Neighbourhoods"
hoods$Neighbourhoods[70] <- "Maryvale" 

Annex = hoods[hoods$Neighbourhoods == "Annex",]

plot(Annex)

write_sf(Annex,"Annex.shp")

hoods[1,63]

hoods[1]

hoods$Neighbourhoods

nc[1, "NWBIR74"]

hoods$area <- st_area(hoods)


treeCount<- master %>% 
              count(Hood)

hoodTrees <- merge(hoods, treeCount, by.x ="Neighbourhoods", by.y = "Hood")

hoodTrees <-  st_drop_geometry(hoodTrees)
write_csv(hoodTrees, paste0(out_dir,"/Neighbourhoods",".csv"))

#GGPlots for tree descriptives
#
glimpse(master)


speciesCount<- csvTree %>% 
  count(Common) %>% 
  arrange(., desc(n)) %>% 
  top_n(., 20, n)



#Top 20 trees and number
p <-ggplot(data=speciesCount, aes(x=reorder(Common, n), y=n)) +
  geom_bar(stat="identity", width = 0.5, fill="green") +
  xlab("Common Species)") + 
  ylab("Count") + 
  theme_minimal()+
  coord_flip()

p 

glimpse(master)

#Average Heights by trees

treeHeight<- csvTree %>%
  group_by(Common) %>%
  dplyr::summarize(Mean = mean(Height, na.rm=TRUE), Total = n() ) %>% 
  top_n(.,20,Total) %>% 
  arrange(., desc(Mean))

ggplot(csvTree, aes(x=Common, y=Height)) + 
  geom_boxplot() +
  theme_minimal()+
  coord_flip()

wrongTrees <- master %>% 
  filter(., Height > 30) %>% 
  top_n(.,50,Height) %>% 
  arrange(., desc(Height))