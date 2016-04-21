library(maptools)
library(ggplot2)

# read the data from 2013
shapeFile <- "national-esri-16122011/COM20111216_ELB_region.shp"

xx <- readShapeSpatial(shapeFile)
#xxx <- thinnedSpatialPoly(as(xx, "SpatialPolygons"),
#                          tolerance=0.05, minarea=0.001, topologyPreserve=TRUE)


library(rmapshaper)
# if the rmapshaper library is not installed: 
#devtools::install_github("ateucher/rmapshaper")
?ms_simplify

xxx <- ms_simplify(xx, keep=0.075) # use instead of thinnedSpatialPoly

xxx <- ms_simplify(xx, keep=0.025) # 
xxx <- ms_simplify(xx, keep=0.01) # 

plot(xx) # just checking
plot(xxx) # do those two plots look the same?

# adjust names here
nat_data <- xx@data
nat_data$id <- row.names(nat_data)
write.csv(nat_data, "AECdata/National-data.csv", row.names=FALSE)

# include electorate names and states into the map
nat_map <- fortify(xxx)
nat_map <- merge(nat_map, nat_data[,c("id", "STATE", "ELECT_DIV")])
# write.csv messes things up
nat_map$group <- paste("g",nat_map$group,sep=".")
nat_map$piece <- paste("p",nat_map$piece,sep=".")

write.table(nat_map, "AECdata/National-map-10.csv", row.names=FALSE, col.names=TRUE, sep=",",
            quote=TRUE)

####################
nm <- read.csv("AECdata/National-map-10.csv")
ggplot(aes(map_id=id), data=nat_data) +
  geom_map(aes(fill=AREA_SQKM), map=nm) +
  expand_limits(x=nm$long, y=nm$lat) + 
  theme_map()

####################
# now substitute the data for NSW, WA, and ACT

shapeFile <- "shapefiles/Shape (ESRI)/WA_Electoral_Boundaries_19-01-2016.shp"
shapeFile <- "shapefiles/act-esri-28012016/ACT_Electoral_Boundaries_28-01-2016.shp"


shapeFile <- "shapefiles/qld-shape-files-13012010/QLD_ELB_031209_region.shp"
shapeFile <- "shapefiles/sa-esri-16122011/E_SA16122011_region.shp"
shapeFile <- "shapefiles/vic-esri-24122010/vic 24122010.shp"
shapeFile <- "shapefiles/nsw-esri-06042016/NSW_electoral_boundaries_25-02-2016.shp"


victoria <- fortify(xxx)
victoria$long01 <- (victoria$long-min(victoria$long))/diff(range(victoria$long))
victoria$lat01 <- (victoria$lat-min(victoria$lat))/diff(range(victoria$lat))


victoria$new_long <- victoria$long01*(150.080812-140.685736) + 140.685736
victoria$new_lat <- victoria$lat01*(-33.953035+39.166037) -39.166037

victoria$long <- victoria$new_long
victoria$lat <- victoria$new_lat
write.csv(victoria[,1:7], file="AECdata/VIC-map.csv", row.names=FALSE)

########
# just checking

polys <- read.csv("VIC-map.csv")

ggplot(nat_map) +
  aes(long, lat, group=group) +
  geom_polygon(aes(fill=STATE)) +
  geom_path(color="black") +
  coord_equal()






