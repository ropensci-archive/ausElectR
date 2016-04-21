library(maptools)
library(ggplot2)

shapeFile <- "national-esri-16122011/COM20111216_ELB_region.shp"

shapeFile <- "shapefiles/Shape (ESRI)/WA_Electoral_Boundaries_19-01-2016.shp"
shapeFile <- "shapefiles/act-esri-28012016/ACT_Electoral_Boundaries_28-01-2016.shp"
shapeFile <- "shapefiles/qld-shape-files-13012010/QLD_ELB_031209_region.shp"
shapeFile <- "shapefiles/sa-esri-16122011/E_SA16122011_region.shp"
shapeFile <- "shapefiles/vic-esri-24122010/vic 24122010.shp"
shapeFile <- "shapefiles/nsw-esri-06042016/NSW_electoral_boundaries_25-02-2016.shp"
xx <- readShapeSpatial(shapeFile)
#xxx <- thinnedSpatialPoly(as(xx, "SpatialPolygons"),
#                          tolerance=0.05, minarea=0.001, topologyPreserve=TRUE)

devtools::install_github("ateucher/rmapshaper")
library(rmapshaper)
?ms_simplify

xxx <- ms_simplify(xx) # use instead of thinnedSpatialPoly

plot(xx)
plot(xxx)
# adjust names here
write.csv(xx@data, "National-data.csv", row.names=FALSE)
write.csv(fortify(xxx), "National-map.csv", row.names=FALSE)


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

ggplot(victoria) +
  aes(new_long, new_lat, group=group) +
  geom_polygon(fill="grey80") +
  geom_path(color="black") +
  coord_equal()



