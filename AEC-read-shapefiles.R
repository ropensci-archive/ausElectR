library(maptools)
library(ggplot2)

shapeFile <- "shapefiles/Shape (ESRI)/WA_Electoral_Boundaries_19-01-2016.shp"
shapeFile <- "shapefiles/act-esri-28012016/ACT_Electoral_Boundaries_28-01-2016.shp"
shapeFile <- "shapefiles/qld-shape-files-13012010/QLD_ELB_031209_region.shp"
shapeFile <- "shapefiles/sa-esri-16122011/E_SA16122011_region.shp"
shapeFile <- "shapefiles/vic-esri-24122010/vic 24122010.shp"
shapeFile <- "shapefiles/nsw-esri-06042016/NSW_electoral_boundaries_25-02-2016.shp"
xx <- readShapeSpatial(shapeFile)
xxx <- thinnedSpatialPoly(as(xx, "SpatialPolygons"),
                          tolerance=0.05, minarea=0.001, topologyPreserve=TRUE)

plot(xx)
plot(xxx)
# adjust names here
write.csv(xx@data, "NSW-data.csv", row.names=FALSE)
write.csv(fortify(xx), "NSW-map.csv", row.names=FALSE)

########
# just checking

polys <- read.csv("VIC-map.csv")

ggplot(polys) +
  aes(long, lat, group=group) +
  geom_polygon(fill="grey80") +
  geom_path(color="black") +
  coord_equal()



