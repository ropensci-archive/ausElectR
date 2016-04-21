library(maptools)
library(ggplot2)
xx <- readShapeSpatial("nsw/NSW_electoral_boundaries_25-02-2016.shp")
xxx <- thinnedSpatialPoly(as(xx, "SpatialPolygons"),
                          tolerance=0.1, minarea=0.001, topologyPreserve=TRUE)
class(xxx)
slotNames(xxx)
slotNames(xxx@polygons)

extractPolys <- function(p) {
  polys <- NULL
  for (i in 1:length(p)) {
    for (j in 1:length(p[[i]]@Polygons)) {
      x <- p[[i]]@Polygons[[j]]@coords
      polys$lon <- c(polys$lon, x[,1])
      polys$lat <- c(polys$lat, x[,2])
      polys$ID <- c(polys$ID, rep(p[[i]]@ID, nrow(x)))
      polys$region <- c(polys$region, rep(paste(p[[i]]@ID, j, sep="_"), nrow(x)))
      polys$order <- c(polys$order, 1:nrow(x))
    }
  }
  return(data.frame(polys))
}
polys <- extractPolys(xx@polygons)

# Map theme
theme_map <- theme_bw()
theme_map$line <- element_blank()
theme_map$strip.text <- element_blank()
theme_map$axis.text <- element_blank()
theme_map$plot.title <- element_blank()
theme_map$axis.title <- element_blank()
theme_map$panel.border <- element_rect(colour = "grey90", size=1, fill=NA)

polys <- polys %>% arrange(region, order)

qplot(lon, lat, group=region, data=polys, geom="path") + theme_map
