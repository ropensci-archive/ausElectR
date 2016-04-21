library(cartogram) #github.com/chxy/cartogram
library(dplyr)
library(readr)
library(maptools)
library(ggplot2)

# This is using the old data so far
xx <- readShapeSpatial("nsw/NSW_electoral_boundaries_25-02-2016.shp")

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
      polys$area <- c(polys$area, rep(p[[i]]@area, nrow(x)))
    }
  }
  return(data.frame(polys))
}
polys <- extractPolys(xx@polygons)
polys <- polys %>% filter(lon < 155)

# Map theme
theme_map <- theme_bw()
theme_map$line <- element_blank()
theme_map$strip.text <- element_blank()
theme_map$axis.text <- element_blank()
theme_map$plot.title <- element_blank()
theme_map$axis.title <- element_blank()
theme_map$panel.border <- element_rect(colour = "grey90", size=1, fill=NA)

polys <- polys %>% arrange(region, order)
polys.ctr$area <- sample(95:100, 47, replace=TRUE)

polys.ctr <- polys %>% group_by(ID) %>%
  summarise(lon.c = median(lon, na.rm=T),
            lat.c = median(lat, na.rm=T))

# Only look at Sydney
polys.ctr.syd <- polys.ctr %>% filter(lon.c > 150 & lon.c < 152 &
                                        lat.c < (-32.0) & lat.c > (-34.5))

polys.cart <- dorling(polys.ctr.syd$ID, polys.ctr.syd$lon.c,
                      polys.ctr.syd$lat.c, polys.ctr.syd$area,
                      polygon.vertex=6, name.text=TRUE,
                      dist.ratio=5, iteration=100, xlab='', ylab='')

polys.ctr.cart <- merge(polys.ctr.syd, polys.cart, by.x="ID", by.y="region")

ggplot(data=polys) + geom_path(aes(x=lon, y=lat, group=region)) +
  geom_point(data=polys.ctr.cart, aes(x=x, y=y), size=9, alpha=0.4) +
  geom_segment(data=polys.ctr.cart, aes (x=x, xend=lon.c, y=y, yend=lat.c)) +
  theme_map + coord_equal() + theme(legend.position="none") +
  ylim(c(-32.0, -34.5)) + xlim(c(150, 152))

