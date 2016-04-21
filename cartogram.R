library(cartogram) #github.com/chxy/cartogram
library(dplyr)
library(readr)

# Read data
#aec_polys <- read_csv("AECdata/National-map.csv")
aec_data <- read_csv("AECdata/National-data.csv")
load("AECdata/nat_map.rda")

# Show electoral districts
# aec_polys <- aec_polys %>% group_by(id) %>% arrange(order)
ggplot(data=nat_map) + geom_path(aes(x=long, y=lat, group=group, order=order)) +
  theme_map + coord_equal() #+
  #ylim(c(-32.0, -34.5)) + xlim(c(150, 152))
  #ylim(c(-39.1, -37.0)) + xlim(c(143.5, 146))
  #ylim(c(-29.0, -26.0)) + xlim(c(151.5, 154))
  #ylim(c(-33.0, -30.0)) + xlim(c(115, 117))
  #ylim(c(-35.5, -34.5)) + xlim(c(138, 139))

aec_data$POPULATION <- 100
polys.cart <- dorling(aec_data$id, aec_data$long_c,
                      aec_data$lat_c, aec_data$POPULATION,
                      polygon.vertex=6, name.text=TRUE,
                      dist.ratio=5, iteration=100, xlab='', ylab='')

# Extract Sydney, Melbourne, Brisbane, Perth, Adelaide
aec_data.syd <- aec_data %>% filter(long_c > 150 & long_c < 152 &
                                        lat_c < (-32.0) & lat_c > (-34.5))
aec_data.mel <- aec_data %>% filter(long_c > 143.5 & long_c < 146 &
                                      lat_c < (-37.0) & lat_c > (-39.1))
aec_data.bne <- aec_data %>% filter(long_c > 151.5 & long_c < 154 &
                                      lat_c < (-26.0) & lat_c > (-29.0))
aec_data.per <- aec_data %>% filter(long_c > 115 & long_c < 117 &
                                      lat_c < (-30.0) & lat_c > (-33.0))
aec_data.ade <- aec_data %>% filter(long_c > 138 & long_c < 139 &
                                      lat_c < (-34.5) & lat_c > (-35.5))

# Make cartograms
aec_data.syd.cart <- dorling(aec_data.syd$id, aec_data.syd$long_c,
                             aec_data.syd$lat_c, aec_data.syd$POPULATION,
                             polygon.vertex=6, name.text=TRUE,
                             dist.ratio=5, iteration=100, xlab='', ylab='')
aec_data.mel.cart <- dorling(aec_data.mel$id, aec_data.mel$long_c,
                             aec_data.mel$lat_c, aec_data.mel$POPULATION,
                             polygon.vertex=6, name.text=TRUE,
                             dist.ratio=5, iteration=100, xlab='', ylab='')
aec_data.bne.cart <- dorling(aec_data.bne$id, aec_data.bne$long_c,
                             aec_data.bne$lat_c, aec_data.bne$POPULATION,
                             polygon.vertex=6, name.text=TRUE,
                             dist.ratio=5, iteration=100, xlab='', ylab='')
aec_data.per.cart <- dorling(aec_data.per$id, aec_data.per$long_c,
                             aec_data.per$lat_c, aec_data.per$POPULATION,
                             polygon.vertex=6, name.text=TRUE,
                             dist.ratio=5, iteration=100, xlab='', ylab='')
aec_data.ade.cart <- dorling(aec_data.ade$id, aec_data.ade$long_c,
                             aec_data.ade$lat_c, aec_data.ade$POPULATION,
                             polygon.vertex=6, name.text=TRUE,
                             dist.ratio=5, iteration=100, xlab='', ylab='')

# Now put them all together
aec_data.cart <- rbind(aec_data.syd.cart, aec_data.mel.cart, aec_data.bne.cart,
                       aec_data.per.cart, aec_data.ade.cart)
aec_data.cart$region <- as.numeric(as.character(aec_data.cart$region))
aec_data.cart <- aec_data.cart %>% rename(id=region)

aec_data.cart <- merge(aec_data, aec_data.cart, by="id", all=T)
aec_data.cart$x[is.na(aec_data.cart$x)] <- aec_data.cart$long_c[is.na(aec_data.cart$x)]
aec_data.cart$y[is.na(aec_data.cart$y)] <- aec_data.cart$lat_c[is.na(aec_data.cart$y)]
aec_data.cart$radius[is.na(aec_data.cart$radius)] <- 0.06690128

# Plot it
ggplot(data=nat_map) +
  geom_polygon(aes(x=long, y=lat, group=group, order=order),
               fill="grey90", colour="white") +
  geom_point(data=aec_data.cart, aes(x=x, y=y), size=1, alpha=0.4,
             colour="#572d2c") +
  theme_map + coord_equal()

# write file
write_csv(aec_data.cart, "AECdata/National-data-dorling.csv")
