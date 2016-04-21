nat_map <- read.csv("AECdata/National-map.csv")
nat_data <- read.csv("AECdata/National-data.csv")

library(ggthemes)
ggplot(aes(fill=AREA_SQKM), data=nat_data) +
  geom_map(aes(map_id=id), map=nat_map) +
  expand_limits(x=nat_map$long, y=nat_map$lat) + 
  theme_map()
