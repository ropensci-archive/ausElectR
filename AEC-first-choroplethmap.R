nat_map <- read.csv("AECdata/National-map.csv")
nat_data <- read.csv("AECdata/National-data.csv")

library(ggthemes)
ggplot(aes(map_id=id), data=nat_data) +
  geom_map(aes(fill=AREA_SQKM), map=nat_map) +
  expand_limits(x=nat_map$long, y=nat_map$lat) + 
  theme_map()

# this doesn't work until we rename ELECT_DIV as region
ggplot(aes(map_id=ELECT_DIV), data=nat_data) +
  geom_map(aes(fill=AREA_SQKM), map=nat_map) +
  expand_limits(x=nat_map$long, y=nat_map$lat) + 
  theme_map()

# get abs2011 data from running ReadABS.R
nat_map$region <- nat_map$ELECT_DIV
abs2011$region <- abs2011$Name
both <- intersect(nat_map$Name, abs2011$Name)

ggplot(aes(map_id=region), data=subset(abs2011, Name %in% both)) +
   geom_map(aes(fill=Population), map=subset(nat_map, Name %in% both)) +
   expand_limits(x=nat_map$long, y=nat_map$lat) + 
   theme_map()

save(nat_map, file="AECdata/nat_map.rda")
####
# alternative solutions: merge the two data sets, then use polygon and filling colour:

mapmerge <- merge(nat_map, abs2011, by="Name")
mapmerge <- mapmerge[order(mapmerge$order),]

ggplot(data=mapmerge, aes(x=long, y=lat, group=group, fill=Population)) + geom_polygon()
