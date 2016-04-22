library(cartogram) #github.com/chxy/cartogram
library(dplyr)
library(readr)
library(purrr)

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
# Function to extract region
syd <- list(c(151.2, -33.8))
aec_extract_f <- function(aec_data, ctr=c(151.2, -33.8),
                        expand=c(3,4.5), ...) {
  aec_data_sub <- aec_data %>% filter(long_c > ctr[1]-expand[1]  &
                                        long_c < ctr[1]+expand[1] &
                                        lat_c > ctr[2]-expand[2] &
                                        lat_c < ctr[2]+expand[2])
  return(aec_data_sub)
}
aec_data_syd <- aec_extract(aec_data, expand=list(c(2,3)))
ggplot(data=aec_data_syd) +
  geom_point(aes(x=long_c, y=lat_c), size=4, colour="red")

aec_carto_f <-function(aec_data_sub, polygon.vertex=6, name.text=TRUE,
                     dist.ratio=dist.ratio, iteration=100,
                     xlab="", ylab="", ...) {
  #aec_data_sub <- aec_extract(aec_data)
  aec_data_dor <- dorling(aec_data_sub$id, aec_data_sub$long_c,
                          aec_data_sub$lat_c, aec_data_sub$POPULATION,
          polygon.vertex=polygon.vertex,
          name.text=name.text,
          dist.ratio=dist.ratio,
          iteration=iteration,
          xlab=xlab, ylab=ylab)
  return(aec_data_dor)
}
aec_data_dor <- aec_carto(aec_data, expand=list(c(3,4.5)))

# Now a function to pack all together
aec_carto_join_f <- function(aec_data, aec_carto) {
  aec_carto_join <- merge(aec_data, aec_carto, by="id", all=TRUE)

  # Make corto centers of remote districts same as actual lat/long
  aec_carto_join$x[is.na(aec_carto_join$x)] <-
    aec_carto_join$long_c[is.na(aec_carto_join$x)]
  aec_carto_join$y[is.na(aec_carto_join$y)] <-
    aec_carto_join$lat_c[is.na(aec_carto_join$y)]

  return(aec_carto_join)
}
cities <- list(c(151.2, -33.8), # Sydney
               c(153.0, -27.5), # Brisbane
               c(145.0, -37.8), # Melbourne
               c(138.6, -34.9), # Adelaide,
               c(115.9, -32.0)) # Perth
expand <- list(c(2,3), c(2,3), c(2.5,4), c(3,5), c(5,8))
aec_carto <- cities %>%
  purrr::map(aec_data=aec_data, aec_extract_f) %>%
  purrr::map_df(aec_carto_f) %>%
  mutate(region=as.integer(as.character(region))) %>%
  rename(id=region)
aec_carto <- purrr::map2(.x=cities, .y=expand,
                         .f=aec_extract_f, aec_data=aec_data) %>%
  purrr::map_df(aec_carto_f) %>%
  mutate(region=as.integer(as.character(region))) %>%
  rename(id=region)
aec_cart_join <- aec_carto_join_f(aec_data, aec_carto)
ggplot(data=nat_map) +
  geom_polygon(aes(x=long, y=lat, group=group, order=order),
               fill="grey90", colour="white") +
  geom_point(data=aec_cart_join, aes(x=x, y=y), size=2, alpha=0.4,
             colour="#572d2c") +
  geom_text(data=aec_cart_join, aes(x=x, y=y, label=id), size=0.5) +
  theme_map + coord_equal()

# Attach electoral winners data
aes_winners <- read.csv(paste0(getwd(), "/AECdata/HouseMembersElectedDownload-17496.csv"), skip = 1)
aes_winners %>% select(DivisionNm, PartyNm) %>% 
  merge(aec_cart_join, by.x="DivisionNm", by.y="ELECT_DIV") -> cart.winners.large

# Bring different versions of the Liberal Party together
cart.winners.large$PartyNm <- as.character(cart.winners.large$PartyNm)
Liberalversions <- c("Country Liberals (NT)", "Liberal", "Liberal National Party", "The Nationals")
cart.winners.large <- mutate(cart.winners.large, PartyNm = ifelse(as.character(PartyNm) %in% Liberalversions,
                                                      "Liberal National Coalition", PartyNm))

#Colour by elected party, order = Labor, Independent, Katters, Lib/Nats, Palmer, Greens
partycolours = c("#FF0000", "#000000", "#550000", "#0000FF", "#FF9900", "#00FF00")

# Plot it
ggplot(data=nat_map) +
  geom_polygon(aes(x=long, y=lat, group=group, order=order),
               fill="grey90", colour="white") +
  geom_point(data=cart.winners.large, aes(x=x, y=y, colour=PartyNm), size=1, alpha=0.4) +
  theme_map + coord_equal() + scale_colour_manual(name="Politcal Party", values=partycolours)



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

# Attach electoral winners data
#aes_winners <- read.csv(paste0(getwd(), "/AECdata/HouseMembersElectedDownload-17496.csv"), skip = 1)
aes_winners %>% select(DivisionNm, PartyNm) %>% 
  merge(aec_data.cart, by.x="DivisionNm", by.y="ELECT_DIV") -> cart.winners

# Bring different versions of the Liberal Party together
cart.winners$PartyNm <- as.character(cart.winners$PartyNm)
#Liberalversions <- c("Country Liberals (NT)", "Liberal", "Liberal National Party", "The Nationals")
cart.winners <- mutate(cart.winners, PartyNm = ifelse(as.character(PartyNm) %in% Liberalversions,
                                                      "Liberal National Coalition", PartyNm))

#Colour by elected party, order = Labor, Independent, Katters, Lib/Nats, Palmer, Greens
#partycolours = c("#FF0000", "#000000", "#550000", "#0000FF", "#FF9900", "#00FF00")

# Plot it
ggplot(data=nat_map) +
  geom_polygon(aes(x=long, y=lat, group=group, order=order),
               fill="grey90", colour="white") +
  geom_point(data=cart.winners, aes(x=x, y=y, colour=PartyNm), size=1, alpha=0.4) +
  theme_map + coord_equal() + scale_colour_manual(name="Politcal Party", values=partycolours)


# write file
write_csv(aec_data.cart, "AECdata/National-data-dorling.csv")
