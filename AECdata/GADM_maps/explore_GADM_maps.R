
library(sp)
zero <- readRDS("AUS_adm0.rds")
one <- readRDS("AUS_adm1.rds")
two <- readRDS("AUS_adm2.rds")

plot(zero)
plot(one)
plot(two)


str(two)
names(two)
head(two@data)
