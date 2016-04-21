# data first
files <- dir("AECdata/", pattern="-data")

library(purrr)
all <- file.path(dir="AECdata", files) %>% map_df(read_csv, stringsasfactors=FALSE)
write.csv(all, file="AEC-data.csv", row.names=FALSE)


# polygons second

files <- dir("AECdata/", pattern="-map")
for (ff in seq_along(files)) {
  cat(files[ff])
    polys <- read.csv(file.path("AECdata", files[ff]))
    print(qplot(data=polys, geom="polygon", x=long, y=lat, group=group))
    ch <- scan()
}

all <- NULL
states <- gsub("(.*)-.*", "\\1", files)
for (ff in seq_along(files)) {
  temp <-read.csv(file.path(dir="AECdata", files[ff]))
  temp$State <- states[ff]
  all <- rbind(all, temp)  
}

all$state.group <- with(all, paste(State, group, sep="."))
qplot(data=subset(all, State!="VIC"), geom="polygon", x=long, y=lat, group=factor(state.group))
qplot(data=all, geom="polygon", x=long, y=lat, group=factor(group))

write.csv(all, "AEC-map.csv", row.names=FALSE)
