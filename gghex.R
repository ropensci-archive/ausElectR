
hexFiles <- dir(pattern = "hex\\.txt", full.names = TRUE, recursive = TRUE)
hexDat <- lapply(hexFiles, read.table, header = T)
states <- sub(".*/([a-z]+)hex.txt", "\\1", hexFiles)
hexDat <- Map(function(x, y) cbind(x, state = y), hexDat, states)
hexDat <- dplyr::bind_rows(hexDat)
bumps <- read.csv(text = "
state, xbump, ybump
nsw, 0, 0
qld, -3, 16
vic, -9, -10
sa, -18, -6
tas, -6, -25
nt, -26, 33
wa, -40, 0"
)
hexDat <- dplyr::left_join(hexDat, bumps, by = "state")
hexDat$x <- hexDat$x + hexDat$xbump
hexDat$y <- hexDat$y + hexDat$ybump
hexDat$xcent <- with(hexDat, x*sqrt(3) - (y%%2) * sqrt(3)/2)
hexDat$ycent <- with(hexDat, y*1.5)

library(plotly)
p <- ggplot(hexDat, aes(xcent, ycent, text = electorate)) + 
  geom_hex(stat = "identity") + 
  lims(x=c(-80, 8), y=c(-40, 50))
ggplotly(p, tooltip = "text")

save(hexDat, file = "echidnaR/data/hexDat.rda")

#boundaries <- read.table("hexdata/roughedges.txt", header = TRUE)
#
#ozresults <- read.table(
#  "AECdata/HouseMembersElectedDownload-17496.csv", 
#  header=TRUE, skip=1, sep=",", quote="\""
#)
#
#cols <- data.frame(
#  PartyAb = c("ALP","LP" ,"LNP", "NP" , "IND","PUP", "KAP", "GRN", "CLP"), 
#  fill = c("red","blue","blue","forestgreen","gray","yellow","darkred","green","orange"),
#  border = c(NA,NA,"yellow","yellow",NA,NA,NA,"green","blue")
#)
#ozresults <- merge(ozresults, cols, by = "PartyAb")
#
#hex_x <- c(0, sqrt(3)/2, sqrt(3)/2, 0, -sqrt(3)/2, -sqrt(3)/2, 0)
#hex_y <- c(1, 0.5, -0.5, -1, -0.5, 0.5, 1)
#
#
#library(ggplot2)
#ggplot(hexDat) + geom_polygon(group = state)
#
#polygon(hex_x+xcent[i],hex_y+ycent[i],col=fill[i],border=border[i],...)
#if (text) text(xcent[i],ycent[i],id[i],cex=0.4)
#
#idx <- match(nsw$electorate, ozresults$DivisionNm)
#with(nsw, hexit(x,y, electorate, 
#                fill=as.character(ozresults$fill[idx]), 
#                border=as.character(ozresults$border[idx]),lwd=2)
#)