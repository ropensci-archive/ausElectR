
hexFiles <- dir(pattern = "hex\\.txt", full.names = TRUE, recursive = TRUE)
hexDat <- lapply(hexFiles, read.table, header = T)
states <- sub(".*/([a-z]+)hex.txt", "\\1", hexFiles)
hexDat <- Map(function(x, y) cbind(x, state = y), hexDat, states)
hexDat <- dplyr::bind_rows(hexDat)
names(hexDat) <- c("x", "y", "Electorate", "state")
bumps <- read.csv(text = "
state, xbump, ybump
nsw, 0, 0
qld, -2, 16
vic, -9, -13
sa, -18, -6
tas, -6, -28
nt, -24, 41
wa, -40, 0"
)
hexDat <- dplyr::left_join(hexDat, bumps, by = "state")
hexDat$x <- hexDat$x + hexDat$xbump
hexDat$y <- hexDat$y + hexDat$ybump
hexDat$xcent <- with(hexDat, x*sqrt(3) - (y%%2) * sqrt(3)/2)
hexDat$ycent <- with(hexDat, y*1.5)

edges <- read.table("hexdata/roughedges.txt", header = T)
bb <- read.table("hexdata/oz_outline.txt", header = T)

library(plotly)
p <- ggplot(hexDat, aes(xcent, ycent, text = Electorate)) + 
  geom_hex(stat = "identity") + 
  geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1), 
               data = edges, inherit.aes = FALSE, linetype = "dashed") +
  geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1), 
               data = bb, inherit.aes = FALSE, linetype = "dotted") +
  lims(x = c(-80, 8), y = c(-46, 75))
ggplotly(pp, tooltip = "text")

save(hexDat, file = "echidnaR/data/hexDat.rda")
