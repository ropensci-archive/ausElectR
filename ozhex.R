nsw <-read.table("hexdata/nswhex.txt",header=TRUE)
qld <-read.table("hexdata/qldhex.txt",header=TRUE)
sa <-read.table("hexdata/sahex.txt",header=TRUE)
wa <-read.table("hexdata/wahex.txt",header=TRUE)
tas <-read.table("hexdata/tashex.txt",header=TRUE)
vic <-read.table("hexdata/vichex.txt",header=TRUE)
nt <-read.table("hexdata/nthex.txt",header=TRUE)


## constants for describing a hex
hex_x<-c(0, 0.866025403784439, 0.866025403784439, 0, -0.866025403784439, 
-0.866025403784439, 0)
hex_y<-c(1, 0.5, -0.5, -1, -0.5, 0.5, 1)


## simple drawing function
hexit<-function(x,y,id,..., text=FALSE){
	xcent<-x*sqrt(3) - (y%%2) *sqrt(3)/2
	ycent<-y*1.5
	
	for(i in 1:length(x)){
		polygon(hex_x+xcent[i],hex_y+ycent[i],...)
		if (text) text(xcent[i],ycent[i],id[i],cex=0.4)
	}
	
	
}

## Each state is centered at its capital city
## The state offsets may not be quite right, especially NT and WA

plot(0,0, ylim=c(-40, 50), xlim=c(-80, 8),xlab="",ylab="")
with(nsw, hexit(x,y, electorate))
with(qld, hexit(x-3,y+16,electorate,col="skyblue"))
with(vic, hexit(x-9, y-10, electorate,col="yellow"))
with(sa, hexit(x-18, y-6, electorate, col="gray90"))
with(tas, hexit(x-6, y-25, electorate))
with(nt, hexit(x-26, y+33, electorate,col="lightpink"))
with(wa, hexit(x-40, y, electorate,col="lightgreen"))

## approx state boundaries -- by hand, from memory
edges <- read.table("hexdata/roughedges.txt", header=TRUE)

do.call(segments, c(edges,lty=2)


