### Figure Out distance from coast stuff
## 13 Sept 2018 - Cat

# Load Libraries
options(repos = c(CRAN = "http://cran.rstudio.com"))
library(geosphere)
library(raster)


bb<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_yearsitespp.csv", header=TRUE)
bb<-subset(bb, select=c("long", "lat"))
bb$LONG<-bb$long
bb$LAT<-bb$lat
bb<-subset(bb, select=c("LONG", "LAT"))
bb<-bb[!duplicated(bb),]

coast <- raster::shapefile("/n/wolkovich_lab/Lab/Cat/coastline")

distances<-geosphere::dist2Line(bb, coast) ## units=meters
distances<-as.data.frame(distances)


dist<-cbind(bb, distances)

dist<-subset(dist, select=c("LAT", "LONG", "distance"))
dist$lat<-dist$LAT
dist$long<-dist$LONG
dist<-subset(dist, select=c("lat", "long", "distance"))


write.csv(dist, file="/n/wolkovich_lab/Lab/Cat/distances.csv", row.names = FALSE)

