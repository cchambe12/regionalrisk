### Figure Out distance from coast stuff
## 13 Sept 2018 - Cat

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(rgeos)
library(rgdal)
library(maptools)
library(geosphere)
library(dplyr)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses")

# Create Europe Map
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coast <- readShapeSpatial("input/natural_earth_vector/10m_physical/ne_10m_coastline.shp",CRS(wgs.84))


bb<-read.csv("output/fs_yearsitespp.csv", header=TRUE)
bb<-subset(bb, select=c("long", "lat"))
bb<-bb%>%rename(LONG=long)%>%rename(LAT=lat)
bb<-bb[!duplicated(bb),]

distances<-dist2Line(bb, coast) ## units=meters
distances<-as.data.frame(distances)
dist<-cbind(bb, distances)

dist<-dist%>%dplyr::select(LAT, LONG, distances)%>%rename(lat=LAT)%>%rename(long=LONG)


write.csv(distances, file="~/Documents/git/regionalrisk/analyses/output/distances.csv", row.names = FALSE)


