### Figure Out distance from coast stuff
## 13 Sept 2018 - Cat

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(maptools)
library(geosphere)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses")

# Create Europe Map
coast <- readShapeSpatial("input/natural_earth_vector/10m_physical/ne_10m_coastline")

#shapefile(coast, "output/coastline.shp")

bb<-read.csv("output/fs_yearsitespp.csv", header=TRUE)
bb<-subset(bb, select=c("long", "lat"))
bb$LONG<-bb$long
bb$LAT<-bb$lat
bb<-subset(bb, select=c("LONG", "LAT"))
bb<-bb[!duplicated(bb),]
#b1<-bb[1:200,]
#b2<-bb[201:800,]
#b3<-bb[801:1400,]
#b4<-bb[1401:2000,]
#b5<-bb[2001:2600,]
b6<-bb[2601:3800,] 
b7<-bb[3801:8400,]
b8<-bb[4401:5000,]
b9<-bb[5001:5600,]
b10<-bb[5601:6200,]
b11<-bb[6201:6800,]
b12<-bb[6801:7400,]
b13<-bb[7401:8000,]
b14<-bb[8001:8600,]
b15<-bb[8601:9200,]
b16<-bb[9201:9800,]
b17<-bb[9801:11648,]


#dist1<-dist2Line(b1, coast) ## units=meters
#dist1<-as.data.frame(dist1)
#dist1<-cbind(b1, dist1)
  
#dist2<-dist2Line(b2, coast) ## units=meters
#dist2<-as.data.frame(dist2)
#dist2<-cbind(b2, dist2)

#dist3<-dist2Line(b3, coast) ## units=meters
#dist3<-as.data.frame(dist3)
#dist3<-cbind(b3, dist3)

#dist4<-dist2Line(b4, coast) ## units=meters
#dist4<-as.data.frame(dist4)
#dist4<-cbind(b4, dist4)

#dist5<-dist2Line(b5, coast) ## units=meters
#dist5<-as.data.frame(dist5)
#dist5<-cbind(b5, dist5)

dist6<-dist2Line(b6, coast) ## units=meters
dist6<-as.data.frame(dist6)
dist6<-cbind(b6, dist6)

dist7<-dist2Line(b7, coast) ## units=meters
dist7<-as.data.frame(dist7)
dist7<-cbind(b7, dist7)

dist8<-dist2Line(b8, coast) ## units=meters
dist8<-as.data.frame(dist8)
dist8<-cbind(b8, dist8)

dist9<-dist2Line(b9, coast) ## units=meters
dist9<-as.data.frame(dist9)
dist9<-cbind(b9, dist9)

dist10<-dist2Line(b10, coast) ## units=meters
dist10<-as.data.frame(dist10)
dist10<-cbind(b10, dist10)

dist11<-dist2Line(b11, coast) ## units=meters
dist11<-as.data.frame(dist11)
dist11<-cbind(b11, dist11)

dist12<-dist2Line(b12, coast) ## units=meters
dist12<-as.data.frame(dist12)
dist12<-cbind(b12, dist12)

dist13<-dist2Line(b13, coast) ## units=meters
dist13<-as.data.frame(dist13)
dist13<-cbind(b13, dist13)

dist14<-dist2Line(b14, coast) ## units=meters
dist14<-as.data.frame(dist14)
dist14<-cbind(b14, dist14)

dist15<-dist2Line(b15, coast) ## units=meters
dist15<-as.data.frame(dist15)
dist15<-cbind(b15, dist15)

dist16<-dist2Line(b16, coast) ## units=meters
dist16<-as.data.frame(dist16)
dist16<-cbind(b16, dist16)

dist17<-dist2Line(b17, coast) ## units=meters
dist17<-as.data.frame(dist17)
dist17<-cbind(b17, dist17)

dist<-rbind(dist1, dist2, dist3, dist4, dist5) #, dist6, dist7, dist8, dist9, dist10,
            #dist11, dist12, dist13, dist14, dist15, dist16, dist17)

dist<-dist%>%dplyr::select(LAT, LONG, distance)%>%rename(lat=LAT)%>%rename(long=LONG)


write.csv(dist, file="~/Documents/git/regionalrisk/analyses/output/distances1.csv", row.names = FALSE)


