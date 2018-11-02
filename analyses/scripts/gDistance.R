#### 20 Sept 2018 - Cat
## using gDistance as it's faster and accurate as well

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(rgeos)
library(rgdal)
library(raster)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses")

# Create Europe Map


#shapefile(coast, "output/coastline.shp")

bb<-read.csv("output/fs_yearsitespp.csv", header=TRUE)
bb<-subset(bb, select=c("long", "lat"))
bb$LONG<-bb$long
bb$LAT<-bb$lat
bb<-subset(bb, select=c("LONG", "LAT"))
bb<-bb[!duplicated(bb),]
b1<-bb[1:2000,]
b2<-bb[2001:4500,]
b3<-bb[4501:7000,]
b4<-bb[7001:9500,]
b5<-bb[9501:11648,]

wgs84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
utm10n<-CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")

coast <- shapefile("input/natural_earth_vector/10m_physical/ne_10m_coastline.shp")

foo<-broom::tidy(coast)

bc<-SpatialPointsDataFrame(data.frame(x=b5$LONG,y =b5$LAT),data=data.frame(b5),proj4string = wgs84)

#  Project into something - Decimal degrees are no fun to work with when measuring distance!
#bcProj<-spTransform(bc,utm10n)

#creating example construction data layer:
con.coords <- subset(foo, select=c("long", "lat"))
con<-SpatialPointsDataFrame(data.frame(x=con.coords$long,y =con.coords$lat),data=con.coords,proj4string = wgs84)
#projection(con) <- wgs84

#All at once (black carbon points on top, construction on the y-axis)
dist2<-apply(gDistance(bc, con,byid=TRUE),2,min) ## then need to multiply by 100 to get km
b2$distance<- dist2
b2$distkm<- b2$distance*111

dist1<-apply(gDistance(bc, con,byid=TRUE),2,min) 
b1$distance<- dist1
b1$distkm<- b1$distance*111

dist3<-apply(gDistance(bc, con,byid=TRUE),2,min) 
b3$distance<- dist3
b3$distkm<- b3$distance*111

dist4<-apply(gDistance(bc, con,byid=TRUE),2,min) 
b4$distance<- dist4
b4$distkm<- b4$distance*111

dist5<-apply(gDistance(bc, con,byid=TRUE),2,min) 
b5$distance<- dist5
b5$distkm<- b5$distance*111

distances <- rbind(b1, b2, b3, b4, b5)
write.csv(distances, file = "~/Documents/git/regionalrisk/analyses/output/dist_wgs.csv",row.names = FALSE)



dis.test<-subset(distances, distances$distkm<12500)
dis.far<-subset(distances, distances$distkm>14500)
distances$disty<-distances$distkm/10000

ggplot(dis.far, aes(x=LONG, y=LAT, fill=distkm)) + geom_point(aes(fill=distkm))

ggplot(distances, aes(x=LONG, y=LAT, fill=disty)) + geom_point(aes(fill=disty))

