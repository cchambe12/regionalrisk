### 18 January 2018 - Cat
### Starting Analysis - looking at different questions
## 1) FS (y/n) ~ year + (1|species) + site
## 2) FS # ~ species + site + MAT

#### NEED THE REST OF THE MAT DATA!!!

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(raster)
library(reshape2)

### Load data
d<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_yearsitespp.csv", header=TRUE)

d$lat.long<-paste(d$lat, d$long)
d<-d[!duplicated(d$lat.long),]
lats<-d$lat
lons<-d$long
coords<-data.frame(x=lons, y=lats)
coords<-coords[!duplicated(coords),]

r<-brick("/n/wolkovich_lab/Lab/Cat/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")
points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

dclim <- cbind.data.frame(coordinates(points),values)

dx<-melt(dclim, id.vars=c("x","y"))

dx<-dx%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tavg=value)

dx$date<-substr(dx$date, 2,11)
dx$Date<-gsub("[.]","-", dx$date)

dx<-dplyr::select(dx, -date)
dx$year<-substr(dx$Date, 0,4)
dx$month<-substr(dx$Date, 6, 7)
dx$month<-as.numeric(dx$month)

#dx$doy<-yday(dx$Date)
dx$spring<-ifelse(dx$month>=3 & dx$month<=5, "spring", 0)
ddx<-dx[(dx$spring=="spring"),]
ddx<-ddx[!is.na(ddx$Tavg),]

ddx$lat.long<-paste(ddx$lat, ddx$long)
ddx$mst<-ave(ddx$Tavg, ddx$year, ddx$lat.long)
#xdd<-dx%>%dplyr::select(-Tavg, -Date, -spring, -doy)
#xdd<-xdd[!duplicated(xdd),]

spring.one<-ddx%>%dplyr::select(-Tavg, -Date, -spring, -month)
spring.one<-spring.one[!duplicated(spring.one),]

write.csv(spring.one, file="/n/wolkovich_lab/Lab/Cat/mat_MAM.csv", row.names=FALSE)

