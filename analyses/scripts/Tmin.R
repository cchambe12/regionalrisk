## Build a new script to get Tmin values for Regional Risk using Odyssey
## 4 September 2018 - Cat

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(raster)
library(reshape2)

d<-read.csv("/n/wolkovich_lab/Lab/Cat/regrisk.cleaned.2.csv", header=TRUE)

d<-d[!duplicated(d$lat.long),]
lats<-d$lat
lons<-d$long
coords<-data.frame(x=lons, y=lats)
coords<-coords[!duplicated(coords),]

r<-brick("/n/wolkovich_lab/Lab/Cat/tn_0.25deg_reg_v16.0.nc", varname="tn", sep="")
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

write.csv(dx, file="/n/wolkovich_lab/Lab/Cat/tmin.csv", row.names = FALSE)


