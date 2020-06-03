# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(raster)
library(reshape2)
library(data.table)

dxx<-read.csv("/n/wolkovich_lab/Lab/Cat/allspp_climateprep_dvr.csv", header=TRUE)

r<-brick("/n/wolkovich_lab/Lab/Cat/tn_0.25deg_reg_v16.0.nc", varname="tn", sep="")

bb<-dxx
bb$lat.long<-paste(bb$lat, bb$long, sep=",")
bb<-bb[!duplicated(bb$lat.long),]
lats <- bb$lat
lons <- bb$long

coords <- data.frame(x=lons,y=lats)

coords<- na.omit(coords)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

dclim <- cbind.data.frame(coordinates(points),values)

dx<-melt(dclim, id.vars=c("x","y"))

dx<-dx%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tmin=value)

dx$date<-substr(dx$date, 2,11)
dx$Date<- gsub("[.]", "-", dx$date)

dxx<-dplyr::select(dxx, -date)
dx<-dplyr::select(dx, -date)

x<-inner_join(dx, dxx, by=c("Date", "lat", "long"))

write.csv(x, file="/n/wolkovich_lab/Lab/Cat/allspp_data_dvr.csv", row.names=FALSE)
#x<-read.csv("~/Documents/git/regionalrisk/analyses/output/allspp_data_dvr.csv")

x$fs<- ifelse(x$Tmin<=-2.2, 1, 0)
x$lo<-ave(x$doy, x$PEP_ID, x$year, x$species, FUN=last)
x$bb<-NA
x$bb<-ifelse(x$species=="AESHIP", x$lo-11, x$bb)
x$bb<-ifelse(x$species=="ALNGLU", x$lo-12, x$bb)
x$bb<-ifelse(x$species=="BETPEN", x$lo-11, x$bb) #was 15; used Danf's WL1 and CS0 data average for all Betula species
x$bb<-ifelse(x$species=="FAGSYL", x$lo-5, x$bb)
x$bb<-ifelse(x$species=="FRAEXC", x$lo-7, x$bb)
x$bb<-ifelse(x$species=="QUEROB", x$lo-7, x$bb)
x<-x[!(x$doy<x$bb),]
x<-x[!duplicated(x),]
x$fs.count<- ave(x$fs, x$PEP_ID, x$year, x$species, FUN=sum)
allspp<-x%>%dplyr::select(lat, long, PEP_ID, fs.count, year, species)
allspp<-allspp[!duplicated(allspp),]
allspp<-na.omit(allspp)
allspp$fs<-ifelse(allspp$fs.count>=1, 1, 0)

write.csv(allspp, file="/n/wolkovich_lab/Lab/Cat/fs_allspp_dvr.csv", row.names = FALSE)
