# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(raster)
library(reshape2)
library(data.table)

dxx<-read.csv("/n/wolkovich_lab/Lab/Cat/allspp_climateprep_fullleaf.csv", header=TRUE)
#dxx<-read.csv("~/Desktop/allspp_climateprep.csv", header=TRUE)

r<-brick("/n/wolkovich_lab/Lab/Cat/tn_0.25deg_reg_v16.0.nc", varname="tn", sep="")
#r<-brick("~/Desktop/tn_0.25deg_reg_v16.0.nc", varname="tn", sep="")

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

dx<-reshape2::melt(dclim, id.vars=c("x","y"))

dx<-dx%>%
  dplyr::rename(long=x)%>%
  dplyr::rename(lat=y)%>%
  dplyr::rename(date=variable)%>%
  dplyr::rename(Tmin=value)

dx$date<-substr(dx$date, 2,11)
dx$Date<- gsub("[.]", "-", dx$date)

dxx<-dplyr::select(dxx, -date)
dx<-dplyr::select(dx, -date)

x<-inner_join(dx, dxx, by=c("Date", "lat", "long"))

write.csv(x, file="/n/wolkovich_lab/Lab/Cat/allspp_data.csv", row.names=FALSE)
#write.csv(x, file="~/Desktop/allspp_data.csv", row.names=FALSE)

x$fs<- ifelse(x$Tmin<=-2.2, 1, 0)
x$bb<-ave(x$doy, x$PEP_ID, x$year, x$species, FUN=first)
x$lo<-ave(x$doy, x$PEP_ID, x$year, x$species, FUN=last) # Based on Danf's ACESAC - most closely related. Choose WL0 based on keeping all species consistent
x<-x[!(x$doy<x$bb),]
x<-x[!(x$doy>x$lo),]
x<-x[!duplicated(x),]
x$fs.count<- ave(x$fs, x$PEP_ID, x$year, x$species, FUN=sum)
allspp<-x%>%dplyr::select(lat, long, PEP_ID, fs.count, year, species)
allspp<-allspp[!duplicated(allspp),]
allspp<-na.omit(allspp)
allspp$fs<-ifelse(allspp$fs.count>=1, 1, 0)

write.csv(allspp, file="/n/wolkovich_lab/Lab/Cat/fs_allspp_fullleaf.csv", row.names = FALSE)
#write.csv(allspp, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_original.csv", row.names = FALSE)
