# Extract Climate Data more efficiently
# 22 August 2017 - Cat

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(Interpol.T)
library(chillR)
library(raster)

eur.tempmn <- nc_open("tn_0.25deg_reg_v15.0.nc")
r<-brick("tn_0.25deg_reg_v15.0.nc", varname="tn", sep="")
bb<-read.csv("~/Documents/git/regionalrisk/analyses/output/bbch_region_aesculus.csv", header=TRUE)

names(r)<-c("x", "y", "value", "name")

location<-as.numeric(c(bb$LAT), bb$LON)

r<-r[12]

df<- apply(X = bb, 1, function(x, r) {
  ex <- extract(r, location)
}, r = r)

lats <- bb$LAT 
lons <- bb$LON

coords <- data.frame(x=lons,y=lats)

r[is.na(r)]<-0
points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

df <- cbind.data.frame(coordinates(points),values)

r[] = 1:ncell(r)
df<-as.data.frame(r)