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
library(tidyr)
library(ggplot2)
#library(rstanarm)
#library(rstan)
#library(bayesplot)
#library(shinystan)


library(lubridate)
library(ncdf4)
library(Interpol.T)
library(chillR)
library(raster)
library(reshape2)
library(data.table)

### Load data
setwd("~/Documents/git/regionalrisk/analyses/output")
d<-read.csv("fs_yearsitespp.csv", header=TRUE)
#dx<-read.csv("mat.csv", header=TRUE)
#df<-d%>%filter(year>=2008)
#xdf<-d%>%filter(year<2008)
#xdf$lat.long<-paste(xdf$lat, xdf$long)
#xdf<-xdf[!duplicated(xdf$lat.long),]
#xlat<-subset(xdf, !(xdf$lat%in%df$lat))
#xlats<-xlat$lat
#xlon<-subset(xdf, !(xdf$long%in%df$long))
#xlons<-xlon$long
#coords<-data.frame(x=lons, y=lats)
#points<-SpatialPoints(coords, proj4string = r@crs)

#values<-extract(r, points)

### Clean the weather data
setwd("~/Desktop/")
r<-brick("~/Desktop/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")

df<-d
df$lat.long<-paste(df$lat, d$flong, sep=",")
df<-df[!duplicated(df),]
lats<-df$lat
lons<-df$long
coords<-data.frame(x=lons, y=lats)
coords<-coords[!duplicated(coords),]
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
dx$lat.long<-paste(dx$lat, dx$long)
dx<-dx[!duplicated(dx),]
dx$mat<-ave(dx$Tavg, dx$year, dx$lat.long)
dx$doy<-yday(dx$Date)
dx$spring<-ifelse(dx$doy>=1 & dx$doy<=90, "spring", 0)
ddx<-dx[(dx$spring=="spring"),]
ddx<-ddx[!is.na(ddx$Tavg),]
ddx$pre.bb<-ave(ddx$Tavg, ddx$year, ddx$lat.long)
#xdd<-dx%>%dplyr::select(-Tavg, -Date, -spring, -doy)
#xdd<-xdd[!duplicated(xdd),]

spring<-ddx%>%dplyr::select(-Tavg, -Date, -spring, -doy)
spring<-spring[!duplicated(spring),]


write.csv(xdd, file="~/Documents/git/regionalrisk/analyses/output/mat_compressed.csv", row.names=FALSE)
write.csv(spring, file="~/Documents/git/regionalrisk/analyses/output/mat_spring.csv", row.names=FALSE)
