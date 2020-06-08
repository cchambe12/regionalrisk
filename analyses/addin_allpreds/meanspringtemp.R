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
d<-read.csv("fs_allspp_long.csv", header=TRUE)
#dx<-read.csv("mat.csv", header=TRUE)

d<-d[!duplicated(d$lat.long),]
lats<-d$lat
lons<-d$long
one.lat<-lats[1:5850]
one.lon<-lons[1:5850]
coords<-data.frame(x=one.lon, y=one.lat)
coords<-coords[!duplicated(coords),]

setwd("~/Desktop/")
r<-brick("~/Desktop/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")
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
dx$spring<-ifelse(dx$month>=2 & dx$month<=4, "spring", 0)
ddx<-dx[(dx$spring=="spring"),]
ddx<-ddx[!is.na(ddx$Tavg),]

ddx$lat.long<-paste(ddx$lat, ddx$long)
ddx$pre.bb<-ave(ddx$Tavg, ddx$year, ddx$lat.long)
#xdd<-dx%>%dplyr::select(-Tavg, -Date, -spring, -doy)
#xdd<-xdd[!duplicated(xdd),]

spring.one<-ddx%>%dplyr::select(-Tavg, -Date, -spring, -month)
spring.one<-spring.one[!duplicated(spring.one),]

write.csv(spring.one, file="~/Desktop/safetynet.csv", row.names=FALSE)

##############################################################################################################################

lats<-d$lat
lons<-d$long
two.lat<-lats[5851:11684]
two.lon<-lons[5851:11684]
coords<-data.frame(x=two.lon, y=two.lat)
coords<-coords[!duplicated(coords),]

#setwd("~/Desktop/")
#r<-brick("~/Desktop/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")
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
dx$month<-as.numeric(substr(dx$Date, 6, 7))

dx$spring<-ifelse(dx$month>=2 & dx$month<=4, "spring", 0)
ddx<-dx[(dx$spring=="spring"),]
ddx<-ddx[!is.na(ddx$Tavg),]


ddx$lat.long<-paste(ddx$lat, ddx$long)

ddx$pre.bb<-ave(ddx$Tavg, ddx$year, ddx$lat.long)
#xdd<-dx%>%dplyr::select(-Tavg, -Date, -spring, -doy)
#xdd<-xdd[!duplicated(xdd),]

spring.two<-ddx%>%dplyr::select(-Tavg, -Date, -spring, -month)
spring.two<-spring.two[!duplicated(spring.two),]

bb<-full_join(spring.one, spring.two)
bb<-bb[!duplicated(bb),]

write.csv(bb, file="~/Documents/git/regionalrisk/analyses/output/mat_fulldata.csv", row.names=FALSE)





##############################################################################################################################
##############################################################################################################################
##############################################################################################################################







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
df$lat.long<-paste(df$lat, df$long, sep=",")
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

dx$doy<-yday(dx$Date)
dx$spring<-ifelse(dx$doy>=1 & dx$doy<=90, "spring", 0)
ddx<-dx[(dx$spring=="spring"),]
ddx<-ddx[!is.na(ddx$Tavg),]

dx<-dplyr::select(dx, -date)
dx$year<-substr(dx$Date, 0,4)
dx$lat.long<-paste(dx$lat, dx$long)
dx<-dx[!duplicated(dx),]
dx$mat<-ave(dx$Tavg, dx$year, dx$lat.long)

ddx$pre.bb<-ave(ddx$Tavg, ddx$year, ddx$lat.long)
#xdd<-dx%>%dplyr::select(-Tavg, -Date, -spring, -doy)
#xdd<-xdd[!duplicated(xdd),]

spring<-ddx%>%dplyr::select(-Tavg, -Date, -spring, -doy)
spring<-spring[!duplicated(spring),]


write.csv(xdd, file="~/Documents/git/regionalrisk/analyses/output/mat_compressed.csv", row.names=FALSE)
write.csv(spring, file="~/Documents/git/regionalrisk/analyses/output/mat_spring.csv", row.names=FALSE)
