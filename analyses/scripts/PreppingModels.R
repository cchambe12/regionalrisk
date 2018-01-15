### 12 January 2018 - Cat
### Starting Analysis - looking at different questions
## 1) FS (y/n) ~ year + (1|species) + site
## 2) FS # ~ species + site + MAT

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(rstan)
library(bayesplot)
library(shinystan)


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


### Clean the weather data
setwd("~/Desktop/")
r<-brick("~/Desktop/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")

## BETPEN
bp<- d %>% filter(species=="BETPEN")
bp$lat.long<-paste(bp$lat, bp$long, sep=",")
bp<-bp[!duplicated(bp$lat.long),]
lats <- bp$lat
lons <- bp$long
coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

bpclim <- cbind.data.frame(coordinates(points),values)

bp<-melt(bpclim, id.vars=c("x","y"))

bp<-bp%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tavg=value)

bp$date<-substr(bp$date, 2,11)
bp$Date<-gsub("[.]","-", bp$date)

bp<-dplyr::select(bp, -date)

## AESHIP
ah<- d %>% filter(species=="AESHIP")
ah$lat.long<-paste(ah$lat, ah$long, sep=",")
ah<-ah[!duplicated(ah$lat.long),]
lats <- ah$lat
lons <- ah$long
coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

bpclim <- cbind.data.frame(coordinates(points),values)

bp<-melt(bpclim, id.vars=c("x","y"))

bp<-bp%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tavg=value)

bp$date<-substr(bp$date, 2,11)
bp$Date<-gsub("[.]","-", bp$date)

bp<-dplyr::select(bp, -date)

## ALNGLU
ag<- d %>% filter(species=="ALNGLU")
ag$lat.long<-paste(ag$lat, ag$long, sep=",")
ag<-ag[!duplicated(ag$lat.long),]
lats <- ag$lat
lons <- ag$long
coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

agclim <- cbind.data.frame(coordinates(points),values)

ag<-melt(agclim, id.vars=c("x","y"))

ag<-ag%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tavg=value)

ag$date<-substr(ag$date, 2,11)
ag$Date<-gsub("[.]","-", ag$date)

ag<-dplyr::select(ag, -date)

fe<- d %>% filter(species=="FRAEXC")
fs<- d %>% filter(species=="FAGSYL")
qr<- d %>% filter(species=="QUEROB")

dx<-d
dx$lat.long<-paste(dx$lat, dx$long, sep=",")
dx<-dx[!duplicated(dx$lat.long),]
lats <- dx$lat
lons <- dx$long

coords <- data.frame(x=lons,y=lats)

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

dxx<-dplyr::select(dxx, -date)
dx<-dplyr::select(dx, -date)


## Start running models...
d$PEP_ID<-as.numeric(as.factor(d$PEP_ID))
d$year<-as.numeric(d$year)
d$species<-as.numeric(as.factor(d$species))

mod<-stan_glmer(fs~year+(1|species)+lat*long, data=d, family=gaussian)


