## 21 June 2017 - Cat
## Working on integrating BBCH with freezing temperatures!!
# Regional Risk stuffs

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(arm)
library(data.table)
library(lme4)

setwd("~/Documents/git/regionalrisk/analyses/output")
bb<-read.csv("bbch_region_aesculus.csv", header=TRUE)
clim<-read.csv("climate_aesculus.csv", header=TRUE)
clim$date<-as.Date(paste(clim$year, clim$month, clim$day, sep="-"))

bb<-bb%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(bb$YEAR, bb$DAY)
bb$date<-as.Date(strptime(x, format="%Y %j"))
bb$year<-as.numeric(substr(bb$date, 0,4))
bb$month<-as.numeric(substr(bb$date, 6, 7))
bb$day<-as.numeric(substr(bb$date, 9,10))
bb<-bb%>%dplyr::select(-National_ID, -YEAR)
#bb$lat<-round(bb$lat, digits=2)
#bb$long<-round(bb$long, digits=2)

#clim$lat<-round(clim$lat, digits=2)
#clim$long<-round(clim$long, digits=2)

#d<-full_join(clim, bb)
#d<-filter(d, year>=1950)
#d<-d[!duplicated(d),]
clim<-filter(clim, year==1950) ## to test code

climate<-clim%>%dplyr::select(lat, long, year)
climate<-climate[!duplicated(climate$lat),]

climate$species<-"AESHIP"
bb.lat<-filter(bb, year==1950)## to test code
bb.lat<-bb.lat%>%dplyr::select(year, lat, long)
bb.lat<-bb.lat[!duplicated(bb.lat$lat),]

test<-full_join(climate, bb.lat)

climate$bblat<-NA

bb.lat$bblat<-bb.lat$lat
bb.lat$bblong<-bb.lat$long


for(i in c(1:nrow(climate))) {
  for(j in c(1:nrow(bb.lat)))
    climate$bblat[i]<-ifelse(bb.lat$lat[j] >= climate$lowlat[i] & bb.lat$lat[j] <= climate$uplat[i], bb.lat$lat[j],
                             NA)
}

clim$species<-"AESHIP"
clim$bbch<-NA
clim<-filter(clim, year==1950) ## to test code
bb<-filter(bb, year==1950)## to test code
for(i in c(1:nrow(clim))) {
  for(j in c(1:nrow(bb)))
    clim$bbch[i]<-ifelse(clim$date[i] == bb$date[j] & clim$lat[i]==bb$lat[j] &
                           clim$long[i]==bb$long[j], bb$BBCH, NA)
  print(paste(i,j))
}

yrs<-unique(clim$year)
for(i in c(1:nrow(clim))){
  for(j in yrs)
    clim$bud[i]<-tapply(clim$date[j],list(clim$bbch),min)
    #clim$leaf[i]<-tapply(clim$date[j],list(clim$bbch),max)
  print(paste(i,j))
}


################## Extras ############################
climate$lat<-sort(climate$lat)
climate$uplat <- NA
climate$lowlat <- NA
climate$lowlat[1] <- 46
climate$uplat[1] <- climate$lat[1]
climate$lowlat[2:50] <- climate$lat[1:50]+0.0001
climate$uplat[2:50] <- climate$lat[2:50]

climate$long<-sort(climate$long)
climate$uplong <- NA
climate$lowlong <- NA
climate$lowlong[1] <- -3
climate$uplong[1] <- climate$long[1]
climate$lowlong[2:50] <- climate$long[1:56]+0.0001
climate$uplong[2:50] <- climate$long[2:56]
