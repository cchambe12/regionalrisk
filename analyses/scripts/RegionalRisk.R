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

setwd("~/Documents/git/regionalrisk/analyses/output")
bb<-read.csv("bbch_region.csv", header=TRUE)
clim<-read.csv("climate_acer.csv", header=TRUE)
clim$date<-as.Date(paste(clim$year, clim$month, clim$day, sep="-"))

bb<-bb%>%filter(YEAR>=2012 & YEAR<=2016)%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
bb$date<-as.Date(bb$DAY, origin = "2012-01-01")
bb$year<-as.numeric(substr(bb$date, 0,4))
bb$month<-as.numeric(substr(bb$date, 6, 7))
bb$day<-as.numeric(substr(bb$date, 9,10))
bb<-bb%>%dplyr::select(-National_ID, -YEAR)

d<-full_join(clim, bb)

#write.csv(d, "~/Desktop/join.csv", row.names=FALSE)

frz<-d%>%
  group_by(year, lat, long, DAY)%>%
  filter(row_number()==1)
ungroup(frz)
check<-frz%>%
  group_by(date, lat, long)%>%
  arrange(PEP_ID)