# Let's try again... Betula pendula
## The purpose of this script is to find the points of PEP data that look at BBCH stage 11 for leafout
# then we will subtract 12 days (Donnelly2017) for budburst date to find a general idea for number of false springs across a range
## A GWR will likely be necessary since the distribution of points is smaller than desired
# 25 October 2017 - Cat

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
library(raster)
library(reshape2)
library(data.table)

setwd("~/Documents/git/regionalrisk/analyses")
d<-read.csv("output/bbch_region_betula.csv", header=TRUE)

### Let's just start with the PEP data and do some cleaning
df<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
df$bb<-df$lo-15 #was 12; used Danf's WL1 and CS0 data average for all Betula species

## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df<-dplyr::select(df, bb, year, PEP_ID, lat, long, lo)
df$pep.year<-paste(df$year, df$PEP_ID)

dxx<-data_frame()
days.btw<-array()
for(i in length(df$pep.year)){
  days.btw[i] <- Map(seq, df$bb[i], df$lo[i], by = 1)
  dxx <- data.frame(PEP_ID=df$PEP_ID, year=df$year, lat=df$lat, long=df$long,
                    pep.year = rep.int(df$pep.year, vapply(days.btw[i], length, 1L)), 
                    doy = do.call(c, days.btw[i]))
}

dxx<-dxx[!duplicated(dxx),]
dxx<-dplyr::select(dxx, -pep.year)
x<-paste(dxx$year, dxx$doy)
dxx$date<-as.Date(strptime(x, format="%Y %j"))
dxx$Date<- as.character(dxx$date)

## Climate Data time...
r<-brick("~/Desktop/tn_0.25deg_reg_v16.0.nc", varname="tn", sep="")

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

betpen<-inner_join(dx, dxx, by=c("Date", "lat", "long"))
any.nas<-betpen[is.na(betpen$Tmin),]

write.csv(betpen, file="~/Documents/git/regionalrisk/analyses/output/betpen_data_dvr.csv", row.names=FALSE)
