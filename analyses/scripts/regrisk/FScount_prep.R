# Let's try again... Tilia platyphyllos
## The purpose of this script is to find the points of PEP data that look at BBCH stage 11 for budburst
# then we will add 12 days for leaf out date to find a general idea for number of false springs across a range
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
d<-read.csv("output/BBdata.csv", header=TRUE)

### Let's just start with the PEP data and do some cleaning
df<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON)%>%
  rename(year=YEAR)%>%
  rename(bb=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)

## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df<-d%>%rename(lat=LAT)%>%rename(long=LON)
df$lat.long<-paste(df$lat, df$long)
df$lo<-df$bb+12
df<- df[order(df$lat.long, df$year, df$species), ]
df$pep.year<-paste(df$year, df$lat.long, df$species)
days.btw <- Map(seq, df$bb, df$lo, by = 1)

dxx <- data.frame(species=df$species, lat=df$lat, long=df$long, year=df$year,
                  pep.year = rep.int(df$pep.year, vapply(days.btw, length, 1L)), 
                  doy = do.call(c, days.btw))
#dxx$year<-substr(dxx$pep.year, 1, 4)
dxx<-dplyr::select(dxx, -pep.year)
x<-paste(dxx$year, dxx$doy)
dxx$date<-as.Date(strptime(x, format="%Y %j"))
dxx$Date<- as.character(dxx$date)

write.csv(dxx, file="~/Documents/git/regionalrisk/analyses/output/fscount_prep.csv", row.names = FALSE)

r<-brick("~/Desktop/tn_0.25deg_reg_v16.0.nc", varname="tn", sep="")

bb<-dxx
bb$lat.long<-paste(bb$lat, bb$long, sep=",")
bb<-bb[!duplicated(bb$lat.long),]
lats <- bb$lat
lons <- bb$long

coords <- data.frame(x=lons,y=lats)

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

tilpla<-inner_join(dx, dxx, by=c("Date", "lat", "long"))
any.nas<-tilpla[is.na(tilpla$Tmin),]

write.csv(tilpla, file="~/Documents/git/regionalrisk/analyses/output/tilpla_data.csv", row.names=FALSE)
