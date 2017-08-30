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
library(reshape2)


setwd("~/Documents/git/regionalrisk/analyses")
eur.tempmn <- nc_open("tn_0.25deg_reg_v15.0.nc")
r<-brick("input/tn_0.25deg_reg_v15.0.nc", varname="tn", sep="")
bx<-read.csv("output/bbch_region_aesculus.csv", header=TRUE)


#bx<-filter(bx, YEAR==1950)
bb<-bx
bb$lat.long<-paste(bb$LAT, bb$LONG, sep=",")
bb<-bb[!duplicated(bb$lat.long),]
lats <- bb$LAT
lons <- bb$LON

coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

df <- cbind.data.frame(coordinates(points),values)

dx<-melt(df, id.vars=c("x","y"))

dx<-dx%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tmin=value)

dx$date<-substr(dx$date, 2,11)
dx$year<-as.numeric(substr(dx$date, 0,4))
dx$month<-as.numeric(substr(dx$date, 6, 7))
dx$day<-as.numeric(substr(dx$date, 9,10))
dx$date<-as.Date(paste(dx$year, dx$month, dx$day, sep="-"))


#write.csv(dx, file="~/Documents/git/regionalrisk/analyses/output/clim.test.csv", row.names = FALSE)

dx<-read.csv("output/clim.test.csv", header=TRUE)

#dx$bbch<-0
bx<-bx%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(bx$YEAR, bx$DAY)
bx$date<-as.Date(strptime(x, format="%Y %j"))
bx$year<-as.numeric(substr(bx$date, 0,4))
bx$month<-as.numeric(substr(bx$date, 6, 7))
bx$day<-as.numeric(substr(bx$date, 9,10))
bx<-bx%>%dplyr::select(lat, long, date, BBCH, PEP_ID)
dx$date<-as.Date(dx$date)

d<-left_join(dx, bx)
d$prov<-paste(d$lat, d$long)
d <- d[order(d$prov, d$date), ]

d$grow<-ifelse(is.na(d$BBCH), NA, TRUE)
#d<-arrange(d, date)
d$count <- ave(
  d$grow, d$PEP_ID, d$year,
  FUN=function(x) cumsum(c(1, head(x, -1)))
)

d$count<-as.numeric(as.character(d$count))
leaf<-d%>%group_by(prov, year, count)
leaf$prov.year<-paste(leaf$prov, leaf$year)
leaf$end<-leaf[max(count,prov.year),]
leaf$end<-leaf$date
leaf<-ungroup(leaf)
leaf<-dplyr::select(leaf, year, date, prov, end)
d.test<-full_join(d, leaf)
bud<-d%>%group_by(prov, year, count)%>%summarize(start=first(count))
bud$start<-bud$date
bud<-ungroup(bud)
bud<-dplyr::select(bud, year, date, prov, start)
d.test<-full_join(d.test, bud)
dxx<-full_join(bud, leaf)

dxx<-read.csv("~/Desktop/aesculus.csv", header=TRUE)
dxx$check<-ifelse(dxx$start==dxx$end, TRUE, FALSE)
d.16<-dxx%>%filter(year==2016)
d.16$start<-as.Date(d.16$start)
d.16$end<-as.Date(d.16$end)
d.16$date<-as.Date(d.16$date)

d.test$count<-ifelse(d.test$start==d.test$end, TRUE, FALSE)
for(i in c(1:nrow(d.16))){
  d.16$count[i]<-ifelse(d.16$date[i]>=d.16$start && d.16$date[i]<=d.16$end, TRUE, NA)
}

d.16.melt<-d.16
d.16.melt$start<-ifelse(d.16.melt$start==d.16.melt$date, "start", NA)
d.16.melt$start<-ifelse(d.16.melt$end==d.16.melt$date, "end", d.16.melt$start)
#class(d.16.melt$start)<- "Date"
d.16.melt <- d.16.melt[order(d.16.melt$prov, d.16.melt$date), ]
for(i in c(1:nrow(d.16.melt))){
  d.16.melt$count[i]<-ifelse((d.16.melt$start[i]=="start")<=d.16.melt$date[i]<= (d.16$start.melt[i]=="end"), TRUE, NA)
}


yrs<-unique(d$year)
prov<-unique(paste(d$lat, d$long))
for(i in prov){
  for(j in yrs)
    d$bud[i]<-tapply(d$date,list(d$BBCH),min)
  #clim$leaf[i]<-tapply(clim$date[j],list(clim$bbch),max)
  print(paste(i,j))
}


d$bud<-tapply(d$prov, list(d$BBCH), min)
