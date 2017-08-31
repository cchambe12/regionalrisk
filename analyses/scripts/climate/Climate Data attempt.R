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
library(data.table)

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

pep<-dxx%>%dplyr::select(lat, long, prov, start, end)
pep<-pep[(!is.na(pep$start) & !is.na(pep$end)),]
pep$start<-as.Date(pep$start)
pep$end<-as.Date(pep$end)
pep<-pep[!duplicated(pep),]
pep$risk<-(pep$end-pep$start) + 1
pep.expand <- pep[rep(row.names(pep), pep$risk), ]
pep.expand<-data.frame(pep.expand,date=pep.expand$start+(sequence(pep$risk)-1))
pep.expand$date<-as.Date(pep.expand$date)
pep.expand$start<-as.Date(pep.expand$start)
pep.expand$end<-as.Date(pep.expand$end)
dxx$start<-as.Date(dxx$start)
dxx$end<-as.Date(dxx$end)
dxx$date<-as.Date(dxx$date)
dxx<-dplyr::select(dxx, prov, date, Tmin)

df <- merge(pep.expand, dxx, by=c('prov', 'date'), sort = FALSE)
d.aes<-df
d.aes$year<-substr(d.aes$date, 0,4)
d.aes$frz<-ifelse(d.aes$Tmin<=-2.2, 1, 0)
d.aes$freezes <- ave(
  d.aes$frz, d.aes$prov, d.aes$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
d.total<-d.aes%>%dplyr::select(prov, year)
d.total<-d.total[!duplicated(d.total),]
d.total$num.years<-ave(d.total$year, d.total[,("prov")], FUN=length)
d.aes <- merge(d.total, d.aes, by=c('prov', 'year'), sort = FALSE)
d.aes$num.years<-as.numeric(d.aes$num.years)
fs<-d.aes%>%dplyr::select(prov, year, freezes)
fs<-fs[!duplicated(fs),]
fs$freezes<-ifelse(fs$freezes>0,1,NA)
fs<-na.omit(fs)
fs<-fs[!duplicated(fs),]
fs$frz.years<-ave(fs$freezes, fs[,("prov")], FUN=length)
fs<-dplyr::select(fs,-freezes)
d.aes <- merge(fs, d.aes, by=c('prov', 'year'), sort = FALSE)
d.aes$frequency<-as.numeric(d.aes$frz.years/d.aes$num.years)
#tmin<-dxx[!is.na(dxx$Tmin),]
#whynas<-d.aes[is.na(d.aes$Tmin),]
#provs<-unique(whynas$prov)
#list<-provs[!provs %in% (unique(tmin$prov))]


