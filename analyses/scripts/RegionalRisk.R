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
bb<-read.csv("bbch_region_betula.csv", header=TRUE)
clim<-read.csv("climate_betula.csv", header=TRUE)
clim$date<-as.Date(paste(clim$year, clim$month, clim$day, sep="-"))

bb<-bb%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(bb$YEAR, bb$DAY)
bb$date<-as.Date(strptime(x, format="%Y %j"))
bb$year<-as.numeric(substr(bb$date, 0,4))
bb$month<-as.numeric(substr(bb$date, 6, 7))
bb$day<-as.numeric(substr(bb$date, 9,10))
bb<-bb%>%dplyr::select(-National_ID, -YEAR)
bb$lat<-round(bb$lat, digits=2)
bb$long<-round(bb$long, digits=2)

clim$lat<-round(clim$lat, digits=2)
clim$long<-round(clim$long, digits=2)

d<-full_join(clim, bb)
d<-filter(d, year>=1950)

how_many<-d[which(is.na(d$Tmin)),]
d<-d[!(is.na(d$Tmin) & is.na(d$PEP_ID)),]

df<-d%>%dplyr::select(year, PEP_ID, BBCH)
df$BBCH<-ifelse(df$BBCH>=7, df$BBCH, NA)
df<-df%>%group_by(year,PEP_ID)%>%arrange(year)
df<-na.omit(df)
df <- within(df, { count <- ave(BBCH,PEP_ID, year, FUN=function(x) length(unique(x)))})
df$count<-ifelse(df$count>=2,df$count, NA)
xx<-na.omit(df)
unique(xx$year) ## 1997-2015

peps<-unique(xx$PEP_ID)
years<-c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)

frz<-d%>%filter(year%in%years)%>%filter(PEP_ID %in% peps)
many<-frz[which(is.na(frz$Tmin)),]
frz<-frz[!(is.na(frz$Tmin)),]

frz$lat.long<-paste(frz$lat,frz$long, sep=",")

##### Now find climate data ######
d$lat.long<-paste(d$lat, d$long, sep=",")
lats<-unique(frz$lat.long)
dx<-filter(d, lat.long%in%lats)
dx<-dx[!(is.na(dx$Tmin)),]
dx$grow<-ifelse(is.na(dx$BBCH), NA, TRUE)
dx$count <- ave(
  dx$grow, dx$PEP_ID, dx$year,
  FUN=function(x) cumsum(c(1, head(x, -1)))
)
dx$frz<- ifelse((dx$Tmin<=-2.2), 1, 0)

try<-dx%>%filter(year==1997)
try$total<-paste(try$year, try$PEP_ID)
printvalues <- function(y) {
  repeat {
    try$count==1;
    print(TRUE)
    if (! (try$count==2) ) {break}   # Negation is crucial here!
  }
}


try$freezes <- ave(
  try$frz, try$total,between(try$count, 1, 2),
  FUN=function(x) cumsum(c(0, head(x, -1)))
)


dx<-dx %>%
  group_by(PEP_ID, year, grow) %>%
  arrange(PEP_ID)%>%
  filter(between(row_number(), 1, n()))%>%
  ungroup()
dx<-arrange(dx, date)




dx$BBCH<-ifelse(is.na(dx$BBCH), NA, TRUE)
dx<-data.frame(dx,first=!duplicated(dx),last=rev(!duplicated(rev(dx))))

#write.csv(d, "~/Documents/git/regionalrisk/analyses/output/acer_combined.csv", row.names=FALSE)



########################## Lame Attempts to keep for now #########################

dxx<-dx%>%dplyr::select(PEP_ID, BBCH, lat.long, year, date)
dxx<-dxx%>%
  group_by(year, PEP_ID,BBCH)%>%
  arrange(PEP_ID)%>%
  filter(between(row_number(), 1, n()))
dxx<-na.omit(dxx)
dxx<-dplyr::select(dxx, -date)
dxx$BBCH<-ifelse(is.na(dxx$BBCH), NA, TRUE)
dxx<-data.frame(dxx,first=!duplicated(dxx),last=rev(!duplicated(rev(dxx))))
dxx$one<-ifelse(dxx$first==TRUE & dxx$last==TRUE, FALSE, dxx$first)
dxx$two<-ifelse(dxx$first==TRUE & dxx$last==TRUE, FALSE, dxx$last)
dxx<-dxx%>%dplyr::select(-first, -last)
dxx$BBCH<-ifelse(is.na(dxx$BBCH), NA, TRUE)
dxx<-data.frame(dxx,first=!duplicated(dxx),last=rev(!duplicated(rev(dxx))))

dx$obs<-ifelse(dx$BBCH<=14, start, dx$BBCH)
dx$obs<-ifelse(dx$BBCH==15, end, dx$BBCH)


dx<-dx%>%
  group_by(year, PEP_ID,BBCH)%>%
  arrange(PEP_ID)%>%
  filter(between(row_number(), 1, n()))
dx<-ungroup(dx)
dx<-dx%>%group_by(date, lat.long)%>%arrange(date)
dx$frz<- ifelse((dx$Tmin<=-2.2), 1, 0)
dx$count <- ave(
  dx$frz, dx$lat.long, dx$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)


