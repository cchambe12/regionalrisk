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


climate<-clim%>%filter(year==1997)
plz<- dx%>% filter(year==1997)
plz<-na.omit(plz)
peps<-unique(plz$PEP_ID[which(plz$count==2)])
plz<-plz%>%filter(PEP_ID %in% peps)
plz$count<-ifelse(plz$count==1, "start", plz$count)
plz$count<-ifelse(plz$count==2, "end", plz$count)
plz$count<-ifelse(plz$count==3, NA, plz$count)
plz<-na.omit(plz)

plz$start<-ifelse(plz$count=="start", plz$date, NA)
class(plz$start)<-"Date"
plz$end<-ifelse(plz$count=="end", plz$date, NA)
class(plz$end)<-"Date"
  
plzers<-plz%>%dplyr::select(year, PEP_ID, start, end, lat, long)
plzers<-setDT(plzers)[, lapply(.SD, na.omit), by = PEP_ID]
plzers<-plzers[!duplicated(plzers), ]

please<-plzers %>%
  arrange(PEP_ID)%>%
  rowwise()%>%
  do(data.frame(PEP_ID = .$PEP_ID, lat= .$lat, long=.$long, date = seq(.$start, .$end, by = "days")))

freeze<-left_join(please,climate)

freeze$frz<-ifelse(freeze$Tmin<=-2.2, 1, 0)
freeze$count <- ave(
  freeze$frz, freeze$PEP_ID, freeze$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

freeze<-ungroup(freeze)
test<- freeze %>% group_by(PEP_ID, lat, long, year) %>% summarise(count = max(count))

mod<-lm(lat~count+long, data=test)
display(mod)


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


