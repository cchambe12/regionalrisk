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

bb<-bb%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(bb$YEAR, bb$DAY)
bb$date<-as.Date(strptime(x, format="%Y %j"))
bb$year<-as.numeric(substr(bb$date, 0,4))
bb$month<-as.numeric(substr(bb$date, 6, 7))
bb$day<-as.numeric(substr(bb$date, 9,10))
bb<-bb%>%dplyr::select(-National_ID, -YEAR)
#bb$lat<-round(bb$lat, digits=0)
#bb$long<-round(bb$long, digits=0)

#clim$lat<-round(clim$lat, digits=0)
#clim$long<-round(clim$long, digits=0)

d<-full_join(clim, bb)
d<-filter(d, year>=1950)
#write.csv(d, "~/Documents/git/regionalrisk/analyses/output/acer_combined.csv", row.names=FALSE)

how_many<-d[which(is.na(d$Tmin)),]

#write.csv(d, "~/Desktop/join.csv", row.names=FALSE)
twelve<-d%>%
  filter(year==2001)
twelve$BBCH<-ifelse(twelve$BBCH>0, TRUE, FALSE)

xx<-twelve%>%
  filter(!is.na(Tmin))

ids<-as.data.frame(table(xx$PEP_ID))


########################## Lame Attempts to keep for now #########################
twel<-twelve%>%
  group_by(lat, long, PEP_ID)%>%
  arrange(BBCH) %>%
  filter(row_number()==1 | row_number()==n())


subby<-na.omit(twelve)  
substart<-subby%>%
  group_by(year, PEP_ID, BBCH)%>%
  arrange(PEP_ID)%>%
  filter(row_number()==1)
substart$start<-"First"
subend<-subby%>%
  group_by(year, PEP_ID, BBCH)%>%
  arrange(PEP_ID)%>%
  filter(row_number()==n())
subend$end<-"Last"
full_join


try<-d %>%
  filter(year>=1950)%>%
  group_by(year, lat, long, DAY)%>%
  arrange(year) %>%
  filter(row_number()==1 | row_number()==n())




frz<-d%>%
  group_by(year, lat, long, DAY)%>%
  filter(row_number()==1)
ungroup(frz)
check<-frz%>%
  group_by(date, lat, long)%>%
  arrange(PEP_ID)
