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
library(plyr)


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

#how_many<-d[which(is.na(d$Tmin)),]
d<-d[!(is.na(d$Tmin) & is.na(d$PEP_ID)),]

d$grow<-ifelse(is.na(d$BBCH), NA, TRUE)
d$count <- ave(
  d$grow, d$PEP_ID, d$year,
  FUN=function(x) cumsum(c(1, head(x, -1)))
)

d$frz<- ifelse((d$Tmin<=-2.2), 1, 0)

plz<- d#%>% filter(year==1997)
plz<-plz[!(is.na(plz$count) & is.na(plz$Tmin)),]
plz<- plz[ave(plz$year, plz$PEP_ID, FUN = length) > 1, ]
plz$count<-ifelse(plz$count==1, "start", plz$count)

############# WORK ON LAST OBSERVATION! ######################
plz$pepyear<-paste(plz$PEP_ID, plz$year, sep=",")
plz$pepBB<-paste(plz$pepyear, plz$BBCH, sep=",")
test<-plz
#test<- within(test, count[test$count==2]<-2)
tt<-test%>%
  filter(count==2) %>%
  group_by(PEP_ID, year) %>%
  filter(date==max(date))
  
tt<-within(tt, count[tt$count==2]<-"end")
dat <- merge(plz, tt, by = "pepBB", all.x = TRUE)
dat<-dat[!(dat$count.x=="start" & dat$count.y=="end"),]
transform(dat, color = ifelse(plz$count!="end", 
                              plz$count, tt$count))[-(2:3)]

#############################################################
plz$count<-ifelse(plz$count==2, "end", plz$count)
plz$count<-ifelse(plz$count==3, NA, plz$count)
plz<-na.omit(plz)

plz$pepyear<-paste(plz$PEP_ID, plz$year, sep=",")
ends<- unique(plz$pepyear[which(plz$count=="end")])
  
plz<-filter(plz, pepyear %in% ends)
plz$start<-ifelse(plz$count=="start", plz$date, NA)
class(plz$start)<-"Date"
plz$end<-ifelse(plz$count=="end", plz$date, NA)
class(plz$end)<-"Date"
  
plzers<-plz%>%dplyr::select(year, PEP_ID, start, end, lat, long, pepyear)%>%group_by(PEP_ID, year)%>%arrange(PEP_ID)

plzers<-setDT(plzers)[, lapply(.SD, na.omit), by = pepyear]
plzers<-plzers[!duplicated(plzers), ]


please<-plzers %>%
  arrange(PEP_ID)%>%
  rowwise()%>%
  do(data.frame(PEP_ID=.$PEP_ID, lat= .$lat, long=.$long, date = seq.Date(.$start, .$end, by=1)))

freeze<-left_join(please,clim)

freeze$frz<-ifelse(freeze$Tmin<=-2.2, 1, 0)
freeze$count <- ave(
  freeze$frz, freeze$PEP_ID, freeze$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

#freeze<-ungroup(freeze)
#test<- freeze %>% group_by(PEP_ID, lat, long, year) %>% summarise(count = max(count))

ggplot((freeze), aes(x=long, y=lat)) + geom_point(aes(col=count))

#write.csv(d, "~/Documents/git/regionalrisk/analyses/output/acer_combined.csv", row.names=FALSE)

