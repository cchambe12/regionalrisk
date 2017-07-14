## Cat July 10, 2017

### Doing the exact same thing except using Acer

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
bb<-read.csv("bbch_region.csv", header=TRUE)
clim<-read.csv("climate_master.csv", header=TRUE)
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
d<-d[!duplicated(d),]

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
#plz<- plz[ave(plz$year, plz$PEP_ID, FUN = length) > 1, ]
plz$count<-ifelse(plz$count==1, "start", plz$count)

############# WORK ON LAST OBSERVATION! ######################
plz$pepyear<-paste(plz$PEP_ID, plz$year, sep=",")
plz$pepBB<-paste(plz$pepyear, plz$BBCH, sep=",")
test<-plz
#test<- within(test, count[test$count==2]<-2)
tt<-test%>%
  filter(count!="start") %>%
  group_by(pepBB) %>%
  filter(date==max(date))
tt$count<-as.numeric(as.character(tt$count))
tt<-tt[!duplicated(tt), ]
tt$count<-ifelse(tt$count=="start", "start", 2)
tt<-within(tt, count[tt$count==2]<-"end")
dat <- merge(plz, tt, by = "pepBB", all.x = TRUE)
dat$count.x[which(dat$count.x==2 & dat$count.y=="end")]<-"end"
dat$count.x[which(dat$count.x=="start" & dat$count.y=="end")]<-"start"

dat.clean<-dat%>%
  dplyr::select(pepBB, year.x, Tmin.x, lat.x, long.x, date.x, PEP_ID.x, BBCH.x, DAY.x, species.x, count.x, frz.x, pepyear.x)%>%
  rename(year=year.x)%>%
  rename(Tmin=Tmin.x)%>%
  rename(lat=lat.x)%>%
  rename(long=long.x)%>%
  rename(date=date.x)%>%
  rename(PEP_ID=PEP_ID.x)%>%
  rename(BBCH=BBCH.x)%>%
  rename(DAY=DAY.x)%>%
  rename(species=species.x)%>%
  rename(count=count.x)%>%
  rename(frz=frz.x)%>%
  rename(pepyear=pepyear.x)
plz<-dat.clean  



#############################################################
#plz$count<-ifelse(plz$count==2, "end", plz$count)
#plz$count<-ifelse(plz$count==3, NA, plz$count)
#plz<-na.omit(plz)

#plz$pepyear<-paste(plz$PEP_ID, plz$year, sep=",")
#ends<- unique(plz$pepyear[which(plz$count=="end")])

#plz<-filter(plz, pepyear %in% ends)
plz$start<-ifelse(plz$count=="start", plz$date, NA)
class(plz$start)<-"Date"
plz$end<-ifelse(plz$count=="end", plz$date, NA)
class(plz$end)<-"Date"

plzers<-plz%>%dplyr::select(year, PEP_ID, start, end, lat, long, pepyear)

plzers<-setDT(plzers)[, lapply(.SD, na.omit), by = pepyear]
#plzers<-plzers[!(plzers$start==plzers$end),]
#plzers<-plzers[!duplicated(plzers), ]
plzers<-plzers[!(is.na(plzers$PEP_ID))]
plzers$end<-ifelse(is.na(plzers$end), plzers$start, plzers$end)
class(plzers$end)<-"Date"

please<-plzers %>%
  arrange(PEP_ID)%>%
  rowwise()%>%
  do(data.frame(PEP_ID=.$PEP_ID, year=.$year, pepyear=.$pepyear, lat= .$lat, long=.$long, date = seq.Date(.$start, .$end, by=1)))

freeze<-left_join(please,clim)
freeze<-freeze[!(is.na(freeze$Tmin)),]

freeze$frz<-ifelse(freeze$Tmin<=-2.2, 1, 0)
freeze$count <- ave(
  freeze$frz, freeze$PEP_ID, freeze$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

## Any relationship between growing season and region?
peppies<-as.data.frame(table(freeze$pepyear))
peppies<-peppies%>%
  rename(pepyear=Var1)%>%
  rename(growth=Freq)
freeze<-full_join(freeze, peppies)
dxx<-freeze%>%group_by(PEP_ID, year, lat, long, growth)%>%summarise(frz=max(count))
dxx$fs<-ifelse(dxx$frz>=1, 1, 0)
total<-tally(group_by(dxx,PEP_ID))
total<-rename(total, total=n)
dxx<-full_join(dxx, total)
fs<-dxx%>%dplyr::select(-total)
fs$fs<-ifelse(dxx$fs>=1, 1, NA)
fs<-na.omit(fs)
fs<-tally(group_by(fs,PEP_ID))
fs<-rename(fs, events=n)
dxx<-full_join(dxx, fs)
dxx$events<-ifelse(is.na(dxx$events), 0, dxx$events)
dxx$freq<-dxx$events/dxx$total

#ggplot((dxx), aes(x=long, y=lat)) + geom_point(aes(color=as.factor(freq)))
model1<-lm(freq~ growth+lat*long, data=dxx)
display(model1)
mod<-lm(events~growth + lat*long, data=dxx)
display(mod)

mod<-glmer(fs~lat + (1|year), data=dxx, family=binomial(link="logit"))
display(mod)


#qplot(as.factor(PEP_ID), growth, data = freeze, 
#geom = "boxplot", color=as.factor(lat)) + 
#xlab("Site")+ylab("Growing Season length")

#ggplot((freeze), aes(x=long, y=lat)) + geom_point(aes(col=count))

#write.csv(dxx, "~/Documents/git/regionalrisk/analyses/output/acer_events.csv", row.names=FALSE)

