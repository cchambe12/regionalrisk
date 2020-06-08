## Started 27 Sept 2018 ##
## By Cat ##

## Using elevation and NAO index and spring mean annual temperature ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data

## fix KNMI Climate Explorer data for NAO - https://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/cpc_nao&STATION=CPC_NAO&TYPE=i&id=someone@somewhere
# http://www.cpc.noaa.gov/products/precip/CWlink/daily_ao_index/teleconnections.shtml - 17 December 2018
#setwd("~/Documents/git/regionalrisk")
#nao<-read.delim("data/icpc_nao_daily.dat.txt", header=TRUE)
#write.csv(nao, "~/Documents/git/regionalrisk/analyses/output/icpc_nao_daily.csv", row.names=FALSE)
nao<-read.csv("output/icpc_nao_daily.csv", header=TRUE)

nov<-nao[(nao$month>=11 | nao$month<=4),]
nov$nao<-ave(nov$nao, nov$month, nov$year)
nov<-subset(nov, select=c(year, month, nao))
nov<-nov[!duplicated(nov),]

nov<-nov[!(nov$year==1950 & nov$month<=4),]
nov<-nov[!(nov$year>2017),]
nov<-nov[!(nov$year==2017 & nov$month>4),]
nov$rep<-rep(1951:2017, each=6)

nov$nov.nao<-ave(nov$nao, nov$rep)
nov<-nov[!(nov$rep==2017),]

nov<-subset(nov, select=c(rep, nov.nao))
nov<-nov[!duplicated(nov),]

nov<-nov%>%rename(year=rep)%>%rename(nao=nov.nao)

#write.csv(nov, file=("output/nao_NovApr.csv"), row.names=FALSE)
