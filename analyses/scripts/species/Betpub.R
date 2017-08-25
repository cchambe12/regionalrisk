## 8 August 2017 - Cat
## Cleaning up PEP data for Regional Risk paper
# Betula pubescens


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
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(grid)
library(rworldmap)
library(gridExtra)


# Set Working Directory
setwd("~/Documents/git/regionalrisk/data/betpub")
fin<-read.csv("PEP725_FI/PEP725_FI_Betpub.csv", header=TRUE)
fin.station<-read.csv("PEP725_FI/PEP725_FI_stations.csv", header=TRUE)
ireland<-read.csv("PEP725_IE/PEP725_IE_Betpub.csv", header=TRUE)
ire.station<-read.csv("PEP725_IE/PEP725_IE_stations.csv", header=TRUE) 
german2<-read.csv("PEP725_IP/PEP725_IP_Betpub.csv", header=TRUE)
german2.station<-read.csv("PEP725_IP/PEP725_IP_stations.csv", header=TRUE)
neth<-read.csv("PEP725_NL/PEP725_NL_Betpub.csv", header=TRUE)
neth.station<-read.csv("PEP725_NL/PEP725_NL_stations.csv", header=TRUE)
swed<-read.csv("PEP725_SE/PEP725_SE_Betpub.csv", header=TRUE)
swed.station<-read.csv("PEP725_SE/PEP725_SE_stations.csv", header=TRUE)
norway<-read.csv("PEP725_NO/PEP725_NO_Betpub.csv", header=TRUE)
norway.station<-read.csv("PEP725_NO/PEP725_NO_stations.csv", header=TRUE)

fi<-fin%>%filter(BBCH<=19)
fi<-full_join(fi, fin.station)
fi<-dplyr::select(fi, -NAME)
fi<-na.omit(fi)

ie<-ireland%>%filter(BBCH<=19)
ie<-full_join(ie, ire.station)
ie<-dplyr::select(ie, -NAME)
ie<-na.omit(ie)

d<-bind_rows(fi, ie)

ip<-german2%>%filter(BBCH<=19)
ip<-full_join(ip, german2.station)
ip<-dplyr::select(ip, -NAME)
ip<-na.omit(ip)

d<-bind_rows(d, ip)

nl<-neth%>%filter(BBCH<=19)
nl<-full_join(nl, neth.station)
nl<-dplyr::select(nl, -NAME)
nl<-na.omit(nl)

d<-bind_rows(d, nl)

se<-swed%>%filter(BBCH<=19)
se<-full_join(se, swed.station)
se<-dplyr::select(se, -NAME)
se<-na.omit(se)

d<-bind_rows(d, se)

no<-norway%>%filter(BBCH<=19)
no<-full_join(no, norway.station)
no<-dplyr::select(no, -NAME)
no<-na.omit(no)

d<-bind_rows(d, no)

d<-dplyr::select(d, -X, -X.1)
#d<-na.omit(d)
d$species<-"BETPUB"
write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/bbch_region_betpub.csv", row.names = FALSE)
