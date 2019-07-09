## 8 August 2017 - Cat
## Cleaning up PEP data for Regional Risk paper
# Populus tremula


# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


# Set Working Directory
setwd("~/Documents/git/regionalrisk/data/populus")
bos<-read.csv("PEP725_BA/PEP725_BA_Populus.csv", header=TRUE)
bos.station<-read.csv("PEP725_BA/PEP725_BA_stations.csv", header=TRUE)
fin<-read.csv("PEP725_FI/PEP725_FI_Populus.csv", header=TRUE)
fin.station<-read.csv("PEP725_FI/PEP725_FI_stations.csv", header=TRUE)
ireland<-read.csv("PEP725_IE/PEP725_IE_Populus.csv", header=TRUE)
ire.station<-read.csv("PEP725_IE/PEP725_IE_stations.csv", header=TRUE) 
german2<-read.csv("PEP725_IP/PEP725_IP_Populus.csv", header=TRUE)
german2.station<-read.csv("PEP725_IP/PEP725_IP_stations.csv", header=TRUE)
mont<-read.csv("PEP725_ME/PEP725_ME_Populus.csv", header=TRUE) 
mont.station<-read.csv("PEP725_ME/PEP725_ME_stations.csv", header=TRUE)
norway<-read.csv("PEP725_NO/PEP725_NO_Populus.csv", header=TRUE)
norway.station<-read.csv("PEP725_NO/PEP725_NO_stations.csv", header=TRUE)

bs<-bos%>%filter(BBCH<=19)
bs<-full_join(bs, bos.station)
bs<-dplyr::select(bs, -NAME)
bs<-na.omit(bs)

fi<-fin%>%filter(BBCH<=19)
fi<-full_join(fi, fin.station)
fi<-dplyr::select(fi, -NAME)
fi<-na.omit(fi)

d<-bind_rows(bs, fi)

ie<-ireland%>%filter(BBCH<=19)
ie<-full_join(ie, ire.station)
ie<-dplyr::select(ie, -NAME)
ie<-na.omit(ie)

d<-bind_rows(d, ie)

ip<-german2%>%filter(BBCH<=19)
ip<-full_join(ip, german2.station)
ip<-dplyr::select(ip, -NAME)
ip<-na.omit(ip)

d<-bind_rows(d, ip)

me<-mont%>%filter(BBCH<=19)
me<-full_join(me, mont.station)
me<-dplyr::select(me, -NAME)
me<-na.omit(me)

d<-bind_rows(d, me)

no<-norway%>%filter(BBCH<=19)
no<-full_join(no, norway.station)
no<-dplyr::select(no, -NAME)
no<-na.omit(no)

d<-bind_rows(d, no)

d<-dplyr::select(d, -X, -X.1)
d<-na.omit(d)
d$species<-"POPTRE"
write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/bbch_region_populus.csv", row.names = FALSE)


