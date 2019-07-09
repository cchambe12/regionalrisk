## 28 June 2017 - Cat
## Cleaning up PEP data for Regional Risk paper
# Cornus mas


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
setwd("~/Documents/git/regionalrisk/data/cornus")
austria<-read.csv("PEP725_AT/PEP725_AT_Cornus.csv", header=TRUE)
aust.station<-read.csv("PEP725_AT/PEP725_AT_stations.csv", header=TRUE)
bos<-read.csv("PEP725_BA/PEP725_BA_Cornus.csv", header=TRUE)
bos.station<-read.csv("PEP725_BA/PEP725_BA_stations.csv", header=TRUE)
croatia<-read.csv("PEP725_HR/PEP725_HR_Cornus.csv", header=TRUE) 
croa.station<-read.csv("PEP725_HR/PEP725_HR_stations.csv", header=TRUE) 
germany<-read.csv("PEP725_DE/PEP725_DE_Cornus.csv", header=TRUE)
germ.station<-read.csv("PEP725_DE/PEP725_DE_stations.csv", header=TRUE)
mont<-read.csv("PEP725_ME/PEP725_ME_Cornus.csv", header=TRUE) 
mont.station<-read.csv("PEP725_ME/PEP725_ME_stations.csv", header=TRUE)
slov<-read.csv("PEP725_SI/PEP725_SI_Cornus.csv", header=TRUE)
slov.station<-read.csv("PEP725_SI/PEP725_SI_stations.csv", header=TRUE)
vakia<-read.csv("PEP725_SK/PEP725_SK_Cornus.csv", header=TRUE) # ADDED!
vakia.station<-read.csv("PEP725_SK/PEP725_SK_stations.csv", header=TRUE) #ADDED!


at<-austria%>%filter(BBCH<=19)
at<-full_join(at, aust.station)
at<-dplyr::select(at, -NAME)
at<-na.omit(at)

bs<-bos%>%filter(BBCH<=19)
bs<-full_join(bs, bos.station)
bs<-dplyr::select(bs, -NAME)
bs<-na.omit(bs)

d<-bind_rows(at,bs)

gm<-germany%>%filter(BBCH<=19)
gm<-full_join(gm, germ.station)
gm<-dplyr::select(gm, -NAME)
gm<-na.omit(gm)

d<-bind_rows(d, gm)

hr<-croatia%>%filter(BBCH<=19)
hr<-full_join(hr, croa.station)
hr<-dplyr::select(hr, -NAME)
hr<-na.omit(hr)

d<-bind_rows(d, hr)

me<-mont%>%filter(BBCH<=19)
me<-full_join(me, mont.station)
me<-dplyr::select(me, -NAME)
me<-na.omit(me)

d<-bind_rows(d, me)

si<-slov%>%filter(BBCH<=19)
si<-full_join(si, slov.station)
si<-dplyr::select(si, -NAME)
si<-na.omit(si)

d<-bind_rows(d, si)

sk<-vakia%>%filter(BBCH<=19)
sk<-full_join(sk, vakia.station)
sk<-dplyr::select(sk, -NAME)
sk<-na.omit(sk)

d<-bind_rows(d, sk)

d<-bind_rows(d, uk)
d<-dplyr::select(d, -X, -X.1)
#d<-na.omit(d)
d$species<-"CORMAS"
write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/bbch_region_cornus.csv", row.names = FALSE)

