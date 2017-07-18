## 13 July 2017 - Cat
## Cleaning up PEP data for Regional Risk paper
# Tilia cordata


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


# Set Working Directory
setwd("~/Documents/git/regionalrisk/data/tilia")
austria<-read.csv("PEP725_AT/PEP725_AT_Tilia.csv", header=TRUE)
aust.station<-read.csv("PEP725_AT/PEP725_AT_stations.csv", header=TRUE)
bos<-read.csv("PEP725_BA/PEP725_BA_Tilia.csv", header=TRUE)
bos.station<-read.csv("PEP725_BA/PEP725_BA_stations.csv", header=TRUE)
switz<-read.csv("PEP725_CH/PEP725_CH_Tilia.csv", header=TRUE)
switz.station<-read.csv("PEP725_CH/PEP725_CH_stations.csv", header=TRUE)
czech<-read.csv("PEP725_CZ/PEP725_CZ_Tilia.csv", header=TRUE)
czech.station<-read.csv("PEP725_CZ/PEP725_CZ_stations.csv", header=TRUE)
croatia<-read.csv("PEP725_HR/PEP725_HR_Tilia.csv", header=TRUE) 
croa.station<-read.csv("PEP725_HR/PEP725_HR_stations.csv", header=TRUE) 
germany<-read.csv("PEP725_DE/PEP725_DE_Tilia.csv", header=TRUE)
germ.station<-read.csv("PEP725_DE/PEP725_DE_stations.csv", header=TRUE)
spain<-read.csv("PEP725_ES/PEP725_ES_Tilia.csv", header=TRUE)
spain.station<-read.csv("PEP725_ES/PEP725_ES_stations.csv", header=TRUE)
ireland<-read.csv("PEP725_IE/PEP725_IE_Tilia.csv", header=TRUE)
ire.station<-read.csv("PEP725_IE/PEP725_IE_stations.csv", header=TRUE) 
german2<-read.csv("PEP725_IP/PEP725_IP_Tilia.csv", header=TRUE)
german2.station<-read.csv("PEP725_IP/PEP725_IP_stations.csv", header=TRUE)
lat<-read.csv("PEP725_LT/PEP725_LT_Tilia.csv", header=TRUE) #Actually Lithuania
lat.station<-read.csv("PEP725_LT/PEP725_LT_stations.csv", header=TRUE)
mont<-read.csv("PEP725_ME/PEP725_ME_Tilia.csv", header=TRUE) 
mont.station<-read.csv("PEP725_ME/PEP725_ME_stations.csv", header=TRUE)
neth<-read.csv("PEP725_NL/PEP725_NL_Tilia.csv", header=TRUE)
neth.station<-read.csv("PEP725_NL/PEP725_NL_stations.csv", header=TRUE)
pol<-read.csv("PEP725_PL/PEP725_PL_Tilia.csv", header=TRUE)
pol.station<-read.csv("PEP725_PL/PEP725_PL_stations.csv", header=TRUE)
slov<-read.csv("PEP725_SI/PEP725_SI_Tilia.csv", header=TRUE)
slov.station<-read.csv("PEP725_SI/PEP725_SI_stations.csv", header=TRUE)
vakia<-read.csv("PEP725_SK/PEP725_SK_Tilia.csv", header=TRUE) # ADDED!
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

ch<-switz%>%filter(BBCH<=19)
ch<-full_join(ch, switz.station)
ch<-dplyr::select(ch, -NAME)
ch<-na.omit(ch)

d<-bind_rows(d,ch)

cz<-czech%>%filter(BBCH<=19)
cz<-full_join(cz, czech.station)
cz<-dplyr::select(cz, -NAME)
cz<-na.omit(cz)

d<-bind_rows(d, cz)

es<-spain%>%filter(BBCH<=19)
es<-full_join(es, spain.station)
es<-dplyr::select(es, -NAME)
es<-na.omit(es)

d<-bind_rows(d, es)

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

la<-lat%>%filter(BBCH<=19)
la<-full_join(la, lat.station)
la<-dplyr::select(la, -NAME)
la<-na.omit(la)

d<-bind_rows(d, la)

me<-mont%>%filter(BBCH<=19)
me<-full_join(me, mont.station)
me<-dplyr::select(me, -NAME)
me<-na.omit(me)

d<-bind_rows(d, me)

pl<-pol%>%filter(BBCH<=19)
pl<-full_join(pl, pol.station)
pl<-dplyr::select(pl, -NAME)
pl<-na.omit(pl)

d<-bind_rows(d, pl)

nl<-neth%>%filter(BBCH<=19)
nl<-full_join(nl, neth.station)
nl<-dplyr::select(nl, -NAME)
nl<-na.omit(nl)

d<-bind_rows(d, nl)

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
#d<-na.omit(d)
d$species<-"TILCOR"
write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/bbch_region_tilia.csv", row.names = FALSE)
