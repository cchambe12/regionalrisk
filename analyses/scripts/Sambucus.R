## 13 July 2017 - Cat
## Cleaning up PEP data for Regional Risk paper
# Sambucus nigra

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
setwd("~/Documents/git/regionalrisk/data/sambucus")
austria<-read.csv("PEP725_AT/PEP725_AT_Sambucus.csv", header=TRUE)
aust.station<-read.csv("PEP725_AT/PEP725_AT_stations.csv", header=TRUE)
bos<-read.csv("PEP725_BA/PEP725_BA_Sambucus.csv", header=TRUE)
bos.station<-read.csv("PEP725_BA/PEP725_BA_stations.csv", header=TRUE)
switz<-read.csv("PEP725_CH/PEP725_CH_Sambucus.csv", header=TRUE)
switz.station<-read.csv("PEP725_CH/PEP725_CH_stations.csv", header=TRUE)
czech<-read.csv("PEP725_CZ/PEP725_CZ_Sambucus.csv", header=TRUE)
czech.station<-read.csv("PEP725_CZ/PEP725_CZ_stations.csv", header=TRUE)
germany<-read.csv("PEP725_DE/PEP725_DE_Sambucus.csv", header=TRUE)
germ.station<-read.csv("PEP725_DE/PEP725_DE_stations.csv", header=TRUE)
spain<-read.csv("PEP725_ES/PEP725_ES_Sambucus.csv", header=TRUE)
spain.station<-read.csv("PEP725_ES/PEP725_ES_stations.csv", header=TRUE)
croatia<-read.csv("PEP725_HR/PEP725_HR_Sambucus.csv", header=TRUE) 
croa.station<-read.csv("PEP725_HR/PEP725_HR_stations.csv", header=TRUE) 
ireland<-read.csv("PEP725_IE/PEP725_IE_Sambucus.csv", header=TRUE)
ire.station<-read.csv("PEP725_IE/PEP725_IE_stations.csv", header=TRUE) 
german2<-read.csv("PEP725_IP/PEP725_IP_Sambucus.csv", header=TRUE) ### ADDED!
german2.station<-read.csv("PEP725_IP/PEP725_IP_stations.csv", header=TRUE) ### ADDED!
italy<-read.csv("PEP725_IT/PEP725_IT_Sambucus.csv", header=TRUE) ### ADDED!
italy.station<-read.csv("PEP725_IT/PEP725_IT_stations.csv", header=TRUE) #ADDED!
mont<-read.csv("PEP725_ME/PEP725_ME_Sambucus.csv", header=TRUE) # ADDED!
mont.station<-read.csv("PEP725_ME/PEP725_ME_stations.csv", header=TRUE) # ADDED!
mace<-read.csv("PEP725_MK/PEP725_MK_Sambucus.csv", header=TRUE) # ADDED!
mace.station<-read.csv("PEP725_MK/PEP725_MK_stations.csv", header=TRUE) # ADDED!
neth<-read.csv("PEP725_NL/PEP725_NL_Sambucus.csv", header=TRUE)
neth.station<-read.csv("PEP725_NL/PEP725_NL_stations.csv", header=TRUE)
slov<-read.csv("PEP725_SI/PEP725_SI_Sambucus.csv", header=TRUE) # ADDED!
slov.station<-read.csv("PEP725_SI/PEP725_SI_stations.csv", header=TRUE) # ADDED!
vakia<-read.csv("PEP725_SK/PEP725_SK_Sambucus.csv", header=TRUE) # ADDED!
vakia.station<-read.csv("PEP725_SK/PEP725_SK_stations.csv", header=TRUE) #ADDED!
engl<-read.csv("PEP725_UK/PEP725_UK_Sambucus.csv", header=TRUE)
engl.station<-read.csv("PEP725_UK/PEP725_UK_stations.csv", header=TRUE)

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

it<-italy%>%filter(BBCH<=19)
it<-full_join(it, italy.station)
it<-dplyr::select(it, -NAME)
it<-na.omit(it)

nl<-neth%>%filter(BBCH<=19)
nl<-full_join(nl, neth.station)
nl<-dplyr::select(nl, -NAME)
nl<-na.omit(nl)

d<-bind_rows(d, nl)

mk<-mace%>%filter(BBCH<=19)
mk<-full_join(mk, mace.station)
mk<-dplyr::select(mk, -NAME)
mk<-na.omit(mk)

d<-bind_rows(d, mk)

es<-spain%>%filter(BBCH<=19)
es<-full_join(es, spain.station)
es<-dplyr::select(es, -NAME)
es<-na.omit(es)

d<-bind_rows(d, es)

uk<-engl%>%filter(BBCH<=19)
uk<-full_join(uk, engl.station)
uk<-dplyr::select(uk, -NAME)
uk<-na.omit(uk)

d<-bind_rows(d, uk)
d<-dplyr::select(d, -X, -X.1)
d<-na.omit(d)
d$species<-"SAMNIG"

write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/bbch_region_sambucus.csv", row.names = FALSE)


