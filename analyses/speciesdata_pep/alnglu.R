## 8 August 2017 - Cat
## Cleaning up PEP data for Regional Risk paper
# Alnus glutinosa


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
setwd("~/Documents/git/regionalrisk/data/alnus")
bos<-read.csv("PEP725_BA/PEP725_BA_Alnus.csv", header=TRUE)
bos.station<-read.csv("PEP725_BA/PEP725_BA_stations.csv", header=TRUE)
czech<-read.csv("PEP725_CZ/PEP725_CZ_Alnus.csv", header=TRUE)
czech.station<-read.csv("PEP725_CZ/PEP725_CZ_stations.csv", header=TRUE)
croatia<-read.csv("PEP725_HR/PEP725_HR_Alnus.csv", header=TRUE) 
croa.station<-read.csv("PEP725_HR/PEP725_HR_stations.csv", header=TRUE) 
spain<-read.csv("PEP725_ES/PEP725_ES_Alnus.csv", header=TRUE)
spain.station<-read.csv("PEP725_ES/PEP725_ES_stations.csv", header=TRUE)
germany<-read.csv("PEP725_DE/PEP725_DE_Alnus.csv", header=TRUE)
germ.station<-read.csv("PEP725_DE/PEP725_DE_stations.csv", header=TRUE)
mont<-read.csv("PEP725_ME/PEP725_ME_Alnus.csv", header=TRUE) 
mont.station<-read.csv("PEP725_ME/PEP725_ME_stations.csv", header=TRUE)
neth<-read.csv("PEP725_NL/PEP725_NL_Alnus.csv", header=TRUE)
neth.station<-read.csv("PEP725_NL/PEP725_NL_stations.csv", header=TRUE)
slov<-read.csv("PEP725_SI/PEP725_SI_Alnus.csv", header=TRUE)
slov.station<-read.csv("PEP725_SI/PEP725_SI_stations.csv", header=TRUE)
engl<-read.csv("PEP725_UK/PEP725_UK_Alnus.csv", header=TRUE)
engl.station<-read.csv("PEP725_UK/PEP725_UK_stations.csv", header=TRUE)

bs<-bos%>%filter(BBCH<=19)
bs<-full_join(bs, bos.station)
bs<-dplyr::select(bs, -NAME)
bs<-na.omit(bs)

cz<-czech%>%filter(BBCH<=19)
cz<-full_join(cz, czech.station)
cz<-dplyr::select(cz, -NAME)
cz<-na.omit(cz)

d<-bind_rows(bs, cz)

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

me<-mont%>%filter(BBCH<=19)
me<-full_join(me, mont.station)
me<-dplyr::select(me, -NAME)
me<-na.omit(me)

d<-bind_rows(d, me)

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

uk<-engl%>%filter(BBCH<=19)
uk<-full_join(uk, engl.station)
uk<-dplyr::select(uk, -NAME)
uk<-na.omit(uk)

d<-bind_rows(d, uk)
d<-dplyr::select(d, -X, -X.1)
#d<-na.omit(d)
d$species<-"ALNGLU"
#write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/bbch_region_alnus.csv", row.names = FALSE)
