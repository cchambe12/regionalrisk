## 8 August 2017 - Cat
## Cleaning up PEP data for Regional Risk paper
# Tilia platyphyllos


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
setwd("~/Documents/git/regionalrisk/data/tilpla")
austria<-read.csv("PEP725_AT/PEP725_AT_Tilia.csv", header=TRUE)
aust.station<-read.csv("PEP725_AT/PEP725_AT_stations.csv", header=TRUE)
bos<-read.csv("PEP725_BA/PEP725_BA_Tilia.csv", header=TRUE)
bos.station<-read.csv("PEP725_BA/PEP725_BA_stations.csv", header=TRUE)
switz<-read.csv("PEP725_CH/PEP725_CH_Tilia.csv", header=TRUE)
switz.station<-read.csv("PEP725_CH/PEP725_CH_stations.csv", header=TRUE)
germany<-read.csv("PEP725_DE/PEP725_DE_Tilia.csv", header=TRUE)
germ.station<-read.csv("PEP725_DE/PEP725_DE_stations.csv", header=TRUE)
mace<-read.csv("PEP725_MK/PEP725_MK_Tilia.csv", header=TRUE) # ADDED!
mace.station<-read.csv("PEP725_MK/PEP725_MK_stations.csv", header=TRUE) # ADDED!
mont<-read.csv("PEP725_ME/PEP725_ME_Tilia.csv", header=TRUE) 
mont.station<-read.csv("PEP725_ME/PEP725_ME_stations.csv", header=TRUE)

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

gm<-germany%>%filter(BBCH<=19)
gm<-full_join(gm, germ.station)
gm<-dplyr::select(gm, -NAME)
gm<-na.omit(gm)

d<-bind_rows(d, gm)

me<-mont%>%filter(BBCH<=19)
me<-full_join(me, mont.station)
me<-dplyr::select(me, -NAME)
me<-na.omit(me)

d<-bind_rows(d, me)

mk<-mace%>%filter(BBCH<=19)
mk<-full_join(mk, mace.station)
mk<-dplyr::select(mk, -NAME)
mk<-na.omit(mk)

d<-bind_rows(d, mk)
d<-dplyr::select(d, -X, -X.1)
d<-na.omit(d)
d$species<-"TILPLA"

write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/bbch_region_tilpla.csv", row.names = FALSE)




