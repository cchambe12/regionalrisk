## Started 8 March 2018 ##
## By Cat ##

## Working on extra plots for raw data ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")
aes<-read.csv("output/bbch_region_aesculus.csv", header=TRUE)
ag<-read.csv("output/bbch_region_alnus.csv", header=TRUE)
bp<-read.csv("output/bbch_region_betula.csv", header=TRUE)
fsyl<-read.csv("output/bbch_region_fagus.csv", header=TRUE)
fex<-read.csv("output/bbch_region_fraxinus.csv", header=TRUE)
qr<-read.csv("output/bbch_region_quercus.csv", header=TRUE)

aes<-aes%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
aes$bb<-aes$lo-12
aes$species<-"AESHIP"

ag<-ag%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
ag$bb<-ag$lo-12
ag$species<-"ALNGLU"

d<-full_join(aes, ag)

bp<-bp%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
bp$bb<-bp$lo-12
bp$species<-"BETPEN"

d<-full_join(d, bp)

fsyl<-fsyl%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
fsyl$bb<-fsyl$lo-12
fsyl$species<-"FAGSYL"

d<-full_join(d, fsyl)

fex<-fex%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
fex$bb<-fex$lo-12
fex$species<-"FRAEXC"

d<-full_join(d, fex)

qr<-qr%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
qr$bb<-qr$lo-12
qr$species<-"QUEROB"

d<-full_join(d, qr)

d<-dplyr::select(d, lat, long, year, species, bb)
d<-d[!duplicated(d),]

write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/BBdata.csv", row.names=FALSE)
