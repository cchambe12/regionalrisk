#### Started 13 May 2020 by Cat
## Looking at the time between BBCH 11 and BBCH 13 to determine appropriate timeframe to assess

### Full model currently has 755088 rows of data across 6 species and 11648 sites
## BBCH 13 only has 12417 rows of data across 3 species and 153 sites
#  But let's use this information to build a model to give us an idea of DVR length

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/Documents/git/regionalrisk/analyses")
aes<-read.csv("output/bbch_region_aesculus.csv", header=TRUE)
aln<-read.csv("output/bbch_region_alnus.csv", header=TRUE)
bet<-read.csv("output/bbch_region_betula.csv", header=TRUE)
fsyl<-read.csv("output/bbch_region_fagus.csv", header=TRUE)
fra<-read.csv("output/bbch_region_fraxinus.csv", header=TRUE)
que<-read.csv("output/bbch_region_quercus.csv", header=TRUE)

d<-full_join(aes, aln)
d<-full_join(d, bet)
d<-full_join(d, fsyl)
d<-full_join(d, fra)
d<-full_join(d, que)

eleven<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(bb=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
eleven$BBCH <- NULL
eleven$lat.long <- paste(eleven$lat, eleven$long, sep="_")

thirteen<-d%>%
  filter(BBCH==13)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
thirteen$BBCH <- NULL
thirteen$lat.long <- paste(thirteen$lat, thirteen$long, sep="_")

sites13 <- sort(unique(thirteen$PEP_ID))
overlap <- eleven[(eleven$PEP_ID%in%sites13),] ## 0 rows of data

### Maybe try based on lat long...
sites13 <- sort(unique(thirteen$lat.long))
overlap <- eleven[(eleven$lat.long%in%sites13),] ## 0 rows of data

fifteen<-d%>%
  filter(BBCH==15)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
fifteen$BBCH <- NULL
fifteen$lat.long <- paste(fifteen$lat, fifteen$long, sep="_")

sites15 <- sort(unique(fifteen$PEP_ID))
overlap <- eleven[(eleven$PEP_ID%in%sites15),] ## some overlap!

bbandlo <- left_join(fifteen, eleven) ### but no overlap in years... bummer. 

