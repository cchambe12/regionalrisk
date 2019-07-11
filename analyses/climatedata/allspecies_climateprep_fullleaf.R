### Prepare all data for climate data...
### 20 November 2018

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(raster)
library(reshape2)
library(data.table)

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

df<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(bb=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df$lo<-df$bb+12
df<-dplyr::select(df, bb, year, PEP_ID, lat, long, lo, species)
df$pep.year<-paste(df$year, df$PEP_ID, df$species)

dxx<-data_frame()
days.btw<-array()
#pb <- txtProgressBar(min = 1, max = nrow(df), style = 3)

#foo<-df[sample(nrow(df), 20), ]
df<-df[(df$bb>=0),]
df<-na.omit(df)

days.btw <- Map(seq, df$bb, df$lo, by = 1)

dxx <- data.frame(pep.year = rep.int(df$pep.year, vapply(days.btw, length, 1L)), 
                  doy = do.call(c, days.btw))

dxx$year<-as.numeric(substr(dxx$pep.year,0,4))
dxx$PEP_ID<-gsub("^\\S+\\s+|\\s+\\S+$", "", dxx$pep.year)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

dxx$species<-substrRight(dxx$pep.year, 6)
coords<-df%>%dplyr::select(lat, long, pep.year)
dxx<-full_join(dxx, coords)

dxx<-dxx[!duplicated(dxx),]
dxx<-dplyr::select(dxx, -pep.year)
x<-paste(dxx$year, dxx$doy)
dxx$date<-as.Date(strptime(x, format="%Y %j"))
dxx$Date<- as.character(dxx$date)
dxx<-na.omit(dxx)

write.csv(df, file="~/Documents/git/regionalrisk/analyses/output/bb_fullleaf.csv", row.names=FALSE)
write.csv(dxx, file="~/Desktop/allspp_climateprep_fullleaf.csv", row.names=FALSE)


