### Prepping Tmin for species for boxplots
##  12 Sept 2018 - Cat

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")
aes<-read.csv("output/aeship_data_dvr.csv", header=TRUE)
ag<-read.csv("output/alnglu_data_dvr.csv", header=TRUE)
bp<-read.csv("output/betpen_data_dvr.csv", header=TRUE)
fsyl<-read.csv("output/fagsyl_data_dvr.csv", header=TRUE)
fex<-read.csv("output/fraexc_data_dvr.csv", header=TRUE)
qr<-read.csv("output/querob_data_dvr.csv", header=TRUE)


aes$lat.long<-paste(aes$lat, aes$long)
aes$Tmin<-ave(aes$Tmin, aes$lat.long, aes$year)
aes$species<-"AESHIP"
aes<-subset(aes, select=c("lat", "long", "year", "species", "Tmin"))
aes<-aes[!duplicated(aes),]

ag$lat.long<-paste(ag$lat, ag$long)
ag$Tmin<-ave(ag$Tmin, ag$lat.long, ag$year)
ag$species<-"ALNGLU"
ag<-subset(ag, select=c("lat", "long", "year", "species", "Tmin"))
ag<-ag[!duplicated(ag),]

d<-full_join(aes, ag)

bp$lat.long<-paste(bp$lat, bp$long)
bp$Tmin<-ave(bp$Tmin, bp$lat.long, bp$year)
bp$species<-"BETPEN"
bp<-subset(bp, select=c("lat", "long", "year", "species", "Tmin"))
bp<-bp[!duplicated(bp),]

d<-full_join(bp, d)

fsyl$lat.long<-paste(fsyl$lat, fsyl$long)
fsyl$Tmin<-ave(fsyl$Tmin, fsyl$lat.long, fsyl$year)
fsyl$species<-"FAGSYL"
fsyl<-subset(fsyl, select=c("lat", "long", "year", "species", "Tmin"))
fsyl<-fsyl[!duplicated(fsyl),]

d<-full_join(fsyl, d)

fex$lat.long<-paste(fex$lat, fex$long)
fex$Tmin<-ave(fex$Tmin, fex$lat.long, fex$year)
fex$species<-"FRAEXC"
fex<-subset(fex, select=c("lat", "long", "year", "species", "Tmin"))
fex<-fex[!duplicated(fex),]

d<-full_join(fex, d)

qr$lat.long<-paste(qr$lat, qr$long)
qr$Tmin<-ave(qr$Tmin, qr$lat.long, qr$year)
qr$species<-"QUEROB"
qr<-subset(qr, select=c("lat", "long", "year", "species", "Tmin"))
qr<-qr[!duplicated(qr),]

d<-full_join(qr, d)
tm<-d ## full data not averaged during DVR
write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/tminprep_boxplots_dvr.csv", row.names=FALSE)
write.csv(tm, file="~/Documents/git/regionalrisk/analyses/output/tminfull.csv", row.names = FALSE)
