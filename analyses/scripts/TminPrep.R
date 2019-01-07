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
aes<-read.csv("output/aeship_data.csv", header=TRUE)
ag<-read.csv("output/alnglu_data_dvr.csv", header=TRUE)
bp<-read.csv("output/betpen_data.csv", header=TRUE)
fsyl<-read.csv("output/fagsyl_data.csv", header=TRUE)
fex<-read.csv("output/fraexc_data.csv", header=TRUE)
qr<-read.csv("output/querob_data.csv", header=TRUE)


aes$lat.long<-paste(aes$lat, aes$long)
aes$lo<-ave(aes$doy, aes$lat.long, aes$year, FUN=last)
aes$bb<-aes$lo-11 # Based on Danf's BETPAP - most closely related. Choose WL0 based on keeping all species consistent
aes<-aes[!(aes$doy<aes$bb),]
aes<-aes[!duplicated(aes),]
aes<-na.omit(aes)
aes$Tmin<-ave(aes$Tmin, aes$lat.long, aes$year)
aes$species<-"AESHIP"
aes<-subset(aes, select=c("lat", "long", "year", "species", "Tmin"))
aes<-aes[!duplicated(aes),]

ag$lat.long<-paste(ag$lat, ag$long)
ag$lo<-ave(ag$doy, ag$lat.long, ag$year, FUN=last)
ag$bb<-ag$lo-12 # Based on Danf's ALNINC - most closely related. Choose WL0 based on keeping all species consistent
ag<-ag[!(ag$doy<ag$bb),]
ag<-ag[!duplicated(ag),]
ag<-na.omit(ag)
ag$Tmin<-ave(ag$Tmin, ag$lat.long, ag$year)
ag$species<-"ALNGLU"
ag<-subset(ag, select=c("lat", "long", "year", "species", "Tmin"))
ag<-ag[!duplicated(ag),]

d<-full_join(aes, ag)

bp$lat.long<-paste(bp$lat, bp$long)
bp$lo<-ave(bp$doy, bp$lat.long, bp$year, FUN=last)
bp$bb<-bp$lo-11 # Based on Danf's BETPAP - most closely related. Choose WL0 based on keeping all species consistent
bp<-bp[!(bp$doy<bp$bb),]
bp<-bp[!duplicated(bp),]
bp<-na.omit(bp)
bp$Tmin<-ave(bp$Tmin, bp$lat.long, bp$year)
bp$species<-"BETPEN"
bp<-subset(bp, select=c("lat", "long", "year", "species", "Tmin"))
bp<-bp[!duplicated(bp),]

d<-full_join(bp, d)

fsyl$lat.long<-paste(fsyl$lat, fsyl$long)
fsyl$lo<-ave(fsyl$doy, fsyl$year, fsyl$lat.long, FUN=last)
fsyl$bb<-fsyl$lo-5 # Based on Danf's FAGGRA - most closely related. Choose WL0 based on keeping all species consistent
fsyl<-fsyl[!(fsyl$doy<fsyl$bb),]
fsyl<-fsyl[!duplicated(fsyl),]
fsyl<-na.omit(fsyl)
fsyl$Tmin<-ave(fsyl$Tmin, fsyl$lat.long, fsyl$year)
fsyl$species<-"FAGSYL"
fsyl<-subset(fsyl, select=c("lat", "long", "year", "species", "Tmin"))
fsyl<-fsyl[!duplicated(fsyl),]

d<-full_join(fsyl, d)

fex$lat.long<-paste(fex$lat, fex$long)
fex$lo<-ave(fex$doy, fex$lat.long, fex$year, FUN=last)
fex$bb<-fex$lo-7 # Based on Danf's FRANIG - most closely related. Choose WL0 based on keeping all species consistent
fex<-fex[!(fex$doy<fex$bb),]
fex<-fex[!duplicated(fex),]
fex<-na.omit(fex)
fex$Tmin<-ave(fex$Tmin, fex$lat.long, fex$year)
fex$species<-"FRAEXC"
fex<-subset(fex, select=c("lat", "long", "year", "species", "Tmin"))
fex<-fex[!duplicated(fex),]

d<-full_join(fex, d)

qr$lat.long<-paste(qr$lat, qr$long)
qr$lo<-ave(qr$doy, qr$year, qr$lat.long, FUN=last)
qr$bb<-qr$lo-7 # Based on Danf's QUEALB - most closely related. Choose WL0 based on keeping all species consistent
qr<-qr[!(qr$doy<qr$bb),]
qr<-qr[!duplicated(qr),]
qr<-na.omit(qr)
qr$Tmin<-ave(qr$Tmin, qr$lat.long, qr$year)
qr$species<-"QUEROB"
qr<-subset(qr, select=c("lat", "long", "year", "species", "Tmin"))
qr<-qr[!duplicated(qr),]

d<-full_join(qr, d)
tm<-d ## full data not averaged during DVR
write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/tminprep_boxplots_dvr.csv", row.names=FALSE)
write.csv(tm, file="~/Documents/git/regionalrisk/analyses/output/tminfull.csv", row.names = FALSE)
