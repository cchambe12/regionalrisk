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

aes$species <- "AESHIP"
ag$species <- "ALNGLU"
bp$species <- "BETPEN"
fsyl$species <- "FAGSYL"
fex$species <- "FRAEXC"
qr$species <- "QUEROB"

d<- full_join(aes, ag)
d<- full_join(d, bp)
d<- full_join(d, fsyl)
d<- full_join(d, fex)
d<- full_join(d, qr)


d$lat.long<-paste(d$lat, d$long)
d$bb<-ave(d$doy, d$lat.long, d$year, d$species, FUN=first)
d$lo<-d$bb+12
d<-d[!(d$doy<d$bb),]
d<-d[!duplicated(d),]
d<-na.omit(d)
d$Tmin<-ave(d$Tmin, d$lat.long, d$year)
d<-subset(d, select=c("lat", "long", "year", "species", "Tmin"))
d<-d[!duplicated(d),]

write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/tminprep_boxplots_fullleaf.csv", row.names=FALSE)
