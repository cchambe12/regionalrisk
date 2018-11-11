## 12 January 2018 - Cat  
## Diving back into Regional Risk Chapter -- aim of this script is to start analysis
### NOPE! Need to clean the dataframes further -- find number of false springs

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

### Load data
setwd("~/Documents/git/regionalrisk/analyses/output")
bp<-read.csv("betpen_data.csv", header=TRUE)
fs<-read.csv("fagsyl_data.csv", header=TRUE)
fe<-read.csv("fraexc_data.csv", header=TRUE)
qr<-read.csv("querob_data.csv", header=TRUE)
ah<-read.csv("aeship_data.csv", header=TRUE)
ag<-read.csv("alnglu_data_dvr.csv", header=TRUE)

#d<-read.csv("~/Documents/git/springfreeze/input/Budburst.clean.csv",header=TRUE)
#tx<-c("CL0", "CS0", "WL0", "WS0")
#d<-d[(d$treatcode%in%tx),]
#d<-subset(d, select=c("sp", "lday", "bday", "treatcode"))
#d<-na.omit(d)
#d$dvr<-d$lday-d$bday
#d$dvr.sp<-ave(d$dvr, d$sp)
#dx<-subset(d, select=c("sp", "dvr.sp", "treatcode", "dvr"))
#dx<-dx[!duplicated(dx),]
#dx$avg.dvr<-ave(dx$dvr, dx$sp)

## Start looking at the data a bit...
bp$fs<- ifelse(bp$Tmin<=-2.2, 1, 0)
bp$lo<-ave(bp$doy, bp$PEP_ID, bp$year, FUN=last)
bp$bb<-bp$lo-10 # Based on Danf's BETPAP - most closely related. Choose WL0 based on keeping all species consistent
bp<-bp[!(bp$doy<bp$bb),]
bp<-bp[!duplicated(bp),]
bp$fs.count<- ave(bp$fs, bp$PEP_ID, bp$year, FUN=sum)
betpen<-bp%>%dplyr::select(lat, long, PEP_ID, fs.count, year)
betpen<-betpen[!duplicated(betpen),]
betpen<-na.omit(betpen)
betpen$species<-"BETPEN"
betpen$fs<-ifelse(betpen$fs.count>=1, 1, 0) # 155251
#betpen<-dplyr::select(betpen, -fs.count)

ah$fs<- ifelse(ah$Tmin<=-2.2, 1, 0)
ah$lo<-ave(ah$doy, ah$PEP_ID, ah$year, FUN=last)
ah$bb<-ah$lo-7 # Based on Danf's ACESAC - most closely related. Choose WL0 based on keeping all species consistent
ah<-ah[!(ah$doy<ah$bb),]
ah<-ah[!duplicated(ah),]
ah$fs.count<- ave(ah$fs, ah$PEP_ID, ah$year, FUN=sum)
aeship<-ah%>%dplyr::select(lat, long, PEP_ID, fs.count, year)
aeship<-aeship[!duplicated(aeship),]
aeship<-na.omit(aeship)
aeship$species<-"AESHIP"
aeship$fs<-ifelse(aeship$fs.count>=1, 1, 0)
#aeship<-dplyr::select(aeship, -fs.count) #156836

d<-full_join(betpen, aeship)

ag$fs<- ifelse(ag$Tmin<=-2.2, 1, 0)
ag$lo<-ave(ag$doy, ag$PEP_ID, ag$year, FUN=last)
ag$bb<-ag$lo-15 # Based on Danf's ALNINC - most closely related. Choose WL0 based on keeping all species consistent
ag<-ag[!(ag$doy<ag$bb),]
ag<-ag[!duplicated(ag),]
ag$fs.count<- ave(ag$fs, ag$PEP_ID, ag$year, FUN=sum)
alnglu<-ag%>%dplyr::select(lat, long, PEP_ID, fs.count, year)
alnglu<-alnglu[!duplicated(alnglu),]
alnglu<-na.omit(alnglu)
alnglu$species<-"ALNGLU"
alnglu$fs<-ifelse(alnglu$fs.count>=1, 1, 0)
#alnglu<-dplyr::select(alnglu, -fs.count) #91182

d<-full_join(d, alnglu)

fe$fs<- ifelse(fe$Tmin<=-2.2, 1, 0)
fe$lo<-ave(fe$doy, fe$PEP_ID, fe$year, FUN=last)
fe$bb<-fe$lo-6 # Based on Danf's FRANIG - most closely related. Choose WL0 based on keeping all species consistent
fe<-fe[!(fe$doy<fe$bb),]
fe<-fe[!duplicated(fe),]
fe$fs.count<- ave(fe$fs, fe$PEP_ID, fe$year, FUN=sum)
fraexc<-fe%>%dplyr::select(lat, long, PEP_ID, fs.count, year)
fraexc<-fraexc[!duplicated(fraexc),]
fraexc<-na.omit(fraexc)
fraexc$species<-"FRAEXC"
fraexc$fs<-ifelse(fraexc$fs.count>=1, 1, 0)
#fraexc<-dplyr::select(fraexc, -fs.count) #92665

d<-full_join(d, fraexc)

fs$fs<- ifelse(fs$Tmin<=-2.2, 1, 0)
fs$lo<-ave(fs$doy, fs$year, fs$PEP_ID, FUN=last)
fs$bb<-fs$lo-11 # Based on Danf's FAGGRA - most closely related. Choose WL0 based on keeping all species consistent
fs<-fs[!(fs$doy<fs$bb),]
fs<-fs[!duplicated(fs),]
fs$fs.count<- ave(fs$fs, fs$PEP_ID, fs$year, FUN=sum)
fagsyl<-fs%>%dplyr::select(lat, long, PEP_ID, fs.count, year)
fagsyl<-fagsyl[!duplicated(fagsyl),]
fagsyl<-na.omit(fagsyl)
fagsyl$species<-"FAGSYL"
fagsyl$fs<-ifelse(fagsyl$fs.count>=1, 1, 0)
#fagsyl<-dplyr::select(fagsyl, -fs.count) #129133

d<-full_join(d, fagsyl)

qr$fs<- ifelse(qr$Tmin<=-2.2, 1, 0)
qr$lo<-ave(qr$doy, qr$year, qr$PEP_ID, FUN=last)
qr$bb<-qr$lo-9 # Based on Danf's QUEALB - most closely related. Choose WL0 based on keeping all species consistent
qr<-qr[!(qr$doy<qr$bb),]
qr<-qr[!duplicated(qr),]
qr$fs.count<- ave(qr$fs, qr$PEP_ID, qr$year, FUN=sum)
querob<-qr%>%dplyr::select(lat, long, PEP_ID, fs.count, year)
querob<-querob[!duplicated(querob),]
querob<-na.omit(querob)
querob$species<-"QUEROB"
querob$fs<-ifelse(querob$fs.count>=1, 1, 0)
#querob<-dplyr::select(querob, -fs.count) # 131635

d<-full_join(d, querob)

write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/fs_dvr_cleaned.csv", row.names = FALSE)

