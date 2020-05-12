## Started 5 March 2018 ##
## By Cat ##

## Integrating all predictor information before evaluating the space parameter ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data - must choose the dataframe for specific analyses
#fs<-read.csv("output/fs_allspp_dvr.csv", header=TRUE)
#fs<-read.csv("output/fs_allspp_five.csv", header=TRUE)
#fs<-read.csv("output/fs_allspp_original.csv", header=TRUE)
#fs<-read.csv("output/fs_allspp_fullleaf.csv", header=TRUE)
fs<-read.csv("output/fs_allspp_midleaf.csv", header=TRUE)


fs<-subset(fs, select=c("lat", "long", "fs.count", "year", "species", "fs"))
mat<-read.csv("output/mat_MAM.csv", header=TRUE)
mat<-subset(mat, year>1950)
elev<-read.csv("output/fs_bb_sitedata.csv", header=TRUE)
elev<-subset(elev, year>1950)
nao<-read.csv("output/nao_NovApr.csv", header=TRUE)
nao<-subset(nao, year>1950)

elev<-dplyr::select(elev, species, LAT, LON, ALT)
elev<-elev%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
elev$lat.long<-paste(elev$lat, elev$long)
elev<-elev[!duplicated(elev),]
matelev<-full_join(elev, mat)


#### Get elevation information
matelev$cc<-ifelse(matelev$year<=1983&matelev$year>1950, 0, 1)

fs<-dplyr::select(fs, lat, long, species, fs.count, year)
fs<-fs[!duplicated(fs),]
fspreds<-full_join(matelev, fs)
fspreds<-na.omit(fspreds)
fspreds<-fspreds[!duplicated(fspreds),]

fspreds$elev<-ave(fspreds$elev, fspreds$lat.long)
fspreds<-fspreds[!duplicated(fspreds),]


nao<-dplyr::select(nao, year, nao)
nao<-nao[!duplicated(nao),]


fspreds<-full_join(fspreds, nao)

dist<-read.csv("output/dist_wgs.csv", header=TRUE)
dist<-dist%>%rename(long=LONG)%>%rename(lat=LAT)

fspreds<-full_join(fspreds, dist)


#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_dvr_allpred.csv", row.names = FALSE)
#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_orig_allpred.csv", row.names = FALSE)
#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_five_allpred.csv", row.names = FALSE)
#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_fullleaf_allpred.csv", row.names = FALSE)
#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_midleaf_allpred.csv", row.names = FALSE)


