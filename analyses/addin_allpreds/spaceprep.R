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
#fs<-read.csv("output/fs_allspp_longtemps.csv", header=TRUE)
#fs<-read.csv("output/fs_allspp_dvrlong.csv", header=TRUE)
#fs<-read.csv("output/fs_allspp_long.csv", header=TRUE)
fs<-read.csv("output/fs_allspp_verylong.csv", header=TRUE)
#fs<-read.csv("output/fs_allspp_fivelong.csv", header=TRUE)


fs<-subset(fs, select=c("lat", "long", "fs.count", "year", "species", "fs"))
mat<-read.csv("output/mat_MAM.csv", header=TRUE)
mat<-subset(mat, year>1950)
elev<-read.csv("output/fs_bb_sitedata.csv", header=TRUE)
elev<-subset(elev, year>1950)
nao<-read.csv("output/nao_NovApr.csv", header=TRUE)
nao<-subset(nao, year>1950)

elev<-dplyr::select(elev, species, LAT, LON, ALT)
elev<-elev%>%dplyr::rename(lat=LAT)%>%dplyr::rename(long=LON)%>%dplyr::rename(elev=ALT)
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
dist<-dist%>%dplyr::rename(long=LONG)%>%dplyr::rename(lat=LAT)

fspreds<-full_join(fspreds, dist)


#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_longtemps_allpred.csv", row.names = FALSE)
#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_dvrlong_allpred.csv", row.names = FALSE)
#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_long_allpred.csv", row.names = FALSE)
write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_verylong_allpred.csv", row.names = FALSE)
#write.csv(fspreds, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_fivelong_allpred.csv", row.names = FALSE)


