## Started 5 March 2018 ##
## By Cat ##

## Using space parameter from residuals of eigenvectors ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(ggplot2)
library(rstanarm)
library(dplyr)
library(tidyr)
library(brms)
library(jtools)


# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data
#dx<-read.csv("output/fs_space_new.csv", header=TRUE)
#xx<-read.csv("output/fs_yearsitespp_5.csv", header=TRUE)
#xx<-subset(xx, year>1950)
#dx<-subset(dx, select=c("lat", "long", "lat.long", "distkm", "elev", "mst", "nao", "year", "species", "cc"))
#xx<-read.csv("output/fs_yearsitespp_5.csv", header=TRUE)
xx<-read.csv("output/fs_allspp_dvr.csv", header=TRUE)
#foo<-read.csv("output/fs_allspp_original.csv", header=TRUE)
xx<-subset(xx, select=c("lat", "long", "fs.count", "year", "species", "fs"))
df<-read.csv("output/mat_MAM.csv", header=TRUE)
df<-subset(df, year>1950)
mat<-read.csv("output/fs_bb_sitedata.csv", header=TRUE)
mat<-subset(mat, year>1950)
nao<-read.csv("output/nao_NovApr.csv", header=TRUE)
nao<-subset(nao, year>1950)

### Clean up dataframes a bit
#dx<-dx%>%dplyr::select(lat, long, space)
#dx<-dx[!duplicated(dx),]
#df<-df[!duplicated(df),]
#dx<-full_join(df, dx)
#dx<-dx[!duplicated(dx),]

mat<-dplyr::select(mat, species, LAT, LON, ALT)
mat<-mat%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
mat$lat.long<-paste(mat$lat, mat$long)
mat<-mat[!duplicated(mat),]
bb<-full_join(mat, df)


#### Get elevation information
#bb<-bb%>%rename(sp.temp=pre.bb)
bb$cc<-ifelse(bb$year<=1983&bb$year>1950, 0, 1)

xx<-dplyr::select(xx, lat, long, species, fs.count, year)
xx<-xx[!duplicated(xx),]
bb<-full_join(bb, xx)
bb<-na.omit(bb)
bb<-bb[!duplicated(bb),]

bb$elev<-ave(bb$elev, bb$lat.long)
bb<-bb[!duplicated(bb),]


nao<-dplyr::select(nao, year, nao)
nao<-nao[!duplicated(nao),]


bb<-full_join(bb, nao)

dist<-read.csv("output/dist_wgs.csv", header=TRUE)
dist<-dist%>%rename(long=LONG)%>%rename(lat=LAT)

bb<-full_join(bb, dist)

bb<-full_join(dx, xx)
bb<-bb[!duplicated(bb),]
bb<-na.omit(bb)

buds<-read.csv("output/BBdata.csv", header=TRUE)
buds<-dplyr::select(buds, -PEP_ID)
buds<-buds[!duplicated(buds),]

bb<-full_join(dx, buds)
bb<-bb[!duplicated(bb),]
bb<-na.omit(bb)

write.csv(bb, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_dvr_allpred.csv", row.names = FALSE)
write.csv(bb, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_orig_allpred.csv", row.names = FALSE)

