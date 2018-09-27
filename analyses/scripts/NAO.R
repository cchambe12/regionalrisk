## Started 27 Sept 2018 ##
## By Cat ##

## Using elevation and NAO index and spring mean annual temperature ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


#library(rstan)
library(ggplot2)
#library(shinystan)
#library(bayesplot)
library(rstanarm)
library(dplyr)
library(tidyr)
library(brms)
# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data

## fix KNMI Climate Explorer data for NAO - https://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/cpc_nao&STATION=CPC_NAO&TYPE=i&id=someone@somewhere
#setwd("~/Documents/git/regionalrisk")
#nao<-read.delim("data/icpc_nao_daily.dat.txt", header=TRUE)
#write.csv(nao, "~/Documents/git/regionalrisk/analyses/output/icpc_nao_daily.csv", row.names=FALSE)
nao<-read.csv("output/icpc_nao_daily.csv", header=TRUE)

nov<-nao[(nao$month>=11 | nao$month<=4),]
nov$nao<-ave(nov$nao, nov$month, nov$year)
nov<-subset(nov, select=c(year, month, nao))
nov<-nov[!duplicated(nov),]

nov<-nov[!(nov$year==1950 & nov$month<=4),]
nov<-nov[!(nov$year>2017),]
nov<-nov[!(nov$year==2017 & nov$month>4),]
nov$rep<-rep(1951:2017, each=6)

nov$nov.nao<-ave(nov$nao, nov$rep)
nov<-nov[!(nov$rep==2017),]

nov<-subset(nov, select=c(rep, nov.nao))
nov<-nov[!duplicated(nov),]

nov<-nov%>%rename(year=rep)%>%rename(nao=nov.nao)

#write.csv(nov, file=("output/nao_NovApr.csv"), row.names=FALSE)

fs<-read.csv("output/regrisk.fixed.csv", header=TRUE)
bb<-read.csv("output/fs_bb_sitedata.csv", header=TRUE)
bb<-subset(bb, year>=1951)
bb<-subset(bb, select=c(species, year, bb, LAT, LON))
bb<-bb%>%rename(lat=LAT)%>%rename(long=LON)
bb<-bb[!duplicated(bb),]

fs<-full_join(bb, fs)
fs<-fs[!duplicated(fs),]
fs<-na.omit(fs)

mod<-lm(mst~nao*cc, data=fs)

#lm(formula = mst ~ m.index * cc, data = bb)
#coef.est coef.se
#(Intercept)  7.68     0.00  
#m.index      0.59     0.01  
#cc           0.85     0.00  
#m.index:cc  -0.01     0.01  
#---
#residual sd = 1.47, R-Squared = 0.11

#lm(formula = mst ~ nao * cc, data = fs) ## Nov-Apr
#coef.est coef.se
#(Intercept) 7.62     0.00   
#nao         0.90     0.01   
#cc          0.66     0.00   
#nao:cc      0.38     0.01   
#---
#residual sd = 1.46, R-Squared = 0.13

mod1<-lm(bb~m.index*cc, data=fs)

#lm(formula = bb ~ m.index * cc, data = fs)
#coef.est coef.se
#(Intercept) 107.19     0.02 
#m.index      -2.20     0.06 
#cc           -5.46     0.03 
#m.index:cc    0.52     0.09 
#---
#residual sd = 13.68, R-Squared = 0.05

#lm(formula = bb ~ nao * cc, data = fs) # Nov-Apr
#coef.est coef.se
#(Intercept) 107.34     0.02 
#nao          -6.61     0.09 
#cc           -4.52     0.04 
#nao:cc        1.23     0.12 
#---
#  n = 756227, k = 4
#residual sd = 13.62, R-Squared = 0.05











