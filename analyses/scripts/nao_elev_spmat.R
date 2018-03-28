## Started 28 March 2018 ##
## By Cat ##

## Using elevation and NAO index and spring mean annual temperature ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(dplyr)
library(tidyr)
library(brms)
library(ggstance)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data
#bb<-read.csv("output/fs_matspspace.csv", header=TRUE)
bb<-read.csv("output/fs_matspring.csv", header=TRUE)
mat<-read.csv("output/fs_bb_sitedata.csv", header=TRUE)

## fix KNMI Climate Explorer data for NAO - https://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/cpc_nao&STATION=CPC_NAO&TYPE=i&id=someone@somewhere
#setwd("~/Documents/git/regionalrisk")
#nao<-read.delim("data/icpc_nao.dat.txt", header=TRUE)
#write.csv(nao, "~/Documents/git/regionalrisk/analyses/output/icpc_nao.csv", row.names=FALSE)
nao<-read.csv("output/icpc_nao.csv", header=TRUE)
nf<-nao%>%gather(month, index, -year)
nf$m.index<-ave(nf$index, nf$year)
nx<-dplyr::select(nf, year, m.index)
nx<-nx[!duplicated(nx),]
nx<-filter(nx, year<=2016)

#### Get elevation information
bb<-bb%>%rename(sp.temp=pre.bb)
bb<-dplyr::select(bb, -fs.count, -PEP_ID)
bb<-bb[!duplicated(bb),]
mat<-mat%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
mat<-dplyr::select(mat, species, lat, long, elev)
mat<-mat[!duplicated(mat),]
d<-inner_join(bb, mat)
d<-inner_join(d, nx)

#d$cc<-NA
#d$cc<-ifelse(d$year<=1970&d$year>=1950, 0, d$cc)
#d$cc<-ifelse(d$year>=1996&d$year<=2016, 1, d$cc)
#d<-d[!is.na(d$cc),]
d$cc<-ifelse(d$year>=1950&d$year<=1983, 0, 1)
d$elev<-d$elev+1
d$fs.num<-ave(d$fs, d$lat.long, d$species, d$cc, FUN=sum)
d$sp.temp<-ave(d$sp.temp, d$cc, d$lat.long)
fs.cc<-dplyr::select(d, fs.num, sp.temp, elev, cc, species)
fs.cc$species<-as.numeric(as.factor(fs.cc$species))
fs.cc<-fs.cc[!duplicated(fs.cc),]
fs.cc<-fs.cc[!is.na(fs.cc$sp.temp),]
fs.cc<-fs.cc[!is.na(fs.cc$elev),]
fs.cc$elev<-ifelse(fs.cc$elev<=500, 0, 1)
fs.cc<-fs.cc[!duplicated(fs.cc),]

fit<-stan_glmer(fs.num~sp.temp+elev+cc+(1|species), data=fs.cc, family=poisson, chains=2)
## nothing with elevation... spatial autocorrelation issues?


####### Try NAO model... ########
nao.mod<-dplyr::select(d, fs, sp.temp, elev, species, m.index)
nao.mod<-nao.mod[!duplicated(nao.mod),]
nao.mod<-nao.mod[!is.na(nao.mod$sp.temp),]
nao.mod<-nao.mod[]

