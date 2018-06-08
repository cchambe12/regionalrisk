### Additional Plots for Regional Risk
## Looking at MAT and NAO plus others
## 8 June 2018 - Cat

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)
library(brms)


# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")

bb<-read.csv("output/regrisk.cleaned.csv", header=TRUE)
dxx<-read.csv("output/fs_matspring.csv", header=TRUE)
x<-read.csv("output/fs_bb_sitedata.csv", header = TRUE)

dxx$fs.yr<-ave(dxx$fs, dxx$year, FUN=sum)
dxx$fs.yrspp<-ave(dxx$fs, dxx$species, dxx$year, FUN=sum)
dxx$spp.prop<-NA
dxx$spp.sites<-as.numeric(ave(dxx$lat.long, dxx$year, dxx$species, FUN=length))
dxx$spp.prop<-ifelse(dxx$species=="AESHIP", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="ALNGLU", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="BETPEN", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="FAGSYL", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="FRAEXC", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="QUEROB", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.ave<-ave(dxx$spp.prop, dxx$species, FUN=median)

dxx$num.sites<-as.numeric(ave(dxx$lat.long, dxx$year, FUN=length))
dxx$fs.prop<-dxx$fs.yr/dxx$num.sites

dxx$fs.ave<-ave(dxx$fs.prop, FUN=median)

x<-x%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
x<-dplyr::select(x, species, year, bb, bb.yr, lat, long, elev)
x<-x[!duplicated(x),]

dxx<-subset(dxx, select=c("species", "year", "lat", "long", "fs.prop", "spp.prop", "num.sites"))

df<-inner_join(bb, dxx)
df<-inner_join(df, x)
df<-df[!duplicated(df),]

### Some plots!
ggplot(df, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)), stat="smooth", method="auto")

df$mat<-as.integer(round(df$sp.temp, digits=0))
df$matx<-NA
df$matx<-ifelse(df$mat<=-10, 1, df$matx)
df$matx<-ifelse(df$mat>-10&df$mat<=-6, 2, df$matx)
df$matx<-ifelse(df$mat>-6&df$mat<=-2, 3, df$matx)
df$matx<-ifelse(df$mat>-2&df$mat<=2, 4, df$matx)
df$matx<-ifelse(df$mat>2&df$mat<=6, 5, df$matx)
df$matx<-ifelse(df$mat>6&df$mat<=10, 6, df$matx)
df$matx<-ifelse(df$mat>10, 7, df$matx)
df$matx<-as.integer(df$matx)

df$fs<-ave(df$fs.count, df$matx)
df$fs<-as.integer(round(df$fs, digits=0))

ggplot(df, aes(x=fs.count, y=sp.temp)) + geom_bar(aes(col=as.factor(cc)), position="dodge")





