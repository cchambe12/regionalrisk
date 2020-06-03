# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(raster)
library(reshape2)
library(data.table)

x<-read.csv("~/Desktop/allspp_data.csv", header=TRUE)

x$fs<- ifelse(x$Tmin<=-5, 1, 0)
x$lo<-ave(x$doy, x$PEP_ID, x$year, x$species, FUN=last)
x$bb<-x$lo-12 
x<-x[!duplicated(x),]
x$fs.count<- ave(x$fs, x$PEP_ID, x$year, x$species, FUN=sum)
allspp<-x%>%dplyr::select(lat, long, PEP_ID, fs.count, year, species)
allspp<-allspp[!duplicated(allspp),]
allspp<-na.omit(allspp)
allspp$fs<-ifelse(allspp$fs.count>=1, 1, 0)

write.csv(allspp, file="~/Documents/git/regionalrisk/analyses/output/fs_allspp_five.csv", row.names = FALSE)
