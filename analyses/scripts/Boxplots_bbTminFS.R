## Started 5 September 2018 ##
## By Cat ##

## Making new boxplots for BB, Tmin, and FS across species ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggstance)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data
xx<-read.csv("output/fs_yearsitespp.csv", header=TRUE)
xx<-subset(xx, year>1950)
xx$lat.long<-paste(xx$lat, xx$long)
xx<-dplyr::select(xx, long, lat, year, fs.count, species)
xx<-xx[!duplicated(xx),]
xx$all<-paste(xx$lat, xx$long, xx$year, xx$species)

mat<-read.csv("output/BBdata.csv", header=TRUE)
mat<-subset(mat, year>1950)
mat<-dplyr::select(mat, species, year, lat, long, bb)
mat<-mat[!duplicated(mat),]
mat$all<-paste(mat$lat, mat$long, mat$year, mat$species)

d<-full_join(mat, xx, by="all")

#### 5 September 2018 - not lining up for some reason... need to fix ###

