#### 29 January 2018 - Cat
### Building eigenvector matrix for spatial autocorrelation to reduce collinearity issues
## Amazing advice from Nacho

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(dplyr)
library(tidyr)
library(adespatial)
library(ade4)
library(spdep)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/")

## Get Data
bb<-read.csv("output/fs_matspsite.csv", header=TRUE)
bb$lat.long<-paste(bb$lat, bb$long)
bprep<-bb%>%dplyr::select(fs, lat.long)
bprep$y<-ave(bprep$fs, bprep$lat.long, FUN=sum)
bprep<-dplyr::select(bprep, lat.long, y)
bprep<-bprep[!duplicated(bprep),]
bcoord<-bprep%>%dplyr::select(lat.long)
bcoords<-as.data.frame(bcoord[!duplicated(bcoord),])
bcoords<-separate(data = bcoords, col = 1, into = c("lat", "long"), sep = "\\ ")
bcoords$lat<-as.numeric(bcoords$lat)
bcoords$long<-as.numeric(bcoords$long)


## Based on Bauman et al... ##
xymat<-as.matrix(bcoords)
nbgab <- graph2nb(gabrielneigh(xymat), sym = TRUE)
distgab <- nbdists(nbgab, xymat)
MEM_model <-"positive"
nb<-graph2nb(gabrielneigh(as.matrix(bcoords)), sym=TRUE)
listw<-nb2listw(nb, style ="B")
#MEM<-scores.listw(listw, MEM.autocor = MEM_model)


y<-as.vector(bprep$y)
source("scripts/MEM.moransel.R")
moransel<-MEM.moransel(y, listw, MEM.autocor=MEM_model, nperm=999, alpha=0.001)

d<-as.data.frame(moransel[["MEM.select"]])
d<-d%>%rename(site=`moransel[["MEM.select"]][["MEM151"]]`)
d$row<-1:11684
bcoord$row<-1:11684
df<-inner_join(d, bcoord)
df<-dplyr::select(-row)
bx<-full_join(bb, df)

#write.csv(bx, file="~/Documents/git/regionalrisk/analyses/output/mat_site.csv", row.names = FALSE)
