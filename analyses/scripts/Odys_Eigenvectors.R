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
library(rstanarm)

# Set Working Directory
xx<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_yearsitespp.csv", header=TRUE)
xx<-subset(xx, year>1950)
bb<-read.csv("/n/wolkovich_lab/Lab/Cat/mat_fulldata.csv", header=TRUE)
bb<-dplyr::inner_join(bb, xx, by=c("lat", "long", "year"))
bb<-subset(bb, select=c("long", "lat", "year", "fs.count", "lat.long"))
bb<-bb[!duplicated(bb),]
bb$fs.num<-ave(bb$fs.count, bb$lat.long)

bprep<-bb%>%dplyr::select(fs.num, lat.long)
bprep<-bprep[!duplicated(bprep),]
bprep$y<-bprep$fs.num
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


y<-as.vector(bprep$y)
source("/n/wolkovich_lab/Lab/Cat/MEM.moransel.R")
moransel<-MEM.moransel(y, listw, MEM.autocor=MEM_model, nperm=999, alpha=0.001)

dselect<-as.data.frame(moransel[["MEM.select"]])
dx<-cbind(bprep, dselect)
rex<-dx%>%dplyr::select(-lat.long)
rex.mod<-lm(y~ ., data=rex)
space<-residuals(rex.mod)

b_space<-cbind(bprep, space)
prep_space<-full_join(bb, b_space, by="lat.long")
write.csv(prep_space, file="/n/wolkovich_lab/Lab/Cat/fs_matspspace_new.csv", row.names=FALSE)





