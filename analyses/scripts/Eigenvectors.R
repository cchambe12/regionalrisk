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
#setwd("~/Documents/git/regionalrisk/analyses/")

## Get Data
#bb<-read.csv("output/fs_yearsitespp.csv", header=TRUE)
#bb$lat.long<-paste(bb$lat, bb$long)
#bb$fs.count.num<-ave(bb$fs.count, bb$lat.long, FUN=sum)
#bb$fs.num<-ave(bb$fs, bb$species, bb$lat.long, FUN=sum)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/")
#bb<-read.csv("output/fs_matspsite.csv", header=TRUE)
xx<-read.csv("output/fs_yearsitespp.csv", header=TRUE)
xx<-subset(xx, year>1950)
bb<-read.csv("output/mat_fulldata.csv", header=TRUE)
bb<-inner_join(bb, xx, by=c("lat", "long", "year"))
bb<-subset(bb, select=c("long", "lat", "year", "fs.count", "lat.long"))
bb<-bb[!duplicated(bb),]

bprep<-bb
bprep$y<-bprep$fs.count
#bprep<-dplyr::select(bprep, lat.long, y)
#bprep<-bprep[!duplicated(bprep),]
#bcoord<-bprep%>%dplyr::select(lat.long)
#bcoords<-as.data.frame(bcoord[!duplicated(bcoord),])
#bcoords<-separate(data = bcoords, col = 1, into = c("lat", "long"), sep = "\\ ")
bcoords<-bprep%>%dplyr::select(lat, long)
bcoords<-bcoords[!duplicated(bcoords),]
bcoords$lat<-as.numeric(bcoords$lat)
bcoords$long<-as.numeric(bcoords$long)

## Get Data
#bb<-read.csv("output/mat_fulldata.csv", header=TRUE)

#bprep<-bb%>%dplyr::select(pre.bb, lat.long)
#bprep$y<-bprep$pre.bb
#bprep<-dplyr::select(bprep, lat.long, y)
#bprep<-bprep[!duplicated(bprep),]
#bcoord<-bprep%>%dplyr::select(lat.long)
#bcoords<-as.data.frame(bcoord[!duplicated(bcoord),])
#bcoords<-separate(data = bcoords, col = 1, into = c("lat", "long"), sep = "\\ ")
#bcoords$lat<-as.numeric(bcoords$lat)
#bcoords$long<-as.numeric(bcoords$long)


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

dselect<-as.data.frame(moransel[["MEM.select"]])
dx<-cbind(bprep, dselect)
rex<-dx%>%dplyr::select(-lat.long)
rex.mod<-lm(y~ ., data=rex)
space<-residuals(rex.mod)

b_space<-cbind(bprep, space)
prep_space<-full_join(bb, b_space, by="lat.long")
write.csv(prep_space, file="~/Documents/git/regionalrisk/analyses/output/fs_matspspace.csv", row.names=FALSE)

##### Stuff to remove later is below... #####

#write.csv(bx, file="~/Documents/git/regionalrisk/analyses/output/mat_site.csv", row.names = FALSE)

library(gstat)
library(fields)

sub_data<-bb%>%dplyr::select(fs, lat, long, lat.long)
sub_data$fs<-ave(sub_data$fs, sub_data$lat.long, FUN=sum)
sub_data<-dplyr::select(sub_data, -lat.long)
sub_data<-sub_data[!duplicated(sub_data),]

fitTps <- Tps(sub_data[c("lat", "long")], sub_data[,"fs"])
our.p <- predictSurface(fitTps)
spatially_corrected_y <- fitTps$residuals
surface(our.p,type="C",xlab="lat",ylab="long")

fitTps <- Tps(sub_data[c("Row", "Col")], sub_data[,trait_columns[i]])
our.p <- predictSurface(fitTps)
spatially_corrected_y <- fitTps$residuals
surface(our.p,type="C",xlab="Row",ylab="Col")



