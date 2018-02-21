#### 5 February 2018 - Cat
### Building eigenvector matrix for spatial autocorrelation to reduce collinearity issues
## Amazing advice from Nacho - trying FWD method now
### From Bauman et al 2017
# When the response data is univariate, if the purpose of the spatial predictors is to control the spatial
# autocorrelation of an OLS or GLM model, then the most suited MEM variable selection is that of Griffith
# and Peres-Neto (2006). This procedure selects the smallest MEM subset minimising spatial autocorrelation
# (Moran’s I) in the residuals.
# First method took ~24 hours and did not parse down significantly...

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(dplyr)
library(tidyr)
library(adespatial)
library(ade4)
library(spdep)
library(vegan)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/")
bb<-read.csv("output/fs_matspsite.csv", header=TRUE)

bb<-dplyr::select(bb, lat, long, lat.long, fs)
bb$y<-ave(bb$fs, bb$lat.long, FUN=sum)
bprep <- subset(bb, select=c("fs", "lat", "long")) 
bprep <- bprep[complete.cases(bprep),]
bprep<-bprep[!duplicated(bprep),]

#Y<-bprep[,1]
#X<-bprep[,2:3]
#X<-as.matrix(X)

#bb$lat.long<-paste(bb$lat, bb$long)
#bprep<-bb%>%dplyr::select(fs, lat.long)
#bprep$y<-ave(bprep$fs, bprep$lat.long, FUN=sum)
#bprep<-dplyr::select(bprep, lat.long, y)
#bprep<-bprep[!duplicated(bprep),]
#bprep<-separate(data = bprep, col = 1, into = c("lat", "long"), sep = "\\ ")
#Y<- bprep$y

#bcoord<-bprep%>%dplyr::select(lat,long)
#bcoords<-as.data.frame(bcoord[!duplicated(bcoord),])
#bcoords$lat<-as.numeric(bcoords$lat)
#bcoords$long<-as.numeric(bcoords$long)

#xymat<-as.matrix(bcoords,bcoords$lat:bcoords$long)
#X <- xymat[,1:2]
#nbgab <- graph2nb(gabrielneigh(xymat), sym = TRUE)
#distgab <- nbdists(nbgab, xymat)
#MEM_model <-"positive"
#nb<-graph2nb(gabrielneigh(as.matrix(bcoords)), sym=TRUE)
#listw<-nb2listw(nb, style ="B")
#X$lat<-as.numeric(X$lat)
#X$long<-as.numeric(X$long)
#select <- ME(Y ~ ., data=as.data.frame(X), listw = listw, family = gaussian, nsim = 1,
             #alpha = 0.005)
#MEM.select <- select$vectors



#### TESTING!!! ######
#data(mite)
#data(mite.xy)
#data(mite.env)

bprep<-bprep[1:70,]
#bprep$species<-as.numeric(as.factor(bprep$species))
Y<-bprep[,1]
#Y<-decostand(Y, method="hellinger")
X<-bprep[,2:3]
#X<-decostand(X, method="range")

C<-bprep[,2:3]
C<-as.matrix(C)
MEM_model <-"positive"
nb<-graph2nb(gabrielneigh(C), sym=TRUE)
listw<-nb2listw(nb, style ="B")

#Y<-mite
#Y<-Y[,2]

#data(mite.env)
X<-mite.xy[,1:2]

select<-ME(Y~., data=as.data.frame(X), listw=listw, family=gaussian, nsim=2, alpha=1)
MEM.select<-select$vectors


