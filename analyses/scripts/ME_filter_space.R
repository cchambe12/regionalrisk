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

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/")
bb<-read.csv("output/fs_matspsite.csv", header=TRUE)

bb$lat.long<-paste(bb$lat, bb$long)
bprep<-bb%>%dplyr::select(fs, lat.long)
bprep$y<-ave(bprep$fs, bprep$lat.long, FUN=sum)
bprep<-dplyr::select(bprep, lat.long, y)
bprep<-bprep[!duplicated(bprep),]
bprep<-separate(data = bprep, col = 1, into = c("lat", "long"), sep = "\\ ")
Y<- bprep$y
X <- bprep[, 1:2]

bcoord<-bprep%>%dplyr::select(lat,long)
bcoords<-as.data.frame(bcoord[!duplicated(bcoord),])
bcoords$lat<-as.numeric(bcoords$lat)
bcoords$long<-as.numeric(bcoords$long)

xymat<-as.matrix(bcoords)
nbgab <- graph2nb(gabrielneigh(xymat), sym = TRUE)
distgab <- nbdists(nbgab, xymat)
MEM_model <-"positive"
nb<-graph2nb(gabrielneigh(as.matrix(bcoords)), sym=TRUE)
listw<-nb2listw(nb, style ="B")
select <- ME(Y ~., data = as.data.frame(X), listw = listw, family = gaussian, nsim = 99,
             alpha = 0.01)
MEM.select <- select$vectors




