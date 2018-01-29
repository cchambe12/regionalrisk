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
bcoords<-bb%>%dplyr::select(lat, long)
bcoords<-bcoords[!duplicated(bcoords),]

### Will this work...? ###
xymat<-as.matrix(bcoords)
nbgab <- graph2nb(gabrielneigh(xymat), sym = TRUE)
distgab <- nbdists(nbgab, xymat)
fdist <- lapply(distgab, function(x) 1-x/max(dist(xymat)))
listwgab <- nb2listw(nbgab, glist = fdist, style = "B")
print(listw2mat(listwgab),digits=3)
mem.gab<-mem(listwgab)

## Based on Bauman et al... ##
xymat<-as.matrix(bcoords)
nbgab <- graph2nb(gabrielneigh(xymat), sym = TRUE)
distgab <- nbdists(nbgab, xymat)
MEM_model <-"positive"
nb<-graph2nb(gabrielneigh(as.matrix(bcoords), nmult=5), sym=TRUE)
listw<-nb2listw(nb, style ="B")
MEM<-scores.listw(listw, MEM.autocor = MEM_model)

## MIR approach - fewer predictors, second most accurate
source("scripts/MEM.moransel.R")
moransel<-MEM.moransel(bb, bcoords, listw, MEM.autocor=MEM_model, nperm=999, alpha=0.05)