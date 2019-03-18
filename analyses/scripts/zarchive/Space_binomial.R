### Cat 1 Oct 2018
# Taking another stab at the space parameter


rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(adespatial)
library(vegan)
library(spdep)

setwd("~/Documents/git/regionalrisk/analyses/")
bb<-read.csv("output/regrisk.nov.csv", header=TRUE)

MEM_model<-"positive"
style<-"B"

C<-subset(bb, select=c(long, lat))
C$xy<-paste(C$long, C$lat)
C<-C[!duplicated(C$xy),]
C<-subset(C, select=c(long, lat))

nb<-graph2nb(gabrielneigh(as.matrix(C), nnmult=5), sym=TRUE)
listw<- nb2listw(nb, style=style)
MEM<-scores.listw(listw, MEM.autocor = MEM_model)

Y<-ave(bb$fs.count, bb$lat.long, FUN=sum)
Y<-ifelse(Y>0, 1, 0)

X<-subset(bb, select=c(elev, distkm, mst))

select<-ME(Y~., data=as.data.frame(X), listw = listw, family="binomial", nsim=99, alpha=0.05)
MEM.select<-select$vectors


