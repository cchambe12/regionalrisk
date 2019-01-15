### Cat 28 Nov 2018
# Better control for space so it is not the strongest predictor

library(adespatial)
library(spdep)
library(vegan)

#setwd("~/Documents/git/regionalrisk/analyses/output")

#goo<-read.csv("fs_allspp_five_allpred.csv", header=TRUE)
bb<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_allspp_five_allpred.csv", header=TRUE)

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))

fit<-lm(fs ~ nao.z + mat.z + dist.z + elev.z +
          cc.z + species + nao.z:species + 
          mat.z:species + dist.z:species + elev.z:species + cc.z:species + 
          nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z, data=bb)

Y<-residuals(fit)

MEM_model<-"positive"
style<-"B"

C<-subset(bb, select=c(long, lat))
falsies<-cbind(C, Y)
falsies$xy<-paste(C$long, C$lat)
falsies$Y<-ave(falsies$Y, falsies$xy)
falsies<-falsies[!duplicated(falsies),]

C<-subset(falsies, select=c(long, lat))
Y<-falsies$Y
findY<-subset(falsies, select=c("xy", "Y"))
Y <- falsies$Y
library(tidyr)
Y <- spread(data = findY, 
            key = xy,
            value = Y)

Y<-as.numeric(decostand(Y, method="pa")) # check to see if this changes anything

nb <- graph2nb(gabrielneigh(as.matrix(C), nnmult=5), sym=TRUE)
listw <- nb2listw(nb, style=style, zero.policy = TRUE)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)

#source("/n/wolkovich_lab/Lab/Cat/MEM.moransel.R")
#moransel<-MEM.moransel(Y, listw, MEM.autocor = MEM_model, nperm=999, alpha=0.05)

#dselect<-as.data.frame(moransel[["MEM.select"]])

#write.csv(dselect, file="/n/wolkovich_lab/Lab/Cat/memselect_five.csv", row.names=FALSE)
#dselect<-read.csv("~/Documents/git/regionalrisk/memselect_five.csv", header=TRUE)

dx<-cbind(Y, MEM)

rex.mod<-lm(Y~ ., data=dx)
space<-rex.mod$fitted.values ## space<-rex.mod$fitted.values - vector which is the predicted with spatial autocorrelation
eigen<-space
eigen<-cbind(eigen, falsies)

library(dplyr)
prep_space<-full_join(bb, eigen)

write.csv(prep_space, file="/n/wolkovich_lab/Lab/Cat/fs_newspace_five.csv", row.names=FALSE)
#write.csv(prep_space, file="~/Documents/git/regionalrisk/analyses/output/fs_newspace_orig.csv", row.names=FALSE)
