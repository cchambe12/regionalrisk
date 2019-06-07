### Cat 28 Nov 2018
# Better control for space so it is not the strongest predictor

# Clear workspace
if(FALSE){
  rm(list=ls()) # remove everything currently held in the R memory
  options(stringsAsFactors=FALSE)
  graphics.off()
}

library(adespatial)
library(spdep)
library(vegan)

#setwd("~/Documents/git/regionalrisk/analyses/output")

#bb<-read.csv("fs_allspp_orig_allpred.csv", header=TRUE)
bb<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_allspp_orig_allpred.csv", header=TRUE)

#bb<-bb[sample(nrow(bb), 80000), ]

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

#bb$fs<-ifelse(bb$fs.count>0, 1, 0)
#Y<-bb$fs

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
#source("..//scripts/MEM.moransel.R")
#moransel<-MEM.moransel(Y, listw, MEM.autocor = MEM_model, nperm=999, alpha=0.05)
#moransel2<-MEM.moransel(Y, listw, MEM.autocor = MEM_model, nperm=999, alpha=0.001)

#dselect<-as.data.frame(moransel[["MEM.select"]])
#dselect2<-as.data.frame(moransel2[["MEM.select"]])


#write.csv(dselect, file="/n/wolkovich_lab/Lab/Cat/memselect_orig_pafull.csv", row.names=FALSE)
#dselect<-read.csv("~/Documents/git/regionalrisk/memselect_orig.csv", header=TRUE)

dx<-cbind(Y, MEM)

rex.mod<-lm(Y~ ., data=dx)
space<-rex.mod$fitted.values ## space<-rex.mod$fitted.values - vector which is the predicted with spatial autocorrelation
eigen<-space
eigen<-cbind(eigen, falsies)

library(dplyr)
prep_space<-full_join(bb, eigen)

write.csv(prep_space, file="/n/wolkovich_lab/Lab/Cat/fs_newspace_orig_pa.csv", row.names=FALSE)
#write.csv(prep_space, file="~/Documents/git/regionalrisk/analyses/output/fs_newspace_orig.csv", row.names=FALSE)

if(FALSE){
  goo<- read.csv("~/Desktop/fs_newspace_orig_pa.csv", header=TRUE)
  
  #goo<-prep_space
  
  goo<-subset(goo, select=c("species", "lat", "elev", "year", "mst", "cc", "fs.count", "nao",
                          "distkm", "eigen"))
  
  goo$fs<-ifelse(goo$fs.count>0, 1, 0)
  
  goo$nao.z <- (goo$nao-mean(goo$nao,na.rm=TRUE))/(2*sd(goo$nao,na.rm=TRUE))
  goo$mat.z <- (goo$mst-mean(goo$mst,na.rm=TRUE))/(2*sd(goo$mst,na.rm=TRUE))
  goo$cc.z <- (goo$cc-mean(goo$cc,na.rm=TRUE))/(2*sd(goo$cc,na.rm=TRUE))
  goo$elev.z <- (goo$elev-mean(goo$elev,na.rm=TRUE))/(2*sd(goo$elev,na.rm=TRUE))
  goo$lat.z <- (goo$lat-mean(goo$lat,na.rm=TRUE))/(2*sd(goo$lat,na.rm=TRUE))
  goo$dist.z <-(goo$distkm-mean(goo$distkm,na.rm=TRUE))/(2*sd(goo$distkm,na.rm=TRUE))
  goo$space.z <-(goo$eigen-mean(goo$eigen,na.rm=TRUE))/(2*sd(goo$eigen,na.rm=TRUE))
  #goo$goo.z <-(goo$goo-mean(goo$goo,na.rm=TRUE))/(2*sd(goo$goo,na.rm=TRUE))
  
  
  test<-glm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
              cc.z + species + nao.z:species + 
              mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
              nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, 
            data=goo, family=binomial)
  
  arm::display(test)
}







