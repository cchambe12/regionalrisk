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

#bb<-bb[sample(nrow(bb), 10000), ]

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
#foo<-cbind(C, Y)
#foo$xy<-paste(C$long, C$lat)
#foo$Y<-ave(foo$Y, foo$xy)
#foo<-foo[!duplicated(foo),]

#C<-subset(foo, select=c(long, lat))
#Y<-foo$Y

Y <- decostand(Y, method = "pa")
C <- as.matrix(C)

nb <- graph2nb(gabrielneigh(as.matrix(C), nnmult=5), sym=TRUE)
listw <- nb2listw(nb, style=style, zero.policy = TRUE)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)

source("/n/wolkovich_lab/Lab/Cat/MEM.moransel.R")
#source("..//scripts/MEM.moransel.R")
moransel<-MEM.moransel(Y, listw, MEM.autocor = MEM_model, nperm=999, alpha=0.05, zero.policy=TRUE)

dselect<-as.data.frame(moransel[["MEM.select"]])

write.csv(dselect, file="/n/wolkovich_lab/Lab/Cat/memselect_orig_pafull.csv", row.names=FALSE)
#dselect<-read.csv("~/Documents/git/regionalrisk/memselect_orig.csv", header=TRUE)

dx<-cbind(Y, dselect)

rex.mod<-lm(Y~ ., data=dx)
space<-rex.mod$fitted.values ## space<-rex.mod$fitted.values - vector which is the predicted with spatial autocorrelation
eigen<-space
eigen<-cbind(eigen, foo)

library(dplyr)
prep_space<-full_join(bb, eigen)

write.csv(prep_space, file="/n/wolkovich_lab/Lab/Cat/fs_newspace_orig_pafull.csv", row.names=FALSE)
#write.csv(prep_space, file="~/Documents/git/regionalrisk/analyses/output/fs_newspace_orig.csv", row.names=FALSE)

if(FALSE){
bb<- read.csv("~/Desktop/fs_newspace_orig.csv", header=TRUE)

bb<-prep_space

bb<-subset(bb, select=c("species", "lat", "elev", "year", "mst", "cc", "fs.count", "nao",
                        "distkm", "eigen"))

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$eigen-mean(bb$eigen,na.rm=TRUE))/(2*sd(bb$eigen,na.rm=TRUE))
#bb$bb.z <-(bb$bb-mean(bb$bb,na.rm=TRUE))/(2*sd(bb$bb,na.rm=TRUE))


test<-glm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                  cc.z + species + nao.z:species + 
                  mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
                  nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, 
                data=bb, family=binomial)

arm::display(test)
}



