## Cat - 28 Sept 2018

rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(rstan)
library(rstanarm)

bb<-read.csv("/n/wolkovich_lab/Lab/Cat/bb_latprep_nov.csv", header=TRUE)

bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$space-mean(bb$space,na.rm=TRUE))/(2*sd(bb$space,na.rm=TRUE))


mod<-stan_glm(fs.count~ nao.z + mat.z + elev.z + dist.z +
                cc.z + species + nao.z:species + 
                mat.z:species + elev.z:species + dist.z:species + cc.z:species + 
                nao.z:cc.z + mat.z:cc.z + elev.z:cc.z + dist.z:cc.z, data=bb)


save(mod, file="/n/wolkovich_lab/Lab/Cat/elevdist.Rdata")




