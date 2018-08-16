### Let's run some real models in Odyssey! W00!
## Cat - 6 June 2018
# Looking to do main model without interactions to start - just using brms for efficiency and accuracy

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#### get the data
bb.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/bb.brm.nointer.csv", header=TRUE)
bb.stan<-subset(bb.stan, select=c("fs.count", "m.index", "sp.temp", "cc", "sm.elev","space", "species"))
bb.stan<-bb.stan[!duplicated(bb.stan),]
bb.stan<-na.omit(bb.stan)
bb.stan$nao.z <- (bb.stan$m.index-mean(bb.stan$m.index,na.rm=TRUE))/sd(bb.stan$m.index,na.rm=TRUE)
bb.stan$mat.z <- (bb.stan$sp.temp-mean(bb.stan$sp.temp,na.rm=TRUE))/sd(bb.stan$sp.temp,na.rm=TRUE)
bb.stan$cc.z <- (bb.stan$cc-mean(bb.stan$cc,na.rm=TRUE))/sd(bb.stan$cc,na.rm=TRUE)
bb.stan$elev.z <- (bb.stan$sm.elev-mean(bb.stan$sm.elev,na.rm=TRUE))/sd(bb.stan$sm.elev,na.rm=TRUE)
bb.stan$space.z <- (bb.stan$space-mean(bb.stan$space,na.rm=TRUE))/sd(bb.stan$space,na.rm=TRUE)

cent.fast<-brm(fs.count~nao.z+mat.z+cc.z+elev.z+space.z+nao.z:cc.z + mat.z:cc.z + elev.z:cc.z +
                        space.z:cc.z +
                        (0+nao.z||species) + (0+mat.z||species) + (0+cc.z||species), data=bb.stan, chains=4,cores=4)

save(cent.fast, file="/n/wolkovich_lab/Lab/Cat/cen_fast.Rdata")

