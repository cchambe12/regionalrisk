### Let's run some real models in Odyssey! W00!
## Cat - 6 June 2018
# Looking to do main model without interactions to start - just using brms for efficiency and accuracy

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)

#### get the data
bb<-read.csv("/n/wolkovich_lab/Lab/Cat/bb_latprep.csv", header=TRUE)

bb$nao.z <- (bb$m.index-mean(bb$m.index,na.rm=TRUE))/(2*sd(bb$m.index,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$space-mean(bb$space,na.rm=TRUE))/(2*sd(bb$space,na.rm=TRUE))

bprior1 <- prior(normal(0,1), class="b") + prior(student_t(1,0,2), group="species", class="sd")

cent.fast<-brm(fs.count~nao.z+mat.z+cc.z+elev.z+dist.z+nao.z:cc.z + mat.z:cc.z + elev.z:cc.z +
                 dist.z:cc.z + (0+nao.z||species) + (0+mat.z||species) + (0+cc.z||species) +
               (0+elev.z||species) + (0+dist.z||species), 
               data=bb.stan, prior=bprior1, chains=2)

save(cent.fast, file="/n/wolkovich_lab/Lab/Cat/cen_realfast.Rdata")

