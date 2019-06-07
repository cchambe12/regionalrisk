### 13 Feb 2019 - make some APC plots and PPC plots!
## Libraries
options(stringsAsFactors = FALSE)

library(brms)
library(tidybayes)
library(dplyr)
library(rstan)
library(rstanarm)
library(bayesplot)

load("/n/wolkovich_lab/Lab/Cat/orig_full_fagus.Rdata")

bb <- read.csv("/n/wolkovich_lab/Lab/Cat/fs_newspace_orig.csv", header=TRUE)

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

if(FALSE){
bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$eigen-mean(bb$eigen,na.rm=TRUE))/(2*sd(bb$eigen,na.rm=TRUE))

bb$species <- as.character(ifelse(bb$species=="FAGSYL", "aaFAGSYL", bb$species))

bb<-bb[sample(nrow(bb), 350000), ]


me.elev <- bb %>%  add_predicted_draws(orig.full.fagus, method = "predict")

#me.elev<-me.elev[sample(nrow(me.elev), 755088), ]


write.csv(me.elev, file="/n/wolkovich_lab/Lab/Cat/me.elev.csv", row.names=FALSE)


orig.ppc <- pp_check(orig.full.fagus)

png("/n/wolkovich_lab/Lab/Cat/orig_ppc.png", 
    width=8,
    height=5, units="in", res = 350 )
orig.ppc
dev.off()
}



#d<-d[sample(nrow(d), 755088), ]
y <- as.vector(bb$fs)
yrep <- posterior_predict(orig.full.fagus, draws = 755088)
ppc.sd <- ppc_stat(y, yrep, stat = "sd")

png("/n/wolkovich_lab/Lab/Cat/ppc_sd.png", 
    width=8,
    height=5, units="in", res = 350 )
ppc.sd
dev.off()

ppc.min <- ppc_stat(y, yrep, stat = "min")

png("/n/wolkovich_lab/Lab/Cat/ppc_min.png", 
    width=8,
    height=5, units="in", res = 350 )
ppc.min
dev.off()

if(FALSE){
ppc.max <- ppc_stat(y, yrep, stat = "max")

png("/n/wolkovich_lab/Lab/Cat/ppc_max.png", 
    width=8,
    height=5, units="in", res = 350 )
ppc.max
dev.off()
}

load("/n/wolkovich_lab/Lab/Cat/orig_bigpriors_fagus.Rdata")

bigpriors.ppc <- pp_check(orig.bigpriors.fagus)

png("/n/wolkovich_lab/Lab/Cat/orig_ppc_bigpriors.png", 
    width=8,
    height=5, units="in", res = 350 )
bigpriors.ppc
dev.off()

#y <- as.vector(bb$fs)
yrep <- posterior_predict(orig.bigpriors.fagus, draws = 755088)
bigpriors.sd <- ppc_stat(y, yrep, stat = "sd")

png("/n/wolkovich_lab/Lab/Cat/big_sd.png", 
    width=8,
    height=5, units="in", res = 350 )
bigpriors.sd
dev.off()

bigpriors.min <- ppc_stat(y, yrep, stat = "min")

png("/n/wolkovich_lab/Lab/Cat/big_min.png", 
    width=8,
    height=5, units="in", res = 350 )
bigpriors.min
dev.off()

bigpriors.max <- ppc_stat(y, yrep, stat = "max")

png("/n/wolkovich_lab/Lab/Cat/big_max.png", 
    width=8,
    height=5, units="in", res = 350 )
bigpriors.max
dev.off()

