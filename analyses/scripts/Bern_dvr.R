## Libraries
### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
require(rstan)
require(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

bb<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_newspace_dvr.csv", header=TRUE)
#bb<-read.csv("~/Documents/git/regionalrisk/fs_space_dvr.csv", header=TRUE)
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

bb<-bb[sample(nrow(bb), 80000), ]


dvr.short<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                        cc.z + species + nao.z:species + 
                       mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
                      nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, 
         data=bb, chains=2,family=bernoulli(), cores=2, iter = 4000, warmup=2500,
        prior = prior(normal(0,1), class = "b"))

save(dvr.short, file="/n/wolkovich_lab/Lab/Cat/dvr_short.Rdata")

bb<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_newspace_dvr.csv", header=TRUE)
#bb<-read.csv("~/Documents/git/regionalrisk/fs_space_dvr.csv", header=TRUE)
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


dvr.full<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                 cc.z + species + nao.z:species + 
                 mat.z:species + dist.z:species + elev.z:species + space.z:species +
                   cc.z:species + 
                 nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, 
               data=bb, chains=2,family=bernoulli(), cores=2, iter = 4000, warmup=2500,
               prior = prior(normal(0,1), class = "b"))

save(dvr.full, file="/n/wolkovich_lab/Lab/Cat/dvr_full.Rdata")

#dvr.bigpriors<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
 #               cc.z + species + nao.z:species + 
  #             mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
   #           nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, 
    #       data=bb, chains=2,family=bernoulli(), cores=2, iter = 4000, warmup=2500,
     #     prior = prior(normal(0,5), class = "b"))

#save(dvr.bigpriors, file="/n/wolkovich_lab/Lab/Cat/dvr_bigpriors.Rdata")


