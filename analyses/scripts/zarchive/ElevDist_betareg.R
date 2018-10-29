## Libraries
### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
require(rstan)
require(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

bb<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_space_new.csv", header=TRUE)
#bb<-read.csv("~/Documents/git/regionalrisk/analyses/output/fs_space_new.csv", header=TRUE)
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

bb<-bb[sample(nrow(bb), 8000), ]

binom<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
            cc.z + species + nao.z:species + 
            mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
            nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, chains=2, cores=2, 
          family=binomial(), iter = 4500, warmup=2000)



save(binom, file="/n/wolkovich_lab/Lab/Cat/elevdist_binom.Rdata")

## 10000 iterations, warmup at 5000
pois<-brm(fs.count ~ nao.z + mat.z + dist.z + space.z + elev.z +
            cc.z + species + nao.z:species + 
            mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
            nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb,
          family = poisson(), chains=2, cores=2 , iter = 4500, warmup=2500)


save(pois, file="/n/wolkovich_lab/Lab/Cat/elevdist_pois.Rdata")

negbinom<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
           cc.z + species + nao.z:species + 
           mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
           nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, chains=2,
         family=negbinomial(), cores=2, iter = 4500, warmup=2500)


save(negbinom, file="/n/wolkovich_lab/Lab/Cat/elevdist_negbinom.Rdata")

zeroneg<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
                cc.z + species + nao.z:species + 
                mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
                nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, chains=2,
              family=zero_inflated_negbinomial(), cores=2, iter = 4500, warmup=2500)


save(zeroneg, file="/n/wolkovich_lab/Lab/Cat/elevdist_zeroneg.Rdata")

