## Libraries
### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
library(brms)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

bb<-read.csv("/n/wolkovich_lab/Lab/Cat/bb_latprep_nov.csv", header=TRUE)

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$space-mean(bb$space,na.rm=TRUE))/(2*sd(bb$space,na.rm=TRUE))

bb<-bb[sample(nrow(bb), 1000), ]


binomial<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
                cc.z + species + nao.z:species + 
                mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
                nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, cores=4,
            family = binomial(), chains=2)


save(binomial, file="/n/wolkovich_lab/Lab/Cat/elevdist_binomial.Rdata")

beta<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
              cc.z + species + nao.z:species + 
              mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
              nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, data=bb, chains=2, cores=4,
            family=Beta())


save(beta, file="/n/wolkovich_lab/Lab/Cat/elevdist_beta.Rdata")

negbinom<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
                cc.z + species + nao.z:species + 
                mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
                nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, chains=2, cores=4,
              family=zero_inflated_negbinomial())


save(negbinom, file="/n/wolkovich_lab/Lab/Cat/elevdist_negbinom.Rdata")

base<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
             cc.z + species + nao.z:species + 
             mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
             nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, chains=2, cores=4,
           family=gaussian)


save(base, file="/n/wolkovich_lab/Lab/Cat/elevdist_base2chains.Rdata")

hurdle<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
            cc.z + species + nao.z:species + 
            mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
            nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, chains=2, cores=4,
          family=hurdle_negbinomial())


save(hurdle, file="/n/wolkovich_lab/Lab/Cat/elevdist_hurdle.Rdata")

hurdpois<-brm(fs ~ nao.z + mat.z + dist.z + space.z + elev.z +
              cc.z + species + nao.z:species + 
              mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
              nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, chains=2, cores=4,
            family=hurdle_poisson())


save(hurdpois, file="/n/wolkovich_lab/Lab/Cat/elevdist_hurdlepois.Rdata")


