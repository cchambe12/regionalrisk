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

#bb<-bb[sample(nrow(bb), 250000), ]

poisstrictpriorsquart<-brm(fs.count ~ nao.z + mat.z + dist.z + elev.z + space.z +
                       cc.z + species + nao.z:species + 
                       mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
                       nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, data=bb, chains=2,
                     family=poisson(), cores=2, iter = 4000, warmup=2000, prior=prior(normal(0,1), class="b"))
                 #prior = c(prior_string("normal(0,5)", class = "b"),
                           #prior(normal(-1, 5), class=b, coef=mat.z),
                           #prior(normal(-2, 5), class=b, coef=nao.z)))

save(poisstrictpriorsquart, file="/n/wolkovich_lab/Lab/Cat/elevdist_poisstrictpriorsquart.Rdata")

bernstrictpriorsquart<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                             cc.z + species + nao.z:species + 
                             mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
                             nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, data=bb, chains=2,
                           family=bernoulli(link="logit"), cores=2, iter = 4000, warmup=2000, prior = prior(normal(0,1), class="b"))
                           #prior = c(prior_string("normal(0,5)", class = "b"),
                            #         prior(normal(-1, 5), class=b, coef=mat.z),
                             #        prior(normal(-2, 5), class=b, coef=nao.z)))

save(bernstrictpriorsquart, file="/n/wolkovich_lab/Lab/Cat/elevdist_bernstrictpriorsquart.Rdata")

