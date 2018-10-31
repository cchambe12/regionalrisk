## Libraries
### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
require(rstan)
require(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

bb<-read.csv("/n/wolkovich_lab/Lab/Cat/regrisk.nov.csv", header=TRUE)
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

bb<-bb[sample(nrow(bb), 800), ]

pois.test<-brm(fs.count ~ nao.z + mat.z + dist.z + space.z + elev.z +
           cc.z + species + 
          mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
         mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb, chains=2, cores=2, 
      family=poisson(), iter = 4500, warmup=2000, thin=1.5, 
      prior = prior(cauchy(0,13)))



save(pois.test, file="/n/wolkovich_lab/Lab/Cat/elevdist_poistest.Rdata")

## 10000 iterations, warmup at 5000
#pois<-brm(fs.count ~ nao.z + mat.z + dist.z + space.z + elev.z +
#           cc.z + species + nao.z:species + 
#          mat.z:species + dist.z:species + space.z:species + elev.z:species + cc.z:species + 
#         nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + space.z:cc.z + elev.z:cc.z, data=bb,
#      family = poisson(), chains=2, cores=2 , iter = 4500, warmup=2500)


#save(pois, file="/n/wolkovich_lab/Lab/Cat/elevdist_pois.Rdata")

berntest<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                   cc.z + species + nao.z:species + 
                   mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
                   nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, data=bb, chains=2,
                 family=bernoulli(), cores=2, iter = 4500, warmup=2000, thin=5, 
              priors = c(prior(bernoulli(0,1), class = "Intercept"),
                         prior(normal(0,1), class = "b"),
                         prior(normal(0,1), class = "sd")))


save(berntest, file="/n/wolkovich_lab/Lab/Cat/elevdist_berntest.Rdata")

cc<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
           species + nao.z:species + 
           mat.z:species + dist.z:species + elev.z:species + space.z:species, data=bb, chains=2,
         family=bernoulli(), cores=2, iter = 4500, warmup=2500)


save(cc, file="/n/wolkovich_lab/Lab/Cat/elevdist_cc.Rdata")

species<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                cc.z + 
                nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, data=bb, chains=2,
              family=bernoulli(), cores=2, iter = 4500, warmup=2000, thin=5, 
              priors = c(prior(bernoulli(0,1), class = "Intercept"),
                         prior(normal(0,1), class = "b"),
                         prior(normal(0,1), class = "sd")))

save(species, file="/n/wolkovich_lab/Lab/Cat/elevdist_species.Rdata")

