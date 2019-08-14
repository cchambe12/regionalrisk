### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
## Libraries
require(rstan)
require(brms)

options(stringsAsFactors = FALSE)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if(FALSE){
bb<-read.csv("/n/wolkovich_lab/Lab/Cat/FinalModels/fs_newspace_orig.csv", header=TRUE)
#bb<-read.csv("~/Documents/git/regionalrisk/analyses/output/fs_newspace_orig.csv", header=TRUE)
#check<-read.csv("~/Desktop/fs_newspace_orig.csv", header=TRUE)
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

bb<-bb[sample(nrow(bb), 1000), ]


orig.short<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                         cc.z + species + nao.z:species + 
                         mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
                         nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, 
             data=bb, chains=2,family=bernoulli(), cores=2, iter = 4000, warmup=2500,
            prior = prior(normal(0,1), class = "b"))

save(orig.short, file="/n/wolkovich_lab/Lab/Cat/orig_short.Rdata")


bb<-read.csv("/n/wolkovich_lab/Lab/Cat/FinalModels/fs_newspace_orig.csv", header=TRUE)
#bb<-read.csv("~/Documents/git/regionalrisk/analyses/output/fs_newspace_orig.csv", header=TRUE)
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

#bb$species <- as.character(ifelse(bb$species=="FAGSYL", "aaFAGSYL", bb$species))

orig.full<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                  cc.z + species + nao.z:species + 
                  mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
                 nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z,  
                data=bb, chains=4,family=bernoulli(), cores=4, iter = 4000, warmup=2500,
                prior = prior(normal(0,1), class = "b"))

save(orig.full, file="/n/wolkovich_lab/Lab/Cat/orig_full.Rdata")
}
if(FALSE){
orig.bigpriors.fagus<-brm(fs ~ nao.z + mat.z + dist.z + elev.z + space.z +
                 cc.z + species + nao.z:species + 
                 mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + 
                nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z, 
               data=bb, chains=2,family=bernoulli(), cores=2, iter = 4000, warmup=2500,
               prior = prior(normal(0,5), class = "b"))

save(orig.bigpriors.fagus, file="/n/wolkovich_lab/Lab/Cat/orig_bigpriors_fagus.Rdata")
}


if(TRUE){
#### Now adding some very simple models...
# 30 July 2019 - Cat

lstfrz <- read.csv("/n/wolkovich_lab/Lab/Cat/lastfreezedates.csv", header=TRUE)
lstfrz$cc <- ifelse(lstfrz$year<=1983, 0, 1)
lstfrz$cc.z <- (lstfrz$cc-mean(lstfrz$cc,na.rm=TRUE))/(2*sd(lstfrz$cc,na.rm=TRUE))

lstfrz.mod.scaled <- brm(lastfreeze~cc.z, data=lstfrz, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                     iter=4000, warmup = 2500, chains=4, cores=4)

save(lstfrz.mod.scaled, file="/n/wolkovich_lab/Lab/Cat/lstfrz.scaled.Rdata")

#lstfrz.mod.species <- brm(lastfreeze~year*species, data=lstfrz, control=list(max_treedepth = 15,adapt_delta = 0.99), 
 #                    iter=4000, warmup = 2500, chains=4, cores=4)

#save(lstfrz.mod.species, file="/n/wolkovich_lab/Lab/Cat/lstfrz.species.Rdata")
}

if(TRUE){
bbdata <- read.csv("/n/wolkovich_lab/Lab/Cat/BBdata.csv")
bbdata$cc <- ifelse(bbdata$year<=1983, 0, 1)
bbdata$cc.z <- (bbdata$cc-mean(bbdata$cc,na.rm=TRUE))/(2*sd(bbdata$cc,na.rm=TRUE))

bb.mod.scaled <- brm(bb~cc.z, data=bbdata, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                     iter=4000, warmup = 2500, chains=4, cores=4)

save(bb.mod.scaled, file="/n/wolkovich_lab/Lab/Cat/bbmod.scaled.Rdata")

bb.mod.simple <- brm(bb~cc, data=bbdata, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                     iter=4000, warmup = 2500, chains=4, cores=4)

save(bb.mod.simple, file="/n/wolkovich_lab/Lab/Cat/bbmod.simple.Rdata")


tmin <- read.csv("/n/wolkovich_lab/Lab/Cat/tminprep_boxplots.csv")
tmin$cc <- ifelse(tmin$year<=1983, 0, 1)
tmin$cc.z <- (tmin$cc-mean(tmin$cc,na.rm=TRUE))/(2*sd(tmin$cc,na.rm=TRUE))

tmin.simple <- brm(Tmin~cc, data=tmin, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                     iter=4000, warmup = 2500, chains=4, cores=4)

save(tmin.simple, file="/n/wolkovich_lab/Lab/Cat/tmin.simple.Rdata")

#bb.mod.species <- brm(bb~year*species, data=bb, control=list(max_treedepth = 15,adapt_delta = 0.99), 
 #                    iter=4000, warmup = 2500, chains=4, cores=4)

#save(bb.mod.species, file="/n/wolkovich_lab/Lab/Cat/bbmod.species.Rdata")
}

