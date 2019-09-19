### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
## Libraries
require(rstan)
require(brms)

options(stringsAsFactors = FALSE)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if(FALSE){
  bbdata <- read.csv("/n/wolkovich_lab/Lab/Cat/BBdata.csv")
  bbdata$cc <- ifelse(bbdata$year<=1983, 0, 1)
  bbdata$cc.z <- (bbdata$cc-mean(bbdata$cc,na.rm=TRUE))/(2*sd(bbdata$cc,na.rm=TRUE))
  
  #bb.mod.scaled <- brm(bb~cc.z, data=bbdata, control=list(max_treedepth = 15,adapt_delta = 0.99), 
   #                    iter=4000, warmup = 2500, chains=4, cores=4)
  
  #save(bb.mod.scaled, file="/n/wolkovich_lab/Lab/Cat/bbmod.scaled.Rdata")
  
  bb.mod <- brm(bb ~ cc + species + cc:species, data=bbdata, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                       iter=4000, warmup = 2500, chains=4, cores=4)
  
  save(bb.mod, file="/n/wolkovich_lab/Lab/Cat/bbmod.Rdata")
}

if(FALSE){
tmin <- read.csv("/n/wolkovich_lab/Lab/Cat/tminprep_boxplots.csv")
tmin$cc <- ifelse(tmin$year<=1983, 0, 1)
#tmin$cc.z <- (tmin$cc-mean(tmin$cc,na.rm=TRUE))/(2*sd(tmin$cc,na.rm=TRUE))

tmin.mod<- brm(Tmin ~ cc + species + cc:species, data=tmin, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                   iter=4000, warmup = 2500, chains=4, cores=4)

save(tmin.mod, file="/n/wolkovich_lab/Lab/Cat/tminmod.Rdata")

#bb.mod.species <- brm(bb~year*species, data=bb, control=list(max_treedepth = 15,adapt_delta = 0.99), 
#                    iter=4000, warmup = 2500, chains=4, cores=4)

#save(bb.mod.species, file="/n/wolkovich_lab/Lab/Cat/bbmod.species.Rdata")
}

if(FALSE){
lstfrz <- read.csv("/n/wolkovich_lab/Lab/Cat/lastfreezedates.csv")
lstfrz$cc <- ifelse(lstfrz$year<=1983, 0, 1)
#lstfrz$cc.z <- (lstfrz$cc-mean(lstfrz$cc,na.rm=TRUE))/(2*sd(lstfrz$cc,na.rm=TRUE))
  
lstfrz.mod <- brm(lastfreeze ~ cc + species + cc:species, data=lstfrz, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                             iter=4000, warmup = 2500, chains=4, cores=4)
  
save(lstfrz.mod, file="/n/wolkovich_lab/Lab/Cat/lstfrzmod.Rdata")
  
  #lstfrz.mod.species <- brm(lastfreeze~year*species, data=lstfrz, control=list(max_treedepth = 15,adapt_delta = 0.99), 
  #                    iter=4000, warmup = 2500, chains=4, cores=4)
  
  #save(lstfrz.mod.species, file="/n/wolkovich_lab/Lab/Cat/lstfrz.species.Rdata")
}


if(TRUE){
  fssimp <- read.csv("/n/wolkovich_lab/Lab/Cat/fs_newspace_orig.csv")
  fssimp$cc <- ifelse(fssimp$year<=1983, 0, 1)
  
  fssimp.mod <- brm(fs ~ cc + species + cc:species, data=fssimp, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                    iter=4000, warmup = 2500, chains=4, cores=4)
  
  save(fssimp.mod, file="/n/wolkovich_lab/Lab/Cat/fssimpmod.Rdata")
}
