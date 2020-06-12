### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
## Libraries
require(rstan)
require(brms)

options(stringsAsFactors = FALSE)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if(TRUE){
  bbdata <- read.csv("/n/wolkovich_lab/Lab/Cat/BBdata.csv")
  #bbdata <- read.csv("~/Documents/git/regionalrisk/analyses/output/BBdata.csv")
  bbdata$cc <- ifelse(bbdata$year<=1983, 0, 1)
  bbdata$lat.long <- paste(bbdata$lat, bbdata$long)
  
  bbdata.sub <- subset(bbdata, select=c("bb", "cc", "species", "PEP_ID"))
  bbdata.sub <- bbdata.sub[!duplicated(bbdata.sub),]
  
  bbsub.mod <- brm(bb ~ cc + species + cc:species, data=bbdata.sub, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                       iter=4000, warmup = 2500, chains=4, cores=4)
  
  save(bbsub.mod, file="/n/wolkovich_lab/Lab/Cat/bbmodsub.Rdata")
  #save(bbsub.mod, file="~/Documents/git/regionalrisk/bbmodsub.Rdata")
}

if(FALSE){
tmin <- read.csv("/n/wolkovich_lab/Lab/Cat/tminprep_boxplots_long.csv")
#tmin <- read.csv("~/Documents/git/regionalrisk/analyses/output/tminprep_boxplots_long.csv")
tmin$cc <- ifelse(tmin$year<=1983, 0, 1)
tmin$lat.long <- paste(tmin$lat, tmin$long)

tmin.sub <- subset(tmin, select=c("cc", "Tmin", "species"))
tmin.sub <- tmin.sub[!duplicated(tmin.sub),]

tminlongsub.mod<- brm(Tmin ~ cc + species + cc:species, data=tmin, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                   iter=4000, warmup = 2500, chains=4, cores=4)

save(tminlongsub.mod, file="/n/wolkovich_lab/Lab/Cat/tminmod_longsub.Rdata")

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
  fssimp <- read.csv("/n/wolkovich_lab/Lab/Cat/fs_newspace_long.csv")
  #fssimp <- read.csv("~/Documents/git/regionalrisk/analyses/output/fs_newspace_long.csv")
  
  fssimp$cc <- ifelse(fssimp$year<=1983, 0, 1)
  fssimp.sub <- subset(fssimp, select=c("fs", "cc", "lat.long", "species")) ## added lat.long to list
  fssimp.sub <- fssimp.sub[!duplicated(fssimp.sub),]
  
  fssimp.sub$fstot <- ave(fssimp.sub$fs, fssimp.sub$lat.long, fssimp.sub$species, fssimp.sub$cc, FUN=sum)
  
  fssimp.sub <- subset(fssimp.sub, select=c("fstot", "cc", "lat.long", "species")) ## added lat.long to list
  fssimp.sub <- fssimp.sub[!duplicated(fssimp.sub),]
  
  fstotlongtest.mod <- brm(fstot ~ cc + species + cc:species, data=fssimp.sub, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                    iter=4000, warmup = 2500, chains=4, cores=4)
  
  save(fstotlongtest.mod, file="/n/wolkovich_lab/Lab/Cat/fstotmodlongtest.Rdata")
  #save(fstotlong.mod, file="~/Documents/git/regionalrisk/fstotmodlong.Rdata")
  
}
