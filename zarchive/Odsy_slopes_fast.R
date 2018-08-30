### Let's run some real models in Odyssey! W00!
## Cat - 6 June 2018
# Looking to do main model without interactions to start - just using brms for efficiency and accuracy

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#### get the data
bb.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/bb.brm2.csv", header=TRUE)
bb.stan<-subset(bb.stan, select=c("fs.count", "m.index", "sp.temp", "cc", "sm.elev","space", "species"))
bb.stan<-bb.stan[!duplicated(bb.stan),]
bb.stan<-na.omit(bb.stan)
bb.stan<-bb.stan[sample(nrow(bb.stan), 100000, replace=FALSE),]

slopes.fast<-brm(fs.count~m.index+sp.temp+cc+sm.elev+space+m.index:cc + sp.temp:cc +sm.elev:cc +
                        space:cc +
                        (0+m.index||species)+(0+sp.temp||species)+(0+cc||species)+
                   (0+sm.elev||species) + (0+space||species), data=bb.stan, chains=4,cores=4)

save(slopes.fast, file="/n/wolkovich_lab/Lab/Cat/brm_slopes_fast.Rdata")

