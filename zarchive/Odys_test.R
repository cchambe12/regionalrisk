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
bb.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/bb.brm.nointer.csv", header=TRUE)
bb.stan<-subset(bb.stan, select=c("fs.count", "m.index", "sp.temp", "cc", "sm.elev", "space"))
bb.stan<-bb.stan[!duplicated(bb.stan),]
bb.stan<-na.omit(bb.stan)

brm.simple<-brm(fs.count~m.index+sp.temp+cc+sm.elev+space+m.index:cc+sp.temp:cc+
                  sm.elev:cc+space:cc, data=bb.stan, chains=2, cores=4)

save(brm.simple, file="/n/wolkovich_lab/Lab/Cat/brmsimple.Rdata")

