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

brm.full.nointer<-brm(fs.count~m.index+sp.temp+cc+space+sm.elev+
                        (m.index+sp.temp+cc+space+sm.elev|species), data=bb.stan, chains=2, 
                      warmup=2500,iter=4000,
                      control = list(max_treedepth = 12,adapt_delta = 0.99), cores=64)

save(brm.full.nointer, file="/n/wolkovich_lab/Lab/Cat/brm.Rdata")

