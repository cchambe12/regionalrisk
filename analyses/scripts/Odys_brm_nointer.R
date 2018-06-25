### Let's run some real models in Odyssey! W00!
## Cat - 6 June 2018
# Looking to do main model without interactions to start - just using brms for efficiency and accuracy

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)

#### get the data
bb.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/bb.brm.nointer.csv", header=TRUE)

brm.full.nointer<-brm(fs.count~nao+sp.temp+cc+space+sm.elev+
                        (nao+sp.temp+cc+space+sm.elev|species), data=bb.stan, chains=2, family=poisson, cores=48)

save(brm.full.nointer, file="/n/wolkovich_lab/Lab/Cat/brm.Rdata")

