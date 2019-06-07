### Let's run some real models in Odyssey! W00!
## Cat - 6 June 2018
# Looking to do main model without interactions to start - just using brms for efficiency and accuracy

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)
library(future)
plan(multiprocess)

#### get the data
bb.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/bb.brm.nointer.csv", header=TRUE)
bb.stan<-subset(bb.stan, select=c("fs.count", "m.index", "sp.temp", "cc", "sm.elev","space", "species"))
bb.stan<-bb.stan[!duplicated(bb.stan),]
bb.stan<-na.omit(bb.stan)

bprior1 <- prior(normal(0,1), class="b") + prior(student_t(1,0,2), group="species", class="sd")

pois.nocen<-brm(fs.count~m.index+sp.temp+cc+sm.elev+space+m.index:cc + sp.temp:cc +sm.elev:cc +
                   space:cc + (0+m.index||species)+(0+sp.temp||species)+(0+cc||species), data=bb.stan, chains=2,future=TRUE,
                 prior=bprior1, family=poisson)

save(pois.nocen, file="/n/wolkovich_lab/Lab/Cat/nocen_pois_future.Rdata")