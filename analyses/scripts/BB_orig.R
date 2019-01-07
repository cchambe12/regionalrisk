## Libraries
### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
library(rstan)
library(brms)
library(dplyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fs<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_newspace_orig.csv", header=TRUE)
#fs<-read.csv("~/Documents/git/regionalrisk/analyses/output/fs_newspace_orig.csv", header=TRUE)
bb<-read.csv("/n/wolkovich_lab/Lab/Cat/BBdata.csv", header=TRUE)
#bb<-read.csv("~/Documents/git/regionalrisk/analyses/output/BBdata.csv", header=TRUE)
bb<-full_join(bb, fs)

bb<-bb[!duplicated(bb),]
bb<-na.omit(bb)

bb<-subset(bb, select=c("species", "lat", "elev", "year", "mst", "cc", "fs.count", "nao",
                        "distkm", "eigen", "bb"))

bb$fs<-ifelse(bb$fs.count>0, 1, 0)
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$bb.z <- (bb$bb-mean(bb$bb,na.rm=TRUE))/(2*sd(bb$bb,na.rm=TRUE))

bbmod<-brm(fs~cc.z+bb.z+(cc.z+bb.z|species), data=bb, chains=2,family=bernoulli(), cores=2, iter = 4000, 
           warmup=2500, prior = prior(normal(0,1), class = "b"))



save(bbmod, file="/n/wolkovich_lab/Lab/Cat/bbmod.Rdata")


