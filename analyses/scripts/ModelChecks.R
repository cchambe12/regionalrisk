library(brms)
library(rstanarm)
library(bayesplot)
library(ggplot2)

setwd("~/Documents/git/regionalrisk/analyses/output")
bb<-read.csv("fs_space_new.csv", header=TRUE)

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$eigen-mean(bb$eigen,na.rm=TRUE))/(2*sd(bb$eigen,na.rm=TRUE))

bb<-bb[sample(nrow(bb), 250000), ]


(loo1 <- loo(binomial))
(waic1 <- waic(hurdle))
# perform 10-fold cross validation
(kfold1 <- brms::kfold(binom, chains=1))
temp_base.test <- update(base.test, chains = 0)


pp_check(bernbetterpriorsquart, type="stat_grouped", group="species")

yrep_nb <- posterior_predict(bernbetterpriorsquart)
y <- bb$fs
q25 <- function(y) quantile(y, 0.25)
ppc_stat(y, yrep_nb,  stat = "min")
