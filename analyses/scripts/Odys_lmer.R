## Cat - 12 August 2018
# Ben Goodrich suggested I try the stan_biglm function

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(lme4)

bb<-read.csv("/n/wolkovich_lab/Lab/Cat/bb.brm.nointer.csv", header=TRUE)

fit2<-lmer(fs.count~m.index+sp.temp+cc+sm.elev+space+m.index:cc+sp.temp:cc+
             sm.elev:cc+space:cc+(m.index+sp.temp+cc|species), data=bb)

save(fit2, file="/n/wolkovich_lab/Lab/Cat/lmer_all.Rdata")