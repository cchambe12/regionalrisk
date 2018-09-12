## Cat - 4 September 2018
# Ben Goodrich suggested I try the stan_biglm function
# Working on honing the model and making plots!

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(rstan)
library(rstanarm)
library(sjPlot)
library(sjmisc)
library(RColorBrewer)
library(dplyr)
library(broom)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/output")

bb<-read.csv("regrisk.cleaned.2.csv", header=TRUE)

bb$nao.z <- (bb$m.index-mean(bb$m.index,na.rm=TRUE))/(2*sd(bb$m.index,na.rm=TRUE))
bb$mat.z <- (bb$sp.temp-mean(bb$sp.temp,na.rm=TRUE))/(2*sd(bb$sp.temp,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$space.z <- (bb$space-mean(bb$space,na.rm=TRUE))/(2*sd(bb$space,na.rm=TRUE))

bb<-bb[!duplicated(bb),]
bb<-na.omit(bb)
bb$species<-ifelse(bb$species=="FAGSYL", "aaFAGSYL", bb$species)

fit<-lm(fs.count~ nao.z + mat.z + elev.z  + 
          space.z + cc.z + species + nao.z:species + 
          mat.z:species + elev.z:species + space.z:species + cc.z:species + 
          nao.z:cc + mat.z:cc + elev.z:cc +
          space.z:cc, data=bb)             # not necessary in this case

b <- coef(fit)[-1]
R <- qr.R(fit$qr)[-1,-1]
SSR <- crossprod(fit$residuals)[1]
not_NA <- !is.na(fitted(fit))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$nao.z)),  as.numeric(mean(bb$mat.z)), as.numeric(mean(bb$elev.z)), as.numeric(mean(bb$space.z)), 
          as.numeric(mean(bb$cc.z)),  
          
          as.numeric(as.factor("AESHIP")),
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")),
          
          as.numeric(mean(bb$nao.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$nao.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$nao.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$nao.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$nao.z[bb$species=="QUEROB"])), 
          as.numeric(mean(bb$mat.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$mat.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$mat.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$mat.z[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$mat.z[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$elev.z[bb$species=="AESHIP"])), 
          as.numeric(mean(bb$elev.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$elev.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$elev.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$elev.z[bb$species=="QUEROB"])), 
          
          as.numeric(mean(bb$space.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$space.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$space.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$space.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$space.z[bb$species=="QUEROB"])),
          
          
          as.numeric(mean(bb$cc.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$cc.z[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc.z[bb$species=="BETPEN"])), 
          as.numeric(mean(bb$cc.z[bb$species=="FRAEXC"])), as.numeric(mean(bb$cc.z[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$nao.z*bb$cc.z)), as.numeric(mean(bb$mat.z*bb$cc.z)), as.numeric(mean(bb$elev.z*bb$cc.z)), 
          as.numeric(mean(bb$space.z*bb$cc.z)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
post.inter <- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                             # the next line is only to make the example go fast
                             chains = 4, iter = 2000)


