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

bb.stan$nao.z <- (bb.stan$m.index-mean(bb.stan$m.index,na.rm=TRUE))/sd(bb.stan$m.index,na.rm=TRUE)
bb.stan$mat.z <- (bb.stan$sp.temp-mean(bb.stan$sp.temp,na.rm=TRUE))/sd(bb.stan$sp.temp,na.rm=TRUE)
bb.stan$cc.z <- (bb.stan$cc-mean(bb.stan$cc,na.rm=TRUE))/sd(bb.stan$cc,na.rm=TRUE)
bb.stan$elev.z <- (bb.stan$sm.elev-mean(bb.stan$sm.elev,na.rm=TRUE))/sd(bb.stan$sm.elev,na.rm=TRUE)
bb.stan$space.z <- (bb.stan$space-mean(bb.stan$space,na.rm=TRUE))/sd(bb.stan$space,na.rm=TRUE)

fit<-lm(fs.count~ m.index + sp.temp + sm.elev  + 
          space + cc + species + m.index:species + 
          sp.temp:species + sm.elev:species + space:species + cc:species + 
          m.index:cc + sp.temp:cc + sm.elev:cc +
          space:cc, data=bb)             # not necessary in this case

b <- coef(fit)[-1]
R <- qr.R(fit$qr)[-1,-1]
SSR <- crossprod(fit$residuals)[1]
not_NA <- !is.na(fitted(fit))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$m.index)),  as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$sm.elev)), as.numeric(mean(bb$space)), 
          as.numeric(mean(bb$cc)),  
          
          as.numeric(as.factor("AESHIP")),
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")),
          
          as.numeric(mean(bb$m.index[bb$species=="AESHIP"])),
          as.numeric(mean(bb$m.index[bb$species=="ALNGLU"])), as.numeric(mean(bb$m.index[bb$species=="BETPEN"])),
          as.numeric(mean(bb$m.index[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$m.index[bb$species=="QUEROB"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="AESHIP"])),
          as.numeric(mean(bb$sp.temp[bb$species=="ALNGLU"])), as.numeric(mean(bb$sp.temp[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sp.temp[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$sm.elev[bb$species=="AESHIP"])), 
          as.numeric(mean(bb$sm.elev[bb$species=="ALNGLU"])), as.numeric(mean(bb$sm.elev[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sm.elev[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$sm.elev[bb$species=="QUEROB"])), 
          
          as.numeric(mean(bb$space[bb$species=="AESHIP"])),
          as.numeric(mean(bb$space[bb$species=="ALNGLU"])), as.numeric(mean(bb$space[bb$species=="BETPEN"])),
          as.numeric(mean(bb$space[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$space[bb$species=="QUEROB"])),
          
          
          as.numeric(mean(bb$cc[bb$species=="AESHIP"])),
          as.numeric(mean(bb$cc[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc[bb$species=="BETPEN"])), 
          as.numeric(mean(bb$cc[bb$species=="FRAEXC"])), as.numeric(mean(bb$cc[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$m.index*bb$cc)), as.numeric(mean(bb$sp.temp*bb$cc)), as.numeric(mean(bb$sm.elev*bb$cc)), 
          as.numeric(mean(bb$space*bb$cc)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
post.inter <- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                             # the next line is only to make the example go fast
                             chains = 4, iter = 2000)


