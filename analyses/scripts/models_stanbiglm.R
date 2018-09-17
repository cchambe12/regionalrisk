## Cat - 12 August 2018
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

bb<-read.csv("regrisk.fixed.csv", header=TRUE)
bb<-read.csv("bb_latprep.csv", header=TRUE)

#bb$sm.elev<-bb$elev/100
#bb<-na.omit(bb)
#write.csv(bb, file="~/Documents/git/regionalrisk/analyses/output/bb.brm2.csv", row.names = FALSE)
#bb$species<-as.numeric(as.factor(bb$species))

### Lines 27-67 are following Ben's example on the stan_biglm documentation
## Need a coastal parameter!

bb$nao.z <- (bb$m.index-mean(bb$m.index,na.rm=TRUE))/(2*sd(bb$m.index,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distance-mean(bb$distance,na.rm=TRUE))/(2*sd(bb$distance,na.rm=TRUE))

bb$species<-ifelse(bb$species=="FAGSYL", "aaFAGSYL", bb$species)

fit<-lm(fs.count~ nao.z + mat.z + elev.z  + lat.z + dist.z +
          cc.z + species + nao.z:species + 
          mat.z:species + elev.z:species + lat.z:species + dist.z:species + cc.z:species + 
          nao.z:cc + mat.z:cc + elev.z:cc + lat.z:cc.z + dist.z:cc.z, data=bb)             # not necessary in this case

b <- coef(fit)[-1]
R <- qr.R(fit$qr)[-1,-1]
SSR <- crossprod(fit$residuals)[1]
not_NA <- !is.na(fitted(fit))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$nao.z)), as.numeric(mean(bb$mat.z)),  as.numeric(mean(bb$elev.z)),  as.numeric(mean(bb$lat.z)), as.numeric(mean(bb$cc.z)),  
          
          as.numeric(as.factor("AESHIP")), as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")), 
          
          as.numeric(mean(bb$nao.z[bb$species=="AESHIP"])), as.numeric(mean(bb$nao.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$nao.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$nao.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$nao.z[bb$species=="QUEROB"])), 
          
          as.numeric(mean(bb$mat.z[bb$species=="AESHIP"])), as.numeric(mean(bb$mat.z[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$mat.z[bb$species=="BETPEN"])), as.numeric(mean(bb$mat.z[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$mat.z[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$elev.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$elev.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$elev.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$elev.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$elev.z[bb$species=="QUEROB"])), 
          
          as.numeric(mean(bb$lat.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$lat.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$lat.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$lat.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$lat.z[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$cc.z[bb$species=="AESHIP"])), 
          as.numeric(mean(bb$cc.z[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$cc.z[bb$species=="FRAEXC"])), as.numeric(mean(bb$cc.z[bb$species=="QUEROB"])),
          as.numeric(mean(bb$nao.z*bb$cc.z)), as.numeric(mean(bb$mat.z*bb$cc.z)), as.numeric(mean(bb$elev.z*bb$cc.z)), 
          as.numeric(mean(bb$lat.z*bb$cc.z)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
post.inter <- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                             # the next line is only to make the example go fast
                             chains = 4, iter = 2000)
cbind(lm = b, stan_lm = rstan::get_posterior_mean(post.inter)[1:26,]) # shrunk
# }


#######    OUTPUT... missing all speciesAESHIP - and not sure how to calculate 
## the rest of the species interactions. Is it like force x photo example? 
## So for m.index:species, would I do...
## (m.index + speciesALNGLU + m.index:speciesALNGLU) to find the species level effect of m.index?
# But then what do I do for AESHIP?

#                             mean se_mean   sd        2.5%         25%         50%         75%       97.5%
#(Intercept)                  2.31    0.00 0.18        1.96        2.19        2.31        2.43        2.67
#m.index                     -0.22    0.00 0.01       -0.23       -0.22       -0.22       -0.22       -0.21
#sp.temp                     -0.03    0.00 0.00       -0.04       -0.04       -0.03       -0.03       -0.03
#sm.elev                      0.11    0.00 0.00        0.11        0.11        0.11        0.11        0.11
#space                        0.90    0.00 0.01        0.89        0.90        0.90        0.90        0.91
#cc                           0.26    0.00 0.01        0.24        0.25        0.26        0.26        0.27
#speciesALNGLU               -0.02    0.00 0.01       -0.04       -0.03       -0.02       -0.01        0.00
#speciesBETPEN                0.10    0.00 0.01        0.08        0.10        0.10        0.11        0.12
#speciesFAGSYL               -0.06    0.00 0.01       -0.08       -0.07       -0.06       -0.06       -0.04
#speciesFRAEXC               -0.25    0.00 0.01       -0.28       -0.26       -0.25       -0.25       -0.23
#speciesQUEROB               -0.12    0.00 0.01       -0.14       -0.13       -0.12       -0.11       -0.10
#m.index:speciesALNGLU        0.02    0.00 0.01        0.01        0.02        0.02        0.03        0.04
#m.index:speciesBETPEN       -0.02    0.00 0.01       -0.04       -0.03       -0.02       -0.02       -0.01
#m.index:speciesFAGSYL        0.11    0.00 0.01        0.09        0.10        0.11        0.11        0.13
#m.index:speciesFRAEXC        0.18    0.00 0.01        0.16        0.17        0.18        0.18        0.19
#m.index:speciesQUEROB        0.15    0.00 0.01        0.14        0.15        0.15        0.16        0.17
#speciesALNGLU:sp.temp        0.00    0.00 0.00        0.00        0.00        0.00        0.01        0.01
#speciesBETPEN:sp.temp       -0.02    0.00 0.00       -0.02       -0.02       -0.02       -0.02       -0.02
#speciesFAGSYL:sp.temp        0.01    0.00 0.00        0.01        0.01        0.01        0.01        0.02
#speciesFRAEXC:sp.temp        0.04    0.00 0.00        0.04        0.04        0.04        0.04        0.04
#speciesQUEROB:sp.temp        0.02    0.00 0.00        0.02        0.02        0.02        0.02        0.02
#speciesALNGLU:sm.elev       -0.01    0.00 0.00       -0.02       -0.02       -0.01       -0.01       -0.01
#speciesBETPEN:sm.elev       -0.01    0.00 0.00       -0.01       -0.01       -0.01        0.00        0.00
#speciesFAGSYL:sm.elev       -0.03    0.00 0.00       -0.04       -0.03       -0.03       -0.03       -0.03
#speciesFRAEXC:sm.elev       -0.09    0.00 0.00       -0.10       -0.09       -0.09       -0.09       -0.09
#speciesQUEROB:sm.elev       -0.06    0.00 0.00       -0.07       -0.07       -0.06       -0.06       -0.06
#speciesALNGLU:space         -0.07    0.00 0.01       -0.09       -0.07       -0.07       -0.06       -0.04
#speciesBETPEN:space         -0.01    0.00 0.01       -0.02       -0.01       -0.01        0.00        0.01
#speciesFAGSYL:space         -0.26    0.00 0.01       -0.28       -0.27       -0.26       -0.26       -0.24
#speciesFRAEXC:space         -0.82    0.00 0.01       -0.84       -0.82       -0.82       -0.81       -0.80
#speciesQUEROB:space         -0.44    0.00 0.01       -0.46       -0.45       -0.44       -0.44       -0.42
#speciesALNGLU:cc            -0.01    0.00 0.01       -0.02       -0.01       -0.01        0.00        0.01
#speciesBETPEN:cc            -0.02    0.00 0.01       -0.03       -0.03       -0.02       -0.02       -0.01
#speciesFAGSYL:cc            -0.16    0.00 0.01       -0.17       -0.16       -0.16       -0.16       -0.15
#speciesFRAEXC:cc            -0.19    0.00 0.01       -0.20       -0.19       -0.19       -0.18       -0.18
#speciesQUEROB:cc            -0.16    0.00 0.01       -0.17       -0.17       -0.16       -0.16       -0.15
#m.index:cc                   0.09    0.00 0.00        0.08        0.09        0.09        0.10        0.10
#sp.temp:cc                  -0.01    0.00 0.00       -0.01       -0.01       -0.01       -0.01       -0.01
#sm.elev:cc                  -0.02    0.00 0.00       -0.02       -0.02       -0.02       -0.02       -0.01
#space:cc                     0.02    0.00 0.01        0.01        0.01        0.02        0.02        0.03
#sigma                        0.95    0.00 0.00        0.94        0.94        0.95        0.95        0.95
#log-fit_ratio                0.00    0.00 0.00        0.00        0.00        0.00        0.00        0.00
#R2                           0.15    0.00 0.00        0.15        0.15        0.15        0.15        0.16
#mean_PPD                     0.34    0.00 0.00        0.34        0.34        0.34        0.34        0.35
#log-posterior         -1475535.51    0.39 6.56 -1475549.17 -1475539.73 -1475535.12 -1475531.09 -1475523.93
#n_eff Rhat
#(Intercept)            1943 1.00
#m.index                3061 1.00
#sp.temp                2969 1.00
#sm.elev                4000 1.00
#space                  3301 1.00
#cc                     4000 1.00
#speciesALNGLU          4000 1.00
#speciesBETPEN          3066 1.00
#speciesFAGSYL          3040 1.00
#speciesFRAEXC          4000 1.00
#speciesQUEROB          4000 1.00
#m.index:speciesALNGLU  2417 1.00
#m.index:speciesBETPEN  4000 1.00
#m.index:speciesFAGSYL  2451 1.00
#m.index:speciesFRAEXC  4000 1.00
#m.index:speciesQUEROB  4000 1.00
#speciesALNGLU:sp.temp  4000 1.00
#speciesBETPEN:sp.temp  3075 1.00
#speciesFAGSYL:sp.temp  4000 1.00
#speciesFRAEXC:sp.temp  4000 1.00
#speciesQUEROB:sp.temp  4000 1.00
#speciesALNGLU:sm.elev  4000 1.00
#speciesBETPEN:sm.elev  4000 1.00
#speciesFAGSYL:sm.elev  4000 1.00
#speciesFRAEXC:sm.elev  4000 1.00
#speciesQUEROB:sm.elev  4000 1.00
#speciesALNGLU:space    4000 1.00
#speciesBETPEN:space    4000 1.00
#speciesFAGSYL:space    4000 1.00
#speciesFRAEXC:space    3738 1.00
#speciesQUEROB:space    4000 1.00
#speciesALNGLU:cc       4000 1.00
#speciesBETPEN:cc       4000 1.00
#speciesFAGSYL:cc       4000 1.00
#speciesFRAEXC:cc       4000 1.00
#speciesQUEROB:cc       4000 1.00
#m.index:cc             4000 1.00
#sp.temp:cc             2551 1.00
#sm.elev:cc             4000 1.00
#space:cc               2169 1.00
#sigma                  4000 1.00
#log-fit_ratio          4000 1.00
#R2                     3169 1.00
#mean_PPD               4000 1.00
#log-posterior           282 1.01

#Samples were drawn using NUTS(diag_e) at Wed Aug 29 11:43:39 2018.
#For each parameter, n_eff is a crude measure of effective sample size,
#and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat=1).


##### Interaction Plots code
nao<- plot_model(fit, type = "pred", terms = c("m.index", "species")) + xlab("NAO") + ylab("Number of False Springs") + ggtitle("") + theme(legend.position = "none")
elev<- plot_model(fit, type = "pred", terms = c("sm.elev", "species")) + xlab("(Small) Elevation") + ylab("Number of False Springs") + ggtitle("") + theme(legend.position = "none")
mat<- plot_model(fit, type = "pred", terms = c("sp.temp", "species")) + xlab("Mean Spring Temperature") + ylab("Number of False Springs") + ggtitle("") + theme(legend.position = "none")
space<- plot_model(fit, type = "pred", terms = c("space", "species")) + xlab("Space Parameter") + ylab("Number of False Springs") + ggtitle("") + theme(legend.position = "none")
cc<- plot_model(fit, type = "pred", terms = c("cc", "species")) + xlab("Climate Change") + ylab("Number of False Springs") + ggtitle("")

quartz()
ggarrange(nao, mat, cc, space, elev, ncol=3, nrow=2)

### A cleaner version! ####

### Going to make Fagus sylvatica the baseline because it is the most phenologically in the middle
# Avg budburst overall for the entire dataset is 104.8766 and Fagus is 106.7, Betula (the next closest) is 99.24

bb$species<-ifelse(bb$species=="FAGSYL", "aaFAGSYL", bb$species)

fit<-lm(fs.count~m.index + m.index:species + sp.temp + sp.temp:species + sm.elev + sm.elev:species + 
          space + space:species + cc  + cc:species + species + m.index:cc + sp.temp:cc +sm.elev:cc +
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
cbind(lm = b, stan_lm = rstan::get_posterior_mean(post.inter)[1:26,]) # shrunk
# }

### Output... Now speciesAESHIP is shown but the Intercept is negative and the 
## interaction terms are still missing AESHIP

#                             mean  se_mean sd        2.5%         25%         50%         75%       97.5%
#(Intercept)                 -0.25    0.00 0.04       -0.32       -0.28       -0.25       -0.22       -0.17
#m.index                     -0.22    0.00 0.01       -0.23       -0.22       -0.22       -0.22       -0.21
#sp.temp                     -0.03    0.00 0.00       -0.04       -0.04       -0.03       -0.03       -0.03
#sm.elev                      0.11    0.00 0.00        0.11        0.11        0.11        0.11        0.11
#space                        0.90    0.00 0.01        0.89        0.89        0.90        0.90        0.91
#cc                           0.26    0.00 0.01        0.24        0.25        0.26        0.26        0.27
#speciesAESHIP                0.22    0.00 0.01        0.20        0.21        0.22        0.22        0.23
#speciesALNGLU                0.20    0.00 0.01        0.18        0.19        0.20        0.20        0.21
#speciesBETPEN                0.32    0.00 0.01        0.31        0.32        0.32        0.32        0.33
#speciesFAGSYL                0.15    0.00 0.01        0.14        0.15        0.15        0.16        0.17
#speciesFRAEXC               -0.04    0.00 0.01       -0.05       -0.04       -0.04       -0.03       -0.02
#speciesQUEROB                0.10    0.00 0.01        0.08        0.09        0.10        0.10        0.11
#m.index:speciesALNGLU        0.02    0.00 0.01        0.01        0.02        0.02        0.03        0.04
#m.index:speciesBETPEN       -0.02    0.00 0.01       -0.04       -0.03       -0.02       -0.02       -0.01
#m.index:speciesFAGSYL        0.11    0.00 0.01        0.09        0.10        0.11        0.11        0.12
#m.index:speciesFRAEXC        0.18    0.00 0.01        0.16        0.17        0.18        0.18        0.19
#m.index:speciesQUEROB        0.15    0.00 0.01        0.14        0.15        0.15        0.16        0.17
#speciesALNGLU:sp.temp        0.00    0.00 0.00        0.00        0.00        0.00        0.01        0.01
#speciesBETPEN:sp.temp       -0.02    0.00 0.00       -0.02       -0.02       -0.02       -0.02       -0.02
#speciesFAGSYL:sp.temp        0.01    0.00 0.00        0.01        0.01        0.01        0.01        0.02
#speciesFRAEXC:sp.temp        0.04    0.00 0.00        0.04        0.04        0.04        0.04        0.04
#speciesQUEROB:sp.temp        0.02    0.00 0.00        0.02        0.02        0.02        0.02        0.02
#speciesALNGLU:sm.elev       -0.01    0.00 0.00       -0.02       -0.02       -0.01       -0.01       -0.01
#speciesBETPEN:sm.elev       -0.01    0.00 0.00       -0.01       -0.01       -0.01        0.00        0.00
#speciesFAGSYL:sm.elev       -0.03    0.00 0.00       -0.04       -0.03       -0.03       -0.03       -0.03
#speciesFRAEXC:sm.elev       -0.09    0.00 0.00       -0.10       -0.09       -0.09       -0.09       -0.09
#speciesQUEROB:sm.elev       -0.06    0.00 0.00       -0.07       -0.07       -0.06       -0.06       -0.06
#speciesALNGLU:space         -0.07    0.00 0.01       -0.09       -0.07       -0.07       -0.06       -0.04
#speciesBETPEN:space         -0.01    0.00 0.01       -0.02       -0.01       -0.01        0.00        0.01
#speciesFAGSYL:space         -0.26    0.00 0.01       -0.28       -0.27       -0.26       -0.26       -0.24
#speciesFRAEXC:space         -0.82    0.00 0.01       -0.84       -0.83       -0.82       -0.81       -0.80
#speciesQUEROB:space         -0.44    0.00 0.01       -0.46       -0.45       -0.44       -0.44       -0.43
#speciesALNGLU:cc            -0.01    0.00 0.01       -0.02       -0.01       -0.01        0.00        0.00
#speciesBETPEN:cc            -0.02    0.00 0.01       -0.03       -0.03       -0.02       -0.02       -0.01
#speciesFAGSYL:cc            -0.16    0.00 0.01       -0.17       -0.16       -0.16       -0.16       -0.15
#speciesFRAEXC:cc            -0.19    0.00 0.01       -0.20       -0.19       -0.19       -0.18       -0.18
#speciesQUEROB:cc            -0.16    0.00 0.01       -0.17       -0.17       -0.16       -0.16       -0.15
#m.index:cc                   0.09    0.00 0.00        0.08        0.09        0.09        0.10        0.10
#sp.temp:cc                  -0.01    0.00 0.00       -0.01       -0.01       -0.01       -0.01       -0.01
#sm.elev:cc                  -0.02    0.00 0.00       -0.02       -0.02       -0.02       -0.02       -0.01
#space:cc                     0.02    0.00 0.01        0.01        0.01        0.02        0.02        0.03
#sigma                        0.95    0.00 0.00        0.94        0.94        0.95        0.95        0.95
#log-fit_ratio                0.05    0.00 0.00        0.05        0.05        0.05        0.05        0.05
#R2                           0.24    0.00 0.00        0.24        0.24        0.24        0.24        0.24
#mean_PPD                     0.34    0.00 0.00        0.34        0.34        0.34        0.34        0.35
#log-posterior         -1475529.29    0.45 6.89 -1475543.97 -1475533.85 -1475528.89 -1475524.38 -1475517.00
#n_eff Rhat
#(Intercept)            2140 1.00
#m.index                4000 1.00
#sp.temp                2329 1.00
#sm.elev                3219 1.00
#space                  4000 1.00
#cc                     4000 1.00
#speciesAESHIP          2247 1.00
#speciesALNGLU          4000 1.00
#speciesBETPEN          4000 1.00
#speciesFAGSYL          4000 1.00
#speciesFRAEXC          4000 1.00
#speciesQUEROB          4000 1.00
#m.index:speciesALNGLU  3568 1.00
#m.index:speciesBETPEN  4000 1.00
#m.index:speciesFAGSYL  3175 1.00
#m.index:speciesFRAEXC  4000 1.00
#m.index:speciesQUEROB  4000 1.00
#speciesALNGLU:sp.temp  4000 1.00
#speciesBETPEN:sp.temp  3386 1.00
#speciesFAGSYL:sp.temp  4000 1.00
#speciesFRAEXC:sp.temp  4000 1.00
#speciesQUEROB:sp.temp  4000 1.00
#speciesALNGLU:sm.elev  4000 1.00
#speciesBETPEN:sm.elev  3706 1.00
#speciesFAGSYL:sm.elev  4000 1.00
#speciesFRAEXC:sm.elev  4000 1.00
#speciesQUEROB:sm.elev  3924 1.00
#speciesALNGLU:space    4000 1.00
#speciesBETPEN:space    4000 1.00
#speciesFAGSYL:space    4000 1.00
#speciesFRAEXC:space    4000 1.00
#speciesQUEROB:space    4000 1.00
#speciesALNGLU:cc       4000 1.00
#speciesBETPEN:cc       4000 1.00
#speciesFAGSYL:cc       4000 1.00
#speciesFRAEXC:cc       4000 1.00
#speciesQUEROB:cc       4000 1.00
#m.index:cc             4000 1.00
#sp.temp:cc             2841 1.00
#sm.elev:cc             4000 1.00
#space:cc               3020 1.00
#sigma                  4000 1.00
#log-fit_ratio          4000 1.00
#R2                     4000 1.00
#mean_PPD               3926 1.00
#log-posterior           235 1.02

#Samples were drawn using NUTS(diag_e) at Wed Aug 29 12:53:07 2018.
#For each parameter, n_eff is a crude measure of effective sample size,
#and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat=1).

### Now prep like an rstan model ####

bb$sp<-as.numeric(as.factor(bb$species))
fit.sp<-lm(fs.count~ m.index + m.index:sp + sp.temp + sp.temp:sp + sm.elev + sm.elev:sp + 
          space + space:sp + cc  + cc:sp + sp + m.index:cc + sp.temp:cc +sm.elev:cc +
          space:cc, data=bb)             # not necessary in this case

b <- coef(fit.sp)[-1]
R <- qr.R(fit.sp$qr)[-1,-1]
SSR <- crossprod(fit.sp$residuals)[1]
not_NA <- !is.na(fitted(fit.sp))
N <- sum(not_NA)
spp.nao<-unique(ave(bb$m.index, bb$sp))
spp.mat<-unique(ave(bb$sp.temp, bb$sp))
spp.cc<-unique(ave(bb$cc, bb$sp))
spp.elev<-unique(ave(bb$sm.elev, bb$sp))
spp.space<-unique(ave(bb$space, bb$sp))
xbar <- c(as.numeric(mean(bb$m.index)),  as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$sm.elev)), as.numeric(mean(bb$space)), 
          as.numeric(mean(bb$cc)),  
          
          as.numeric(as.factor("AESHIP")),
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FAGSYL")), as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")),
          
          spp.nao, spp.mat, spp.cc, spp.elev, spp.space,
          
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
cbind(lm = b, stan_lm = rstan::get_posterior_mean(post.inter)[1:26,]) # shrunk
# }




##### Ugly but workable code for plotting ####
plotting <- as.data.frame(summary(post.inter)$summary)
simple<-plotting
simple$var<- rownames(simple)
rownames(simple)<-1:45
simple<-simple[2:40,]
#simple<-simple[!(simple$var=="speciesALNGLU"|simple$var=="speciesBETPEN"|simple$var=="speciesFAGSYL"|
 #simple$var=="speciesFRAEXC"|simple$var=="speciesQUEROB"),]
simple<-subset(simple, select=c("var", "mean", "2.5%", "97.5%"))
simple$species<-c(1,1,1,1,1,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,0,0,0,0)
simple$Jvar<-NA
#simple$Jvar<-ifelse(simple$var=="(Intercept)", 10, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index", 9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesALNGLU", 8.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesBETPEN", 8.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesFAGSYL", 8.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesFRAEXC", 8.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesQUEROB", 8.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="sp.temp", 8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesALNGLU", 7.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesBETPEN", 7.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesFAGSYL", 7.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesFRAEXC", 7.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesQUEROB", 7.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="sm.elev", 7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesALNGLU", 6.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesBETPEN", 6.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesFAGSYL", 6.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesFRAEXC", 6.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesQUEROB", 6.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="space", 6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesALNGLU", 5.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesBETPEN", 5.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesFAGSYL", 5.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesFRAEXC", 5.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesQUEROB", 5.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="cc", 5, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesALNGLU", 4.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesBETPEN", 4.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesFAGSYL", 4.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesFRAEXC", 4.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesQUEROB", 4.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="m.index:cc", 4, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="sp.temp:cc", 3, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="sm.elev:cc", 2, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="space:cc", 1, simple$Jvar)

species<-unique(simple$species)
simple$est<-simple$mean
simple$var2<-gsub("(species).*","\\1",simple$var)
for(i in c(1:length(species))) {
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="m.index:species", simple$mean[simple$var=="m.index" & simple$species==1] +
                        simple$mean[simple$var2=="m.index:species" & simple$species==species[i]], simple$est)
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="sp.temp:species", simple$mean[simple$var=="sp.temp" & simple$species==1] +
                        simple$mean[simple$var2=="sp.temp:species" & simple$species==species[i]], simple$est)
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="sm.elev:species", simple$mean[simple$var=="sm.elev" & simple$species==1] +
                       simple$mean[simple$var2=="sm.elev:species" & simple$species==species[i]], simple$est)
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="space:species", simple$mean[simple$var=="space"& simple$species==1] +
                       simple$mean[simple$var2=="space:species" & simple$species==species[i]], simple$est)
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="cc:species", simple$mean[simple$var=="cc"& simple$species==1] +
                       simple$mean[simple$var2=="cc:species" & simple$species==species[i]], simple$est)
  
}

for(i in c(1:length(species))) {
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="m.index:species", simple$mean[simple$var=="m.index" & simple$species==1] +
                       simple$mean[simple$var2=="m.index:species" & simple$species==species[i]], simple$`2.5%`)
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="sp.temp:species", simple$mean[simple$var=="sp.temp" & simple$species==1] +
                       simple$mean[simple$var2=="sp.temp:species" & simple$species==species[i]], simple$`2.5%`)
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="sm.elev:species", simple$mean[simple$var=="sm.elev" & simple$species==1] +
                       simple$mean[simple$var2=="sm.elev:species" & simple$species==species[i]], simple$`2.5%`)
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="space:species", simple$mean[simple$var=="space"& simple$species==1] +
                       simple$mean[simple$var2=="space:species" & simple$species==species[i]], simple$`2.5%`)
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="cc:species", simple$mean[simple$var=="cc"& simple$species==1] +
                       simple$mean[simple$var2=="cc:species" & simple$species==species[i]], simple$`2.5%`)
  
}

for(i in c(1:length(species))) {
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="m.index:species", simple$mean[simple$var=="m.index" & simple$species==1] +
                          simple$mean[simple$var2=="m.index:species" & simple$species==species[i]], simple$`97.5%`)
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="sp.temp:species", simple$mean[simple$var=="sp.temp" & simple$species==1] +
                          simple$mean[simple$var2=="sp.temp:species" & simple$species==species[i]], simple$`97.5%`)
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="sm.elev:species", simple$mean[simple$var=="sm.elev" & simple$species==1] +
                          simple$mean[simple$var2=="sm.elev:species" & simple$species==species[i]], simple$`97.5%`)
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="space:species", simple$mean[simple$var=="space"& simple$species==1] +
                          simple$mean[simple$var2=="space:species" & simple$species==species[i]], simple$`97.5%`)
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="cc:species", simple$mean[simple$var=="cc"& simple$species==1] +
                          simple$mean[simple$var2=="cc:species" & simple$species==species[i]], simple$`97.5%`)
  
}


simple<-simple[!(simple$var=="speciesALNGLU"|simple$var=="speciesBETPEN"|simple$var=="speciesFAGSYL"|
                   simple$var=="speciesFRAEXC"|simple$var=="speciesQUEROB"),]


cols <- colorRampPalette(brewer.pal(9,"Set1"))(7)
estimates<-c("NAO Index", "Mean Spring Temperature", "Elevation", "Space Parameter", "Climate Change",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change",
             "Elevation x \nClimate Chnage", "Space Parameter \nx Climate Change")
estimates<-rev(estimates)
regrisk<-ggplot(simple, aes(x=`2.5%`, xend=`97.5%`, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=mean, y=Jvar), col="blue3") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc")), col="blue3") +
  scale_y_discrete(limits = sort(unique(simple$var)), labels=estimates) +
  xlab("Model Estimate Change in \nNumber of False Springs") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0)  #+ coord_cartesian(ylim=c(1,5), xlim=c(-20, 10))
quartz()
regrisk

brms<-as.data.frame(tidy(slopes.fast,robust = TRUE))
brms<-brms[2:46,]
brms$term<-gsub(".*b_","",brms$term)
brms$term<-gsub(".*r_species","",brms$term)
brms<-brms[!(brms$term=="sd_species__m.index" | brms$term=="sd_species__sp.temp" | brms$term=="sd_species__sm.elev"
           | brms$term=="sd_species__space" | brms$term=="sd_species__cc" | brms$term=="sigma"),]

brms$species<-c(0,0,0,0,0,0,0,0,0, 1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)
brms$Jvar<-NA
brms$Jvar<-ifelse(brms$term=="m.index", 9, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[AESHIP,m.index]", 8.9, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[ALNGLU,m.index]", 8.8, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[BETPEN,m.index]", 8.7, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FAGSYL,m.index]", 8.6, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FRAEXC,m.index]", 8.5, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[QUEROB,m.index]", 8.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="sp.temp", 8, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[AESHIP,sp.temp]", 7.9, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[ALNGLU,sp.temp]", 7.8, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[BETPEN,sp.temp]", 7.7, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FAGSYL,sp.temp]", 7.6, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FRAEXC,sp.temp]", 7.5, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[QUEROB,sp.temp]", 7.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="sm.elev", 7, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[AESHIP,sm.elev]", 6.9, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[ALNGLU,sm.elev]", 6.8, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[BETPEN,sm.elev]", 6.7, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FAGSYL,sm.elev]", 6.6, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FRAEXC,sm.elev]", 6.5, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[QUEROB,sm.elev]", 6.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="space", 6, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[AESHIP,space]", 5.9, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[ALNGLU,space]", 5.8, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[BETPEN,space]", 5.7, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FAGSYL,space]", 5.6, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FRAEXC,space]", 5.5, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[QUEROB,space]", 5.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="cc", 5, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[AESHIP,cc]", 4.9, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[ALNGLU,cc]", 4.8, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[BETPEN,cc]", 4.7, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FAGSYL,cc]", 4.6, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[FRAEXC,cc]", 4.5, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="[QUEROB,cc]", 4.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="m.index:cc", 4, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="sp.temp:cc", 3, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="cc:sm.elev", 2, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="cc:space", 1, brms$Jvar)

cols <- colorRampPalette(brewer.pal(9,"Set1"))(7)
estimates<-c("NAO Index", "Mean Spring Temperature", "Elevation", "Space Parameter", "Climate Change",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change",
             "Elevation x \nClimate Chnage", "Space Parameter \nx Climate Change")
estimates<-rev(estimates)
regrisk<-ggplot(brms, aes(x=lower, xend=upper, y=Jvar, yend=Jvar, col=as.factor(species))) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=Jvar, col=as.factor(species), size=as.factor(species))) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("1"=expression(paste(italic("Aesculus hippocastanum"))),
                               "2"=expression(paste(italic("Alnus glutinosa"))),
                               "3"=expression(paste(italic("Betula lenta"))),
                               "4"=expression(paste(italic("Fagus sylvatica"))),
                               "5"=expression(paste(italic("Fraxinus excelsior"))),
                               "6"=expression(paste(italic("Quercus robur"))),
                               "0"="Overall Effects"))+
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  scale_y_discrete(limits = sort(unique(brms$term)), labels=estimates) +
  xlab("Change in Number of False Springs") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = c(0.85,0.25),
        legend.text.align = 0) + #+ coord_cartesian(ylim=c(1,5), xlim=c(-20, 10))
  scale_size_manual(values=c(3, 1, 1, 1, 1, 1, 1, 1, 1, 1), name="Species",
                    labels=c("1"=expression(paste(italic("Aesculus hippocastanum"))),
                             "2"=expression(paste(italic("Alnus glutinosa"))),
                             "3"=expression(paste(italic("Betula lenta"))),
                             "4"=expression(paste(italic("Fagus sylvatica"))),
                             "5"=expression(paste(italic("Fraxinus excelsior"))),
                             "6"=expression(paste(italic("Quercus robur"))),
                             "0"="Overall Effects"))
quartz()
regrisk

nao<-interact_plot(model = slopes.fast, pred = m.index, modx = cc) + xlab("NAO Index") + ylab("Num. False Springs") + theme(legend.position = "none")
elev<-interact_plot(model = slopes.fast, pred = sm.elev, modx = cc) + xlab("Elevation") + ylab("Num. False Springs") + theme(legend.position = "none")
mat<-interact_plot(model = slopes.fast, pred = sp.temp, modx = cc) + xlab("Mean Spring Temperature") + ylab("Num. False Springs") 
spa<-interact_plot(model = slopes.fast, pred = space, modx = cc) + xlab("Space") + ylab("Num. False Springs")

ggarrange(nao, mat, elev, spa, ncol=2, nrow=2)

test<-lm(fs.count~m.index+sp.temp+sm.elev+space+cc+species+m.index:cc+
           sp.temp:cc+sm.elev:cc+space:cc, data=bb)
b <- coef(test)[-1]
R <- qr.R(test$qr)[-1,-1]
SSR <- crossprod(test$residuals)[1]
not_NA <- !is.na(fitted(test))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$m.index)),  as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$sm.elev)), as.numeric(mean(bb$space)), 
          as.numeric(mean(bb$cc)),
          
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FAGSYL")), as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")),
          
          as.numeric(mean(bb$m.index*bb$cc)), as.numeric(mean(bb$sp.temp*bb$cc)), as.numeric(mean(bb$sm.elev*bb$cc)), 
          as.numeric(mean(bb$space*bb$cc)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
test.big<- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                             # the next line is only to make the example go fast
                             chains = 4, iter = 2000)

