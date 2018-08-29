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

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/output")

bb<-read.csv("regrisk.cleaned.2.csv", header=TRUE)

bb$sm.elev<-bb$elev/100
bb<-na.omit(bb)
#bb$species<-as.numeric(as.factor(bb$species))

### Lines 27-67 are following Ben's example on the stan_biglm documentation

fit<-lm(fs.count~ m.index + m.index:species + sp.temp + sp.temp:species + sm.elev + sm.elev:species + 
          space + space:species + cc  + cc:species + species + m.index:cc + sp.temp:cc +sm.elev:cc +
          space:cc, data=bb)             # not necessary in this case

b <- coef(fit)[-1]
R <- qr.R(fit$qr)[-1,-1]
SSR <- crossprod(fit$residuals)[1]
not_NA <- !is.na(fitted(fit))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$m.index)),  as.numeric(mean(bb$m.index[bb$species=="ALNGLU"])), as.numeric(mean(bb$m.index[bb$species=="BETPEN"])),
          as.numeric(mean(bb$m.index[bb$species=="FAGSYL"])), as.numeric(mean(bb$m.index[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$m.index[bb$species=="QUEROB"])), as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$sp.temp[bb$species=="ALNGLU"])), as.numeric(mean(bb$sp.temp[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sp.temp[bb$species=="FAGSYL"])), as.numeric(mean(bb$sp.temp[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="QUEROB"])),
          as.numeric(mean(bb$sm.elev)),  
          as.numeric(mean(bb$sm.elev[bb$species=="ALNGLU"])), as.numeric(mean(bb$sm.elev[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sm.elev[bb$species=="FAGSYL"])), as.numeric(mean(bb$sm.elev[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$sm.elev[bb$species=="QUEROB"])), 
          as.numeric(mean(bb$space)), 
          as.numeric(mean(bb$space[bb$species=="ALNGLU"])), as.numeric(mean(bb$space[bb$species=="BETPEN"])),
          as.numeric(mean(bb$space[bb$species=="FAGSYL"])), as.numeric(mean(bb$space[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$space[bb$species=="QUEROB"])),
          as.numeric(mean(bb$cc)),  
          as.numeric(mean(bb$cc[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc[bb$species=="BETPEN"])), as.numeric(mean(bb$cc[bb$species=="FAGSYL"])), 
          as.numeric(mean(bb$cc[bb$species=="FRAEXC"])), as.numeric(mean(bb$cc[bb$species=="QUEROB"])),
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FAGSYL")), as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")), 
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



### Now a no-intercept model ####

fit<-lm(fs.count~ -1 + m.index + m.index:species + sp.temp + sp.temp:species + sm.elev + sm.elev:species + 
          space + space:species + cc  + cc:species + species + m.index:cc + sp.temp:cc +sm.elev:cc +
          space:cc, data=bb)             # not necessary in this case

b <- coef(fit)
R <- qr.R(fit$qr)
SSR <- crossprod(fit$residuals)[1]
not_NA <- !is.na(fitted(fit))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$m.index)),  as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$sm.elev)), as.numeric(mean(bb$space)), 
          as.numeric(mean(bb$cc)),  
          
          as.numeric(as.factor("AESHIP")),
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FAGSYL")), as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")),
    
          as.numeric(mean(bb$m.index[bb$species=="ALNGLU"])), as.numeric(mean(bb$m.index[bb$species=="BETPEN"])),
          as.numeric(mean(bb$m.index[bb$species=="FAGSYL"])), as.numeric(mean(bb$m.index[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$m.index[bb$species=="QUEROB"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="ALNGLU"])), as.numeric(mean(bb$sp.temp[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sp.temp[bb$species=="FAGSYL"])), as.numeric(mean(bb$sp.temp[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$sm.elev[bb$species=="ALNGLU"])), as.numeric(mean(bb$sm.elev[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sm.elev[bb$species=="FAGSYL"])), as.numeric(mean(bb$sm.elev[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$sm.elev[bb$species=="QUEROB"])), 
          
          as.numeric(mean(bb$space[bb$species=="ALNGLU"])), as.numeric(mean(bb$space[bb$species=="BETPEN"])),
          as.numeric(mean(bb$space[bb$species=="FAGSYL"])), as.numeric(mean(bb$space[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$space[bb$species=="QUEROB"])),
          
          
          as.numeric(mean(bb$cc[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc[bb$species=="BETPEN"])), as.numeric(mean(bb$cc[bb$species=="FAGSYL"])), 
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



##### Ugly but workable code for plotting ####
plotting <- as.data.frame(summary(post.inter)$summary)
simple<-plotting
simple$var<- rownames(simple)
rownames(simple)<-1:45
simple<-simple[2:40,]
simple<-simple[!(simple$var=="speciesALNGLU"|simple$var=="speciesBETPEN"|simple$var=="speciesFAGSYL"|
                   simple$var=="speciesFRAEXC"|simple$var=="speciesQUEROB"),]
simple<-subset(simple, select=c("var", "mean", "2.5%", "97.5%"))
simple$species<-c(0,0,0,0,0,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,0,0,0,0)
simple$Jvar<-NA
simple$Jvar<-ifelse(simple$var=="(Intercept)", 10, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index", 9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesALNGLU", 8.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesBETPEN", 8.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesFAGSYL", 8.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesFRAEXC", 8.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesQUEROB", 8.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="sp.temp", 8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesALNGLU:sp.temp", 7.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesBETPEN:sp.temp", 7.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFAGSYL:sp.temp", 7.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFRAEXC:sp.temp", 7.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesQUEROB:sp.temp", 7.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="sm.elev", 7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesALNGLU:sm.elev", 6.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesBETPEN:sm.elev", 6.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFAGSYL:sm.elev", 6.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFRAEXC:sm.elev", 6.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesQUEROB:sm.elev", 6.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="space", 6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesALNGLU:space", 5.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesBETPEN:space", 5.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFAGSYL:space", 5.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFRAEXC:space", 5.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesQUEROB:space", 5.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="cc", 5, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesALNGLU:cc", 4.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesBETPEN:cc", 4.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFAGSYL:cc", 4.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFRAEXC:cc", 4.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesQUEROB:cc", 4.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="m.index:cc", 4, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="sp.temp:cc", 3, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="sm.elev:cc", 2, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="space:cc", 1, simple$Jvar)

species<-unique(simple$species)
simple$est2<-simple$mean
for(i in c(1:length(species))) {
  simple$est2<-ifelse(simple$sp==species[i] & simple$var=="force:photo", simple$newEstimate[simple$var=="force" & simple$sp==species[i]]+
                        simple$newEstimate[simple$var=="photo" & simple$sp==species[i]]+
                        simple$newEstimate[simple$var=="force:photo" & simple$sp==species[i]], simple$est2)
  simple$est2<-ifelse(simple$sp==species[i] & simple$var=="force:chill", simple$newEstimate[simple$var=="force" & simple$sp==species[i]]+
                        simple$newEstimate[simple$var=="chill" & simple$sp==species[i]]+
                        simple$newEstimate[simple$var=="force:chill" & simple$sp==species[i]], simple$est2)
  
}

cols <- colorRampPalette(brewer.pal(9,"Set1"))(7)
estimates<-c("Intercept (AESHIP)", "NAO Index", "Mean Spring Temperature", "Elevation", "Space Parameter", "Climate Change",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change",
             "Elevation x \nClimate Chnage", "Space Parameter \nx Climate Change")
estimates<-rev(estimates)
regrisk<-ggplot(simple, aes(x=`2.5%`, xend=`97.5%`, y=Jvar, yend=Jvar, col=as.factor(species))) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=mean, y=Jvar, col=as.factor(species))) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("1"=expression(paste(italic("Aesculus hippocastanum"))),
                               "2"=expression(paste(italic("Alnus glutinosa"))),
                               "3"=expression(paste(italic("Betula lenta"))),
                               "4"=expression(paste(italic("Fagus sylvatica"))),
                               "5"=expression(paste(italic("Fraxinus excelsior"))),
                               "6"=expression(paste(italic("Quercus robur"))),
                               "0"="Overall Effect"))+
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  scale_y_discrete(limits = sort(unique(simple$var)), labels=estimates) +
  xlab("Change in Number of False Springs") + ylab("") +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = c(0.85,0.25),
        legend.text.align = 0) #+ coord_cartesian(ylim=c(1,5), xlim=c(-20, 10))
quartz()
regrisk



