## Building fake data for regional risk model choice
# Based on script by Dan Flynn and Lizzie from Buds repo 
## Updated: 24 October 2018 - Cat

# Basic housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Fake data for buburst stan work #
library(dplyr)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: 6 species, two levels for CC, 11648 sites, mst, nao, elev, eigen, dist
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsp = 6     #total number of species
years= 10 #number of years
nind = 30 # number of individuals per species
N = years*nind

# Build up the data frame
sp = gl(nsp,  nind, length=N)

cc <- rep(c(0,1),each=N/2) #assign half to pre 1984 (0) and half to post (1)
mst <- rnorm(N, 8, 1.5)
nao <- rnorm(N, 0.08, 0.3)
space <- rnorm(N, -0.03, 0.33)
elev <- rnorm(N, 16, 21)
dist <- rnorm(N, 20, 27)

###### Set up differences for each level
mstdiff = -1
naodiff = 0.3
spacediff = -0.01
elevdiff = 0.8
distdiff = -0.4
spdiff = 1.5
ccdiff = -0.6

######## SD for each treatment
mstdiff.sd = 0.2
naodiff.sd = 0.05
spacediff.sd = 0.05
elevdiff.sd = 0.2
distdiff.sd = 0.1
spdiff.sd = 0.5
ccdiff.sd = 0.1

mm <- model.matrix(~(cc+nao+mst+elev+space+dist), data.frame(cc,nao,mst,elev,space,dist)) ### ORDER HERE REALLY MATTERS!!! MAKE SURE IT LINES UP WITH "COEFF"
## Coding check below - keep for future tweaks to code/data
#coeff <- c(1, spdiff, txdiff)
#risk <- rnorm(n = length(tx), mean = mm%*%coeff, sd = 1) # should be able to do sd = mm %*% sd.coeff as well, with a different sd for each parameter.
#(fake <- data_frame(risk, tx, sp))

##### Again, now with individuals.

baseinter = 1 # baseline intercept across all individuals for DVR
spint <- baseinter + c(1:nind)-mean(1:nind) # different intercepts by individual

fake <- vector()

for(i in 1:nind){ # loop over individual (random effect of model)
  
  # Give individuals different difference values, drawn from normal
  
  coeff <- c(spint[i], 
             rnorm(1, ccdiff, ccdiff.sd),
             rnorm(1, naodiff, naodiff.sd),
             rnorm(1, mstdiff, mstdiff.sd),
             rnorm(1, elevdiff, elevdiff.sd),
             rnorm(1, spacediff, spacediff.sd),
             rnorm(1, distdiff, distdiff.sd)
  )
  
  fs <- rnorm(n = length(sp), mean = mm %*% coeff, sd = 0.1)
  
  fakex <- data.frame(fs, ind=i, sp, cc, nao, mst, elev, space, dist)
  
  fake<-rbind(fake, fakex)
}    

mod<-lm(fs ~ cc+sp+nao+mst+elev+space+dist, data = fake) # sanity check 
summary(mod)

### run models
fake$nao.z <- (fake$nao-mean(fake$nao,na.rm=TRUE))/(2*sd(fake$nao,na.rm=TRUE))
fake$mat.z <- (fake$mst-mean(fake$mst,na.rm=TRUE))/(2*sd(fake$mst,na.rm=TRUE))
fake$cc.z <- (fake$cc-mean(fake$cc,na.rm=TRUE))/(2*sd(fake$cc,na.rm=TRUE))
fake$elev.z <- (fake$elev-mean(fake$elev,na.rm=TRUE))/(2*sd(fake$elev,na.rm=TRUE))
fake$dist.z <-(fake$dist-mean(fake$dist,na.rm=TRUE))/(2*sd(fake$dist,na.rm=TRUE))
fake$space.z <-(fake$space-mean(fake$space,na.rm=TRUE))/(2*sd(fake$space,na.rm=TRUE))

gaus<-brm(fs ~ nao.z + mat.z + cc.z + elev.z + dist.z + space.z, data=fake)


