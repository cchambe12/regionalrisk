## Building fake data for experiment data and duration of vegetative risk
# Based on script by Dan Flynn and Lizzie from Buds repo 
## Updated: 6 November 2017 - Cat

# Basic housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Fake data for buburst stan work #
library(dplyr)

setwd("~/Documents/git/regionalrisk/analyses")
bb <- read.csv("output/fs_yearsitespp.csv", header=TRUE) ## to check data

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: 6 species, 30 years, 1000 lats and longs, two outcomes
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nyr = 30
nsp = 6 # random effect
nlat = 1000 
nlon = 1000 

(ntot = nyr*nsp*nlat*nlon) # 88 rows

# Build up the data frame
yr = gl(nyr, length=ntot)

sp = gl(nsp, nyr, length = ntot)

lat = gl(nlat, nsp, length = ntot)

lon = gl(nlon, nlat, length = ntot)

(d <- data.frame(yr, sp, lat, lon)) 

###### Set up differences for each level
spdiff = 0.5
yrdiff = 0.1
latdiff = 0.2
londiff = 0.1

######## SD for each treatment
spdiff.sd = 0.1
yrdiff.sd = 0.01
latdiff.sd = 0.01
londiff.sd = 0.01

mm <- model.matrix(~(sp+yr+lat+lon), data.frame(sp, yr, lat, lon)) ### ORDER HERE REALLY MATTERS!!! MAKE SURE IT LINES UP WITH "COEFF"
## Coding check below - keep for future tweaks to code/data
#coeff <- c(1, spdiff, txdiff)
#risk <- rnorm(n = length(tx), mean = mm%*%coeff, sd = 1) # should be able to do sd = mm %*% sd.coeff as well, with a different sd for each parameter.
#(fake <- data_frame(risk, tx, sp))

##### Again, now with individuals.

baseinter = 3 # baseline intercept across all individuals for DVR
spyr <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by individual

fake <- vector()

for(i in 1:nsp){ # loop over individual (random effect of model)
  
  # Give individuals different difference values, drawn from normal
  
  coeff <- c(spyr[i], 
             rnorm(1, yrdiff, yrdiff.sd),
             rnorm(1, latdiff, latdiff.sd),
             rnorm(1, londiff, londiff.sd)
  )
  
  fs <- rnorm(n = length(yr), mean = mm %*% coeff, sd = 0.1)
  
  fakex <- data.frame(fs, sp=i, yr, lat, lon)
  
  fake<-rbind(fake, fakex)
}    

summary(lm(fs ~ yr+lat+lon, data = fake)) # sanity check 

# now fix the levels to 0/1 (not 1/2) as R does
#fake$tx <- as.numeric(fake$tx)
#fake$tx[fake$tx==1] <- 0
#fake$tx[fake$tx==2] <- 1

#summary(lm(dvr ~ tx+sp, data = fake)) # double check 

#save(list=c("fake"), file = "Fake Buds.RData")
write.csv(fake, file="~/Documents/git/regionalrisk/analyses/output/fakedata_fsyearspp.csv", row.names = FALSE)

#mean(fake$risk)
#sd(fake$risk)
#hist(fake$risk)
#length(fake$sp[fake$sp==1])

### run rstanarm models to check the fake data - prepare for running stan code
mod1<-stan_glmer(fs~yr+lat+lon+(1|sp), data=fake)
plot(mod1, pars="beta")
pp_check(mod1)


