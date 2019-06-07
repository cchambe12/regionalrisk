## Building fake data for regional risk - fs # ~ mat + site + species
## Updated: 19 January 2018 - Cat

# Basic housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Fake data for buburst stan work #
library(dplyr)
library(rstanarm)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: 6 species, 30 years, 1000 lats and longs, two outcomes
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
nmat = 12
nsp = 6
nsite = 16
reps = 1
(ntot = nmat*nsp*nsite*reps)

mat = as.numeric(gl(nmat, reps, length=ntot))
sp = as.numeric(gl(nsp, reps*nmat, length = ntot))
site = as.numeric(gl(nsite, reps*nsp*nmat, length = ntot))
(d <- data.frame(mat, sp, site))

a = 2.5
m = 0.2
spp = 0.5
st = 0.1
s = 0.5
fs = rep(0,ntot)

sigma = vector()
for (i in 1:nrow(d)){
  sigma[i] <- rnorm(n = length(ntot) ,m*mat[i] + spp*sp[i] + st*site[i], sd=s )
}

fs<-rpois(nrow(d), sigma)

fake <- data_frame(fs, mat, sp, site)

fake$mat<-as.numeric(fake$mat)
fake$sp<-as.numeric(fake$sp)
fake$site<-as.numeric(fake$site)

summary(lm(fs ~ mat+sp+site, data = fake)) # sanity check 

#write.csv(fake, file="~/Documents/git/regionalrisk/analyses/output/fake_poisson.csv", row.names = FALSE)

fake$ma<-log(fake$mat)
fake$spp<-log(fake$sp)
fake$st<-log(fake$site)

stan_glm(fs~ma+spp+st, data=fake, family=poisson)

