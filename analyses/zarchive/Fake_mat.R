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
ncc = 2
reps = 1
(ntot = nmat*nsp*nsite*ncc*reps)

mat = as.numeric(gl(nmat, reps, length=ntot))
sp = as.numeric(gl(nsp, reps*nmat, length = ntot))
site = as.numeric(gl(nsite, reps*nsp*nmat, length = ntot))
cc = as.numeric(gl(ncc, reps*nsite*nsp*nmat, length = ntot))
(d <- data.frame(mat, sp, site, cc))

a = 2.5
m = 0.2
spp = 0.5
st = 0.1
acc = 0.1
s = 0.1
fs = rep(0,ntot)

pb <- txtProgressBar(min = 1, max = nrow(d), style = 3)
for (i in 1:nrow(d)){
  fs[i] <- rnorm(n = length(ntot) , mean=a + m*mat[i] + spp*sp[i] + st*site[i]
                 + acc*cc[i], sd=s )
  setTxtProgressBar(pb, i)
}

fake <- data_frame(fs, mat, sp, site, cc)

#now fix the levels to 0/1 (not 1/2) as R does
fake$cc <- as.numeric(fake$cc)
fake$cc[fake$cc==1] <- 0
fake$cc[fake$cc==2] <- 1

fake$mat<-as.numeric(fake$mat)
fake$sp<-as.numeric(fake$sp)
fake$site<-as.numeric(fake$site)

summary(lm(fs ~ mat+sp+site+cc, data = fake)) # sanity check 

#write.csv(fake, file="~/Documents/git/regionalrisk/analyses/output/smfake_mat.csv", row.names = FALSE)


