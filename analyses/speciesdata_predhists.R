### 11 July 2019 - Cat
## To check out species differences, start with differences in data

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


## Set Working Directory
setwd("~/Documents/git/regionalrisk")

fs <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)
fs$spp <- as.numeric(as.factor(fs$species))
species <- unique(fs$species)

### alright now let's go through the predictors...
quartz() ## MST
par(mfrow=c(2,3))
for(i in 1:length(species)){
  hist(fs$mst[(fs$spp==i)], xlim=c(-7, 17))
}

quartz() ## Distance
par(mfrow=c(2,3))
for(i in 1:length(species)){
hist(fs$distkm[(fs$spp==i)], xlim=c(0, 720))
}

quartz() ## Elev
par(mfrow=c(2,3))
for(i in 1:length(species)){
  hist(fs$elev[(fs$spp==i)], xlim=c(0, 1900))
}

quartz() ## NAO
par(mfrow=c(2,3))
for(i in 1:length(species)){
  hist(fs$nao[(fs$spp==i)], xlim=c(-0.6, 0.7))
}


### No glaring differences... 