### 15 January 2018 - Cat
### Starting Analysis - looking at different questions
## 1) FS (y/n) ~ year + (1|species) + site
## 2) FS # ~ species + site + MAT
### Working on a new approach to first model

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(rstan)
library(bayesplot)
library(shinystan)

### Load data
setwd("~/Documents/git/regionalrisk/analyses/output")
d<-read.csv("fs_yearsitespp.csv", header=TRUE)
#dx<-read.csv("mat_compressed.csv", header=TRUE)
dx<-read.csv("mat_spring.csv", header=TRUE)

## Start running the models
#d$PEP_ID<-as.numeric(as.factor(d$PEP_ID))
d$year<-as.numeric(d$year)
#d$species<-as.numeric(as.factor(d$species))
d$fs<-as.numeric(d$fs)

## test model
df<-d[sample(nrow(d), 5000), ]
mod<-stan_glm(fs~year+species+lat*long, data=df, family = gaussian(link = 'log'))
##

## Hmmm.. how to subset down to one decade vs another?
d$decade<-substr(d$year, 3, 3)
#years<-c(7, 0)
#dd<-d%>% filter(decade%in%years)
dd<-d
#dd$fs.num<-ave(dd$fs, dd$decade, dd$PEP_ID, dd$species, FUN=sum)
dd<-dd%>%filter(year>1950)
dd$cc<-ifelse(dd$year<=1983, 0, 1)
dd$fs.cc<-ave(dd$fs.count, dd$cc, dd$PEP_ID, dd$species, FUN=sum)
dd$fs.dec<-ave(dd$fs.count, dd$decade, dd$PEP_ID, dd$species, FUN=sum)
ddf<-dd[sample(nrow(dd), 30000), ]

fit<-stan_glmer(fs.num~decade + (1|species) + lat*long, data=ddf, family=gaussian)
fit1<-stan_glm(fs.count~species + lat*long, data=ddf, family=gaussian)

## Both sort of interesting...
# should have a random effect on species
fit2<-stan_glm(fs.cc~species+cc+lat*long, data=ddf, family=gaussian)
## should add 1950 back in and then order decades based of 1950 and have a random effect on species
fit3<-stan_glm(fs.dec~species+decade+lat*long, data=ddf, family=gaussian)

### Do the species vary by year? Are they adapting? 

launch_shinystan(fit3)
### Should try subsetting by species to create maps next!


mat<-full_join(dx, d)
mat<-mat[!duplicated(mat),]
mat<-mat[!is.na(mat$PEP_ID),]
#mat$cc<-ifelse(mat$year>=1984, 1, 0)
#df<-mat[sample(nrow(mat), 50000), ]

write.csv(mat, file="~/Documents/git/regionalrisk/analyses/output/fs_matspring.csv", row.names = FALSE)

## 2) FS # ~ species + site + MAT
mm<-stan_glm(fs.count~species + mat, data=df, family=gaussian)

mm2<-stan_glm(fs.count~mat + lat + long + species + cc, data=df, family=gaussian)
mm2
plot(mm2, pars="beta")


mod1<-stan_glm(fs~mat+sp+site+cc, data=fake, family=gaussian)
plot(mod1, pars="beta")
pp_check(mod1)
