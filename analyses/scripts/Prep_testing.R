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

## Start running the models
d$PEP_ID<-as.numeric(as.factor(d$PEP_ID))
d$year<-as.numeric(d$year)
#d$species<-as.numeric(as.factor(d$species))
d$fs<-as.numeric(d$fs)

## test model
df<-d[sample(nrow(d), 5000), ]
##

## Hmmm.. how to subset down to one decade vs another?
d$decade<-substr(d$year, 3, 3)
years<-c(7, 0)
dd<-d%>% filter(decade%in%years)
dd$fs.num<-ave(dd$fs, dd$decade, dd$PEP_ID, dd$species, FUN=count)
fit<-stan_glm(fs.num~decade + species, data=dd)

mod<-stan_glm(fs~year+species+lat*long, data=df, family = gaussian(link = 'log'))

### Should try subsetting by species to create maps next!
