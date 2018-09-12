## Started 26 January 2018 ##
## By Cat ##

## Try to run REAL data ##
## With Stan! ##
## Rstanarm poisson data - talk to Lizzie
# FS (Y/N) ~ year + (1|sp) + lat + long -- comparing before and after 1980?

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#dostan = TRUE

library(rstan)
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(dplyr)
library(ape)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/regionalrisk/analyses/")
#source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
#bb<-read.csv("output/smfake_mat.csv", header=TRUE)
bb<-read.csv("output/fs_matspsite.csv", header=TRUE)

## # FS (Y/N) ~ YEAR + (1|SP) + LAT + LONG prep data

pre<-bb%>%filter(year>1950)%>%filter(year<=1983)

pre$sp<-as.numeric(as.factor(pre$species))
pre$year<-as.numeric(pre$year)
pre$lat<-as.numeric(pre$lat)
pre$long<-as.numeric(pre$long)

prex<-pre%>%dplyr::select(lat, long, year, sp, fs)
prex<-prex[!duplicated(prex),]

## subsetting data, preparing genus variable, removing NAs
pre.prepdata <- subset(prex, select=c("fs", "year", "sp", "lat", "long")) 
pre.stan <- pre.prepdata[complete.cases(pre.prepdata),]


pre<-stan_glmer(fs~year+(1|sp)+lat+long, data=pre.stan, family=binomial, prior=normal(0,1))
pre
plot<-plot(mat, pars="beta") 

+ scale_x_continuous(limits = c(-0.35, 0.15)) + 
  annotate("text", x = -0.3, y = 5, label = "1951-1983", fontface = "bold")
pp_check(mat)
launch_shinystan(mat)

### Moran's I ####
pre.dist<-as.matrix(dist(cbind(pre.mat.stan$lat, pre.mat.stan$long)))
#pre.dist.inv<-1/pre.dist
#diag(pre.dist.inv) <- 0
#pre.dist.inv[1:5,1:5]
#Moran.I(pre.mat.stan$fs, pre.dist)

pre.dist.bin<-(pre.dist>0 & pre.dist<=0.75)
Moran.I(pre.mat.stan$fs, pre.dist.bin)

############## Post now! ###############