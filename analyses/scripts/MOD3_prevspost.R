## Started 26 January 2018 ##
## By Cat ##

## Try to run REAL data ##
## With Stan! ##
## Rstanarm poisson data - talk to Lizzie
# FS # ~ MAT[sp/site] -- comparing before and after 1980?

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

## # FS # ~ MAT[sp/site] prep data -- would site here be PEP_ID?

#pre<-bb%>%filter(year>1950)%>%filter(year<=1983)
pre<-bb
pre$sp<-as.numeric(as.factor(pre$species))
pre$mat<-ave(pre$mat, pre$PEP_ID)
pre$site<-as.numeric(as.factor(pre$PEP_ID))
pre$fs<-ave(pre$fs, pre$site, pre$species, FUN=sum)

prex<-pre%>%dplyr::select(site, mat, sp, fs)
prex<-prex[!duplicated(prex),]

## subsetting data, preparing genus variable, removing NAs
pre.prepdata <- subset(prex, select=c("fs", "mat", "sp", "site")) 
pre.stan <- pre.prepdata[complete.cases(pre.prepdata),]


pre<-stan_glmer(fs~(mat|sp/site), data=pre.stan, family=poisson, prior=normal(0,1))
pre
plot<-plot(mat, pars="beta") 

+ scale_x_continuous(limits = c(-0.35, 0.15)) + 
  annotate("text", x = -0.3, y = 5, label = "1951-1983", fontface = "bold")
pp_check(mat)
launch_shinystan(mat)

### Moran's I ####
pre.dist<-as.matrix(dist(cbind(unique(pre.mat.stan$lat), unique(pre.mat.stan$long))))
pre.dist.inv<-1/pre.dist
diag(pre.dist.inv) <- 0
pre.dist.inv[1:5,1:5]
Moran.I(pre.mat.stan$fs, pre.dist)

#pre.dist.bin<-(pre.dist>0 & pre.dist<=0.75)
#Moran.I(pre.mat.stan$fs, pre.dist.bin)

############## Post now! ###############