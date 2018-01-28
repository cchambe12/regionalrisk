## Started 19 January 2018 ##
## By Cat ##

## Try to run REAL data ##
## With Stan! ##

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
library(tidyr)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/regionalrisk/analyses/")
#source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
#bb<-read.csv("output/fake_poisson.csv", header=TRUE)
bb<-read.csv("output/fs_matspsite.csv", header=TRUE)

## # FS/decade ~ decade + SP - prep data

bb$year<-as.numeric(bb$year)
bb$decade<-NA
bb$decade<-ifelse(bb$year>=1950 & bb$year<1960, 1, bb$decade)
bb$decade<-ifelse(bb$year>=1960 & bb$year<1970, 2, bb$decade)
bb$decade<-ifelse(bb$year>=1970 & bb$year<1980, 3, bb$decade)
bb$decade<-ifelse(bb$year>=1980 & bb$year<1990, 4, bb$decade)
bb$decade<-ifelse(bb$year>=1990 & bb$year<2000, 5, bb$decade)
bb$decade<-ifelse(bb$year>=2000 & bb$year<2010, 6, bb$decade)
bb$decade<-ifelse(bb$year>=2010 & bb$year<2020, 7, bb$decade)
bb$fs<-ave(bb$fs,bb$PEP_ID, bb$decade, FUN=sum)
bb$sp<-as.numeric(as.factor(bb$species))
bb$decade<-as.numeric(bb$decade)
bb$site<-as.numeric(as.factor(bb$PEP_ID))

bb<-bb%>%dplyr::select(fs, sp, decade, site)
bx<-bb[!duplicated(bb),]
#bb<-bb%>%rename(fs=fs.num)

## subsetting data, preparing genus variable, removing NAs
mat.prepdata <- subset(bx, select=c("fs", "sp", "decade", "site")) 
mat.stan <- mat.prepdata[complete.cases(mat.prepdata),]

#mat.stan<-mat.stan[sample(nrow(mat.stan), 5000), ]

#mat$fs = mat.stan$fs.num
fs = mat.stan$fs
sp = log(mat.stan$sp)
decade = log(mat.stan$decade)
site = log(mat.stan$site)
N = length(fs)

# making a list out of the processed data. It will be input for the model
datalist.td <- list(fs=fs,decade=decade,sp=sp,site=site,N=N)
#### Now using rstan model
dec<-stan_glmer(fs~site+decade+(1|sp), data=mat.stan, family=poisson)

### learn how to build a poisson model in rstan - normal distribution model did not work

mat.td4 = stan('scripts/poisson_decade.stan', data = datalist.td,
               iter = 2000, warmup=1500, control=list(adapt_delta=0.99)) 
betas <- as.matrix(mat.td4, pars = c("b_decade", "b_sp"))
mcmc_intervals(betas)
