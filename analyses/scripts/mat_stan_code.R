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
bb<-read.csv("output/fake_poisson.csv", header=TRUE)
bb<-read.csv("output/fs_matspsite.csv", header=TRUE)
bb<-read.csv("output/mat_site.csv", header=TRUE)

## # yrs FS ~ MAT + SP + SITE prep data

#bb$fs<-ifelse(bb$fs.count>=1, 1, 0)
bb$cc<-NA
bb$cc<-ifelse(bb$year>1950 & bb$year<1983, 0, 1)
bb$site<-as.numeric(bb$site)
bb$fs<-as.numeric(bb$fs)
bb$fs<-ave(bb$fs, bb$PEP_ID, FUN=sum)
bb$sp<-as.numeric(as.factor(bb$species))
bb$mat<-as.numeric(bb$mat)
#bb$site<-as.numeric(as.factor(bb$PEP_ID))
bb$cc<-as.numeric(bb$cc)
#bb$lat<-as.numeric(bb$lat)
#bb$long<-as.numeric(bb$long)

bb<-bb%>%dplyr::select(site, mat, sp, fs, cc)
bx<-bb[!duplicated(bb),]
#bb<-bb%>%rename(fs=fs.num)

## subsetting data, preparing genus variable, removing NAs
mat.prepdata <- subset(bx, select=c("fs", "mat", "sp", "site", "cc")) 
mat.stan <- mat.prepdata[complete.cases(mat.prepdata),]

mat.prepdata <-subset(bb, select=c("fs", "mat", "sp", "site"))
mat.stan <- mat.prepdata[complete.cases(mat.prepdata),]

#mat.stan<-mat.stan[sample(nrow(mat.stan), 5000), ]

#mat$fs = mat.stan$fs.num
fs = mat.stan$fs
mat = log(mat.stan$mat+10)
sp = log(mat.stan$sp+10)
#site = log(mat.stan$site)
lat = log(mat.stan$lat+10)
lon = log(mat.stan$long+10)
#cc = mat.stan$cc
N = length(fs)


# making a list out of the processed data. It will be input for the model
datalist.td <- list(fs=fs,mat=mat,sp=sp,lat=lat, lon=lon,N=N)
datalist.td <- list(fs=fs,mat=mat,sp=sp,site=site,N=N)
#### Now using rstan model
mat<-stan_glm(fs~mat+sp+site, data=mat.stan, family=poisson)
mat.long<-stan_glm(fs~mat*sp*lat*long, data=mat.stan, family=poisson)
mat.glmer<-stan_glmer(fs~mat+(1|sp/site), data=mat.stan, family=poisson)
mat.gp<-stan_glm(fs~mat+sp+lat*long, data=mat.stan, family=poisson, prior=normal(0,1))
### Yay this works!!! I should now make a poisson rstan model!
beta.cc<-stan_glmer(fs~mat+cc+site+(1|sp), data=mat.stan, family=binomial)

cc<-stan_glm(fs~mat*cc+sp, data=mat.stan, family=poisson)
cc.glmer<-stan_glmer(fs~mat*cc+(1|sp), data=mat.stan, family=poisson)

### learn how to build a poisson model in rstan - normal distribution model did not work

mat.td4 = stan('scripts/space_fspoisson_nointer.stan', data = datalist.td,
              iter = 2000, warmup=1500, control=list(adapt_delta=0.99)) 
betas <- as.matrix(mat.td4, pars = c("b_mat", "b_sp", "b_lat", "b_lon"))
mcmc_intervals(betas)

mat.td4

##############################
###### real data rstanarm first

fit1<-stan_betareg(perc~tx+sp+tx:sp, data=pp.stan, link="logit", link.phi="log")
fit1
plot(fit1, pars=c("beta"))
pp_check(fit1)
prior_summary(fit1)

### Another posterior predictive check
yrep <- posterior_predict(fit1)
all.equal(ncol(yrep), nobs(fit1)) # TRUE
nd <- data.frame(perc = mean(pp.stan$perc), tx, sp)
ytilde <- posterior_predict(fit1, newdata = nd)
all.equal(ncol(ytilde), nrow(nd)) # TRUE


## FS(Y/N) ~ year + (1|sp) + site prep data
bb<-read.csv("output/fs_matspsite.csv", header=TRUE)
#bb$fs<-ifelse(bb$fs.count>=1, 1, 0)
bb$fs<-as.numeric(bb$fs)
bb$sp<-as.numeric(as.factor(bb$species))
bb$year<-as.numeric(bb$year)
#bb$mat<-ave(bb$mat, bb$lat.long)
#bb$site<-as.numeric(as.factor(bb$PEP_ID))
bb$lat<-as.numeric(bb$lat)
bb$lon<-as.numeric(bb$long)

bb<-bb%>%dplyr::select(year, lat, lon, sp, fs)
bx<-bb[!duplicated(bb),]
#bb<-bb%>%rename(fs=fs.num)

## subsetting data, preparing genus variable, removing NAs
mat.prepdata <- subset(bx, select=c("fs", "year", "sp", "lat", "lon")) 
mat.stan <- mat.prepdata[complete.cases(mat.prepdata),]
mat.stan$site<-round(mat.stan$site, digits=2)

#mat.stan<-mat.stan[sample(nrow(mat.stan), 5000), ]

#mat$fs = mat.stan$fs.num
fs = mat.stan$fs
mat = log(mat.stan$mat+10)
sp = log(mat.stan$sp+10)
#site = log(mat.stan$site)
lat = log(mat.stan$lat+10)
lon = log(mat.stan$long+10)
#cc = mat.stan$cc
N = length(fs)


# making a list out of the processed data. It will be input for the model
datalist.td <- list(fs=fs,mat=mat,sp=sp,lat=lat, lon=lon,N=N)
#### Now using rstan model
time<-stan_glm(fs~year+sp+lat+lon, data=mat.stan, family=binomial)






