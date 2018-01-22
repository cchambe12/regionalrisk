## Started 19 January 2018 ##
## By Cat ##

## Try to run REAL data ##
## With Stan! ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

dostan = TRUE

library(rstan)
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(betareg)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/regionalrisk/analyses/")
source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
bb<-read.csv("output/fake_mat.csv", header=TRUE)

## subsetting data, preparing genus variable, removing NAs
mat.prepdata <- subset(bb, select=c("fs", "mat", "sp", "site", "cc")) # removed "sp" when doing just one species
mat.stan <- mat.prepdata[complete.cases(mat.prepdata),]

x = 5
fs = mat.stan$fs
mat = mat.stan$mat
sp = mat.stan$sp
site = mat.stan$site
cc = mat.stan$cc
N = length(mat.stan$fs)


# making a list out of the processed data. It will be input for the model
datalist.td <- list(x=x,fs=fs,mat=mat,sp=sp,site=site,cc=cc,N=N)

#### Now using rstan model
mat<-stan_glm(fs~mat+sp+site+cc, data=mat.stan)
mat.td4 = stan('scripts/gp_fsmat.stan', data = datalist.td,
              iter = 2000, warmup=1500, control=list(adapt_delta=0.99)) 
betas <- as.matrix(mat.td4, pars = c("mu_mat", "mu_sp", "mu_site", "mu_cc"))
mcmc_intervals(betas)

mat.td4
plot(mat.td4, pars=c("mu_mat", "mu_sp", "mu_site", "mu_cc"))

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

