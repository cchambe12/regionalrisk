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
bb$decade<-ifelse(bb$year>=1950 & bb$year<1960, "50s", bb$decade)
bb$decade<-ifelse(bb$year>=1960 & bb$year<1970, "60s", bb$decade)
bb$decade<-ifelse(bb$year>=1970 & bb$year<1980, "70s", bb$decade)
bb$decade<-ifelse(bb$year>=1980 & bb$year<1990, "80s", bb$decade)
bb$decade<-ifelse(bb$year>=1990 & bb$year<2000, "90s", bb$decade)
bb$decade<-ifelse(bb$year>=2000 & bb$year<2010, "00s", bb$decade)
bb<-bb[!is.na(bb$decade),]
bb$fs<-ave(bb$fs,bb$PEP_ID, bb$decade,bb$species, FUN=sum)
bb$mat<-ave(bb$mat, bb$PEP_ID, bb$decade)
bb$sp<-as.numeric(as.factor(bb$species))
#bb$decade<-as.character(bb$decade)
bb$lat<-as.numeric(bb$lat)
bb$lon<-as.numeric(bb$long)

bb<-bb%>%dplyr::select(fs, sp, decade, mat, lat, lon)
bx<-bb[!duplicated(bb),]
#bb<-bb%>%rename(fs=fs.num)

## subsetting data, preparing genus variable, removing NAs
mat.prepdata <- subset(bx, select=c("fs", "sp", "decade", "mat", "lat", "lon")) 
mat.stan <- mat.prepdata[complete.cases(mat.prepdata),]

#mat.stan<-mat.stan[sample(nrow(mat.stan), 5000), ]

#mat$fs = mat.stan$fs.num
fs = mat.stan$fs
sp = log(mat.stan$sp)
decade = log(mat.stan$decade)
mat = log(mat.stan$mat)
N = length(fs)

# making a list out of the processed data. It will be input for the model
datalist.td <- list(fs=fs,decade=decade,sp=sp,mat=mat,N=N)
#### Now using rstan model
dec<-stan_glm(fs~mat*decade+sp, data=mat.stan, family=poisson)
dec.site<-stan_glm(fs~mat+decade+sp+lat+lon, data=mat.stan, family=poisson)

### learn how to build a poisson model in rstan - normal distribution model did not work

mat.td4 = stan('scripts/poisson_decade.stan', data = datalist.td,
               iter = 2000, warmup=1500, control=list(adapt_delta=0.99)) 
betas <- as.matrix(mat.td4, pars = c("b_decade", "b_sp"))
mcmc_intervals(betas)



############ Plot Decades... ##################
df<-mat.stan
df$fs.num<-ave(df$fs, df$decade)
df$fs.sd<-ave(df$fs, df$decade, FUN=sd)
dx<-df%>%dplyr::select(fs.num, fs.sd, decade)
dx<-dx[!duplicated(dx),]

diff<-ggplot(dx, aes(x=as.factor(decade), y=fs.num)) + geom_point() + 
  geom_linerange(aes(ymin=fs.num-fs.sd, ymax=fs.num+fs.sd), alpha=0.3) + 
  ylab(expression("Number of False Springs per Decade")) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_x_discrete(limits=c("50s", "60s", "70s", "80s", "90s", "00s")) + xlab("Decade")
plot(diff)


dm<-df%>%dplyr::select(fs, mat, decade )
dm<-dm[!duplicated(dm),]
dm$mat<-as.numeric(dm$mat)
mat.spp<-ggplot(dm, aes(mat, fs)) + xlab("Mean Annual Temperature") +
  ylab("Number of False Springs")  + geom_smooth(aes(col=as.factor(decade)),method="lm", se=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key=element_blank())
plot(mat.spp)

#################### Site stuff... #################

dl<-df%>%dplyr::select(fs, lat, decade )
dl<-dl[!duplicated(dl),]
dl$lat<-as.numeric(dl$lat)
lat.spp<-ggplot(dl, aes(lat, fs)) + xlab("Latitude") +
  ylab("Number of False Springs")  + geom_smooth(aes(col=as.factor(decade)),method="lm", se=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key=element_blank())
plot(lat.spp)

dlon<-df%>%dplyr::select(fs, lon, decade )
dlon<-dlon[!duplicated(dlon),]
dlon$lon<-as.numeric(dlon$lon)
lon.spp<-ggplot(dlon, aes(lon, fs)) + xlab("Longitude") +
  ylab("Number of False Springs")  + geom_smooth(aes(col=as.factor(decade)),method="lm", se=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key=element_blank())
plot(lon.spp)





