## Cat - 4 March 2019
# Final model - now some plots!

## Updates by Lizzie in February 2019
# My goal was to understand plots such as APC_dist_ALNGLU&QUEROB.pdf
# I could never figure out what the add_predicted_draws command does...
# So, I thought we should just do the manipulations of the posterior ourselves
# So I show that below ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(rstan)
library(brms)
library(ggplot2)
library(dplyr)
library(egg) 
library(RColorBrewer)

setwd("~/Documents/git/regionalrisk")

load("orig_full.Rdata")

bb <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$eigen-mean(bb$eigen,na.rm=TRUE))/(2*sd(bb$eigen,na.rm=TRUE))

##############################
## Start of Lizzie's new code
##############################

## Let's just check out the model
sort(unique(bb$species))
summary(orig.full)
orig_sum <- posterior_samples(orig.full)
#str(orig_sum)

# Okay, I want to plot AESHIP's fs by distance from coast, pre and post climate change

# Let's review the model
# fs ~ nao.z + mat.z + dist.z + elev.z + space.z + cc.z + species + nao.z:species +  mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z
# the intercept will be AESHIP with 0 for nao, mat, dist, elev, space, cc and 0 for all interactions ...
# So, one APC could be evaluating pre-cc AESHIP across a range of distances, holding everything else at its mean

# To plot with distance on the x axis we need to set up a vector of distances to predict: 
newdist <- seq(from=range(bb$dist.z)[1], to=range(bb$dist.z)[2], length.out=200)  
#newdist_check <- (newdist)*sd(bb$distkm)*2 + mean(bb$distkm)

# what values for cc?
#sort(unique(bb$cc.z)) 

# Okay, now we need to loop through newdist, and while we're here let's inverse the logit and grab the percentiles we want (you can change these)
inverselogit <- function(x){ exp(x) / (1+exp(x) ) }


### Repeat for each species... Now let's do the same thing but combine all into one loop
aeship.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
aeship.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

alnglu.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
alnglu.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

betpen.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
betpen.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fagsyl.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fagsyl.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fraexc.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fraexc.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

querob.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
querob.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())


for(i in 1:length(newdist)){
  
  ## AESHIP here..
  aeship.dist.precc.onedist <- orig_sum$b_Intercept + orig_sum$b_dist.z*newdist[i] + orig_sum$b_cc.z*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  aeship.dist.postcc.onedist <-orig_sum$b_Intercept + orig_sum$b_dist.z*newdist[i] + orig_sum$b_cc.z*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(aeship.dist.precc.onedist),
                               fs.2=quantile(aeship.dist.precc.onedist, 0.2), fs.98=quantile(aeship.dist.precc.onedist, 0.98))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(aeship.dist.postcc.onedist),
                                fs.2=quantile(aeship.dist.postcc.onedist, 0.2), fs.98=quantile(aeship.dist.postcc.onedist, 0.98))
  aeship.dist.precc <- rbind(aeship.dist.precc, precc.df.here)
  aeship.dist.postcc <- rbind(aeship.dist.postcc, postcc.df.here)
  
  ## ALNGLU...
  alnglu.dist.precc.onedist <- (orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesALNGLU`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  alnglu.dist.postcc.onedist <-(orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesALNGLU`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(alnglu.dist.precc.onedist),
                               fs.2=quantile(alnglu.dist.precc.onedist, 0.2), fs.98=quantile(alnglu.dist.precc.onedist, 0.98))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(alnglu.dist.postcc.onedist),
                                fs.2=quantile(alnglu.dist.postcc.onedist, 0.2), fs.98=quantile(alnglu.dist.postcc.onedist, 0.98))
  alnglu.dist.precc <- rbind(alnglu.dist.precc, precc.df.here)
  alnglu.dist.postcc <- rbind(alnglu.dist.postcc, postcc.df.here)
  
  ## BETPEN...
  betpen.dist.precc.onedist <- (orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesBETPEN`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  betpen.dist.postcc.onedist <-(orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesBETPEN`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(betpen.dist.precc.onedist),
                               fs.2=quantile(betpen.dist.precc.onedist, 0.2), fs.98=quantile(betpen.dist.precc.onedist, 0.98))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(betpen.dist.postcc.onedist),
                                fs.2=quantile(betpen.dist.postcc.onedist, 0.2), fs.98=quantile(betpen.dist.postcc.onedist, 0.98))
  betpen.dist.precc <- rbind(betpen.dist.precc, precc.df.here)
  betpen.dist.postcc <- rbind(betpen.dist.postcc, postcc.df.here)
  
  ## FAGSYL...
  fagsyl.dist.precc.onedist <- (orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesFAGSYL`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  fagsyl.dist.postcc.onedist <-(orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesFAGSYL`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(fagsyl.dist.precc.onedist),
                               fs.2=quantile(fagsyl.dist.precc.onedist, 0.2), fs.98=quantile(fagsyl.dist.precc.onedist, 0.98))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(fagsyl.dist.postcc.onedist),
                                fs.2=quantile(fagsyl.dist.postcc.onedist, 0.2), fs.98=quantile(fagsyl.dist.postcc.onedist, 0.98))
  fagsyl.dist.precc <- rbind(fagsyl.dist.precc, precc.df.here)
  fagsyl.dist.postcc <- rbind(fagsyl.dist.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.dist.precc.onedist <- (orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesFRAEXC`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  fraexc.dist.postcc.onedist <-(orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesFRAEXC`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(fraexc.dist.precc.onedist),
                               fs.2=quantile(fraexc.dist.precc.onedist, 0.2), fs.98=quantile(fraexc.dist.precc.onedist, 0.98))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(fraexc.dist.postcc.onedist),
                                fs.2=quantile(fraexc.dist.postcc.onedist, 0.2), fs.98=quantile(fraexc.dist.postcc.onedist, 0.98))
  fraexc.dist.precc <- rbind(fraexc.dist.precc, precc.df.here)
  fraexc.dist.postcc <- rbind(fraexc.dist.postcc, postcc.df.here)
  
  ## QUEROB...
  querob.dist.precc.onedist <- (orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesQUEROB`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  querob.dist.postcc.onedist <-(orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesQUEROB`)*newdist[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(querob.dist.precc.onedist),
                               fs.2=quantile(querob.dist.precc.onedist, 0.2), fs.98=quantile(querob.dist.precc.onedist, 0.98))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(querob.dist.postcc.onedist),
                                fs.2=quantile(querob.dist.postcc.onedist, 0.2), fs.98=quantile(querob.dist.postcc.onedist, 0.98))
  querob.dist.precc <- rbind(querob.dist.precc, precc.df.here)
  querob.dist.postcc <- rbind(querob.dist.postcc, postcc.df.here)
}

aeship.dist.postcc$cc <- "1"
aeship.dist.precc$cc <- "0"
aeship.dist <- rbind(aeship.dist.postcc, aeship.dist.precc)
aeship.dist$species <- "AESHIP"

alnglu.dist.postcc$cc <- "1"
alnglu.dist.precc$cc <- "0"
alnglu.dist <- rbind(alnglu.dist.postcc, alnglu.dist.precc)
alnglu.dist$species <- "ALNGLU"

betpen.dist.postcc$cc <- "1"
betpen.dist.precc$cc <- "0"
betpen.dist <- rbind(betpen.dist.postcc, betpen.dist.precc)
betpen.dist$species <- "aaBETPEN"

fagsyl.dist.postcc$cc <- "1"
fagsyl.dist.precc$cc <- "0"
fagsyl.dist <- rbind(fagsyl.dist.postcc, fagsyl.dist.precc)
fagsyl.dist$species <- "FAGSYL"

fraexc.dist.postcc$cc <- "1"
fraexc.dist.precc$cc <- "0"
fraexc.dist <- rbind(fraexc.dist.postcc, fraexc.dist.precc)
fraexc.dist$species <- "zFRAEXC"

querob.dist.postcc$cc <- "1"
querob.dist.precc$cc <- "0"
querob.dist <- rbind(querob.dist.postcc, querob.dist.precc)
querob.dist$species <- "QUEROB"

distxcc <- rbind(aeship.dist, alnglu.dist, betpen.dist, fagsyl.dist, fraexc.dist, querob.dist)


# To use any of these values we convert to log-odds scales
distxcc$fsmean_trans <- inverselogit(distxcc$fs.mean)
distxcc$fs2_trans <- inverselogit(distxcc$fs.2)
distxcc$fs98_trans <- inverselogit(distxcc$fs.98)
distxcc$dist_trans <- (distxcc$dist)*sd(bb$distkm)*2 + mean(bb$distkm)

distxcc$fsavg_sp <- ave(distxcc$fsmean_trans, distxcc$species, distxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
distxcc$fs_sp <- ave(distxcc$fs.mean, distxcc$species, distxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
distxcc$fsavg_all <- ave(distxcc$fsmean_trans, distxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
distxcc$fs_all <- ave(distxcc$fs.mean, distxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
distapc <- dplyr::select(distxcc, cc, species, fsavg_sp, fs_sp, fsavg_all, fs_all)
distapc <- distapc[!duplicated(distapc),]

#write.csv(distapc, file="~/Documents/git/regionalrisk/analyses/output/dist_apcoutput.csv", row.names=FALSE)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
distances <- ggplot(distxcc, aes(x=dist_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                    labels=c("0"="1950-1983",
                             "1"="1984-2016")) + xlab("Distance from Coast (km)") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE, col=FALSE) +
  theme(legend.text.align = 0, legend.position = c(0.85, 0.85),
        plot.title = element_text(face="bold")) + ggtitle("(b)")


####################### Now for MEAN SPRING TEMPERATURE ###################
newmat <- seq(from=range(bb$mat.z)[1], to=range(bb$mat.z)[2], length.out=200)  

### Repeat for each species... Now let's do the same thing but combine all into one loop
aeship.mat.precc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
aeship.mat.postcc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

alnglu.mat.precc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
alnglu.mat.postcc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

betpen.mat.precc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
betpen.mat.postcc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fagsyl.mat.precc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fagsyl.mat.postcc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fraexc.mat.precc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fraexc.mat.postcc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

querob.mat.precc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
querob.mat.postcc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())


for(i in 1:length(newmat)){
  
  ## AESHIP here..
  aeship.mat.precc.onemat <- orig_sum$b_Intercept + orig_sum$b_mat.z*newmat[i] + orig_sum$b_cc.z*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newmat[i])
  aeship.mat.postcc.onemat <-orig_sum$b_Intercept + orig_sum$b_mat.z*newmat[i] + orig_sum$b_cc.z*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newmat[i])
  precc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(aeship.mat.precc.onemat),
                               fs.2=quantile(aeship.mat.precc.onemat, 0.2), fs.98=quantile(aeship.mat.precc.onemat, 0.98))
  postcc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(aeship.mat.postcc.onemat),
                                fs.2=quantile(aeship.mat.postcc.onemat, 0.2), fs.98=quantile(aeship.mat.postcc.onemat, 0.98))
  aeship.mat.precc <- rbind(aeship.mat.precc, precc.df.here)
  aeship.mat.postcc <- rbind(aeship.mat.postcc, postcc.df.here)
  
  ## ALNGLU...
  alnglu.mat.precc.onemat <- (orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesALNGLU`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newmat[i])
  alnglu.mat.postcc.onemat <-(orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesALNGLU`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newmat[i])
  precc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(alnglu.mat.precc.onemat),
                               fs.2=quantile(alnglu.mat.precc.onemat, 0.2), fs.98=quantile(alnglu.mat.precc.onemat, 0.98))
  postcc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(alnglu.mat.postcc.onemat),
                                fs.2=quantile(alnglu.mat.postcc.onemat, 0.2), fs.98=quantile(alnglu.mat.postcc.onemat, 0.98))
  alnglu.mat.precc <- rbind(alnglu.mat.precc, precc.df.here)
  alnglu.mat.postcc <- rbind(alnglu.mat.postcc, postcc.df.here)
  
  ## BETPEN...
  betpen.mat.precc.onemat <- (orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesBETPEN`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newmat[i])
  betpen.mat.postcc.onemat <-(orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesBETPEN`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newmat[i])
  precc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(betpen.mat.precc.onemat),
                               fs.2=quantile(betpen.mat.precc.onemat, 0.2), fs.98=quantile(betpen.mat.precc.onemat, 0.98))
  postcc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(betpen.mat.postcc.onemat),
                                fs.2=quantile(betpen.mat.postcc.onemat, 0.2), fs.98=quantile(betpen.mat.postcc.onemat, 0.98))
  betpen.mat.precc <- rbind(betpen.mat.precc, precc.df.here)
  betpen.mat.postcc <- rbind(betpen.mat.postcc, postcc.df.here)
  
  ## FAGSYL...
  fagsyl.mat.precc.onemat <- (orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesFAGSYL`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newmat[i])
  fagsyl.mat.postcc.onemat <-(orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesFAGSYL`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newmat[i])
  precc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(fagsyl.mat.precc.onemat),
                               fs.2=quantile(fagsyl.mat.precc.onemat, 0.2), fs.98=quantile(fagsyl.mat.precc.onemat, 0.98))
  postcc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(fagsyl.mat.postcc.onemat),
                                fs.2=quantile(fagsyl.mat.postcc.onemat, 0.2), fs.98=quantile(fagsyl.mat.postcc.onemat, 0.98))
  fagsyl.mat.precc <- rbind(fagsyl.mat.precc, precc.df.here)
  fagsyl.mat.postcc <- rbind(fagsyl.mat.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.mat.precc.onemat <- (orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesFRAEXC`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newmat[i])
  fraexc.mat.postcc.onemat <-(orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesFRAEXC`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newmat[i])
  precc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(fraexc.mat.precc.onemat),
                               fs.2=quantile(fraexc.mat.precc.onemat, 0.2), fs.98=quantile(fraexc.mat.precc.onemat, 0.98))
  postcc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(fraexc.mat.postcc.onemat),
                                fs.2=quantile(fraexc.mat.postcc.onemat, 0.2), fs.98=quantile(fraexc.mat.postcc.onemat, 0.98))
  fraexc.mat.precc <- rbind(fraexc.mat.precc, precc.df.here)
  fraexc.mat.postcc <- rbind(fraexc.mat.postcc, postcc.df.here)
  
  ## QUEROB...
  querob.mat.precc.onemat <- (orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesQUEROB`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newmat[i])
  querob.mat.postcc.onemat <-(orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesQUEROB`)*newmat[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newmat[i])
  precc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(querob.mat.precc.onemat),
                               fs.2=quantile(querob.mat.precc.onemat, 0.2), fs.98=quantile(querob.mat.precc.onemat, 0.98))
  postcc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(querob.mat.postcc.onemat),
                                fs.2=quantile(querob.mat.postcc.onemat, 0.2), fs.98=quantile(querob.mat.postcc.onemat, 0.98))
  querob.mat.precc <- rbind(querob.mat.precc, precc.df.here)
  querob.mat.postcc <- rbind(querob.mat.postcc, postcc.df.here)
}

aeship.mat.postcc$cc <- "1"
aeship.mat.precc$cc <- "0"
aeship.mat <- rbind(aeship.mat.postcc, aeship.mat.precc)
aeship.mat$species <- "AESHIP"

alnglu.mat.postcc$cc <- "1"
alnglu.mat.precc$cc <- "0"
alnglu.mat <- rbind(alnglu.mat.postcc, alnglu.mat.precc)
alnglu.mat$species <- "ALNGLU"

betpen.mat.postcc$cc <- "1"
betpen.mat.precc$cc <- "0"
betpen.mat <- rbind(betpen.mat.postcc, betpen.mat.precc)
betpen.mat$species <- "aaBETPEN"

fagsyl.mat.postcc$cc <- "1"
fagsyl.mat.precc$cc <- "0"
fagsyl.mat <- rbind(fagsyl.mat.postcc, fagsyl.mat.precc)
fagsyl.mat$species <- "FAGSYL"

fraexc.mat.postcc$cc <- "1"
fraexc.mat.precc$cc <- "0"
fraexc.mat <- rbind(fraexc.mat.postcc, fraexc.mat.precc)
fraexc.mat$species <- "zFRAEXC"

querob.mat.postcc$cc <- "1"
querob.mat.precc$cc <- "0"
querob.mat <- rbind(querob.mat.postcc, querob.mat.precc)
querob.mat$species <- "QUEROB"

matxcc <- rbind(aeship.mat, alnglu.mat, betpen.mat, fagsyl.mat, fraexc.mat, querob.mat)


# To use any of these values we convert to log-odds scales
matxcc$fsmean_trans <- inverselogit(matxcc$fs.mean)
matxcc$fs2_trans <- inverselogit(matxcc$fs.2)
matxcc$fs98_trans <- inverselogit(matxcc$fs.98)
matxcc$mat_trans <- (matxcc$mat)*sd(bb$mst)*2 + mean(bb$mst)

matxcc$fsavg_sp <- ave(matxcc$fsmean_trans, matxcc$species, matxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
matxcc$fs_sp <- ave(matxcc$fs.mean, matxcc$species, matxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
matxcc$fsavg_all <- ave(matxcc$fsmean_trans, matxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
matxcc$fs_all <- ave(matxcc$fs.mean, matxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
matapc <- dplyr::select(matxcc, cc, species, fsavg_sp, fs_sp, fsavg_all, fs_all)
matapc <- matapc[!duplicated(matapc),]
#write.csv(matapc, file="~/Documents/git/regionalrisk/analyses/output/mat_apcoutput.csv", row.names=FALSE)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
meantemp <- ggplot(matxcc, aes(x=mat_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                        labels=c("0"="1950-1983",
                                 "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) + xlab("Mean Spring Temperature (°C)") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE) +
  theme(legend.text.align = 0, plot.title = element_text(face="bold"),
        legend.position="none") + ggtitle("(a)")

####################### Now for ELEVATION ###################
newelev <- seq(from=range(bb$elev.z)[1], to=range(bb$elev.z)[2], length.out=200)  

### Repeat for each species... Now let's do the same thing but combine all into one loop
aeship.elev.precc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
aeship.elev.postcc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

alnglu.elev.precc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
alnglu.elev.postcc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

betpen.elev.precc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
betpen.elev.postcc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fagsyl.elev.precc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fagsyl.elev.postcc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fraexc.elev.precc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fraexc.elev.postcc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

querob.elev.precc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
querob.elev.postcc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())


for(i in 1:length(newelev)){
  
  ## AESHIP here..
  aeship.elev.precc.oneelev <- orig_sum$b_Intercept + orig_sum$b_elev.z*newelev[i] + orig_sum$b_cc.z*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newelev[i])
  aeship.elev.postcc.oneelev <-orig_sum$b_Intercept + orig_sum$b_elev.z*newelev[i] + orig_sum$b_cc.z*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newelev[i])
  precc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(aeship.elev.precc.oneelev),
                               fs.2=quantile(aeship.elev.precc.oneelev, 0.2), fs.98=quantile(aeship.elev.precc.oneelev, 0.98))
  postcc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(aeship.elev.postcc.oneelev),
                                fs.2=quantile(aeship.elev.postcc.oneelev, 0.2), fs.98=quantile(aeship.elev.postcc.oneelev, 0.98))
  aeship.elev.precc <- rbind(aeship.elev.precc, precc.df.here)
  aeship.elev.postcc <- rbind(aeship.elev.postcc, postcc.df.here)
  
  ## ALNGLU...
  alnglu.elev.precc.oneelev <- (orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesALNGLU`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newelev[i])
  alnglu.elev.postcc.oneelev <-(orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesALNGLU`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newelev[i])
  precc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(alnglu.elev.precc.oneelev),
                               fs.2=quantile(alnglu.elev.precc.oneelev, 0.2), fs.98=quantile(alnglu.elev.precc.oneelev, 0.98))
  postcc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(alnglu.elev.postcc.oneelev),
                                fs.2=quantile(alnglu.elev.postcc.oneelev, 0.2), fs.98=quantile(alnglu.elev.postcc.oneelev, 0.98))
  alnglu.elev.precc <- rbind(alnglu.elev.precc, precc.df.here)
  alnglu.elev.postcc <- rbind(alnglu.elev.postcc, postcc.df.here)
  
  ## BETPEN...
  betpen.elev.precc.oneelev <- (orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesBETPEN`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newelev[i])
  betpen.elev.postcc.oneelev <-(orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesBETPEN`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newelev[i])
  precc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(betpen.elev.precc.oneelev),
                               fs.2=quantile(betpen.elev.precc.oneelev, 0.2), fs.98=quantile(betpen.elev.precc.oneelev, 0.98))
  postcc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(betpen.elev.postcc.oneelev),
                                fs.2=quantile(betpen.elev.postcc.oneelev, 0.2), fs.98=quantile(betpen.elev.postcc.oneelev, 0.98))
  betpen.elev.precc <- rbind(betpen.elev.precc, precc.df.here)
  betpen.elev.postcc <- rbind(betpen.elev.postcc, postcc.df.here)
  
  ## FAGSYL...
  fagsyl.elev.precc.oneelev <- (orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesFAGSYL`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newelev[i])
  fagsyl.elev.postcc.oneelev <-(orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesFAGSYL`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newelev[i])
  precc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(fagsyl.elev.precc.oneelev),
                               fs.2=quantile(fagsyl.elev.precc.oneelev, 0.2), fs.98=quantile(fagsyl.elev.precc.oneelev, 0.98))
  postcc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(fagsyl.elev.postcc.oneelev),
                                fs.2=quantile(fagsyl.elev.postcc.oneelev, 0.2), fs.98=quantile(fagsyl.elev.postcc.oneelev, 0.98))
  fagsyl.elev.precc <- rbind(fagsyl.elev.precc, precc.df.here)
  fagsyl.elev.postcc <- rbind(fagsyl.elev.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.elev.precc.oneelev <- (orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesFRAEXC`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newelev[i])
  fraexc.elev.postcc.oneelev <-(orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesFRAEXC`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newelev[i])
  precc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(fraexc.elev.precc.oneelev),
                               fs.2=quantile(fraexc.elev.precc.oneelev, 0.2), fs.98=quantile(fraexc.elev.precc.oneelev, 0.98))
  postcc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(fraexc.elev.postcc.oneelev),
                                fs.2=quantile(fraexc.elev.postcc.oneelev, 0.2), fs.98=quantile(fraexc.elev.postcc.oneelev, 0.98))
  fraexc.elev.precc <- rbind(fraexc.elev.precc, precc.df.here)
  fraexc.elev.postcc <- rbind(fraexc.elev.postcc, postcc.df.here)
  
  ## QUEROB...
  querob.elev.precc.oneelev <- (orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesQUEROB`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newelev[i])
  querob.elev.postcc.oneelev <-(orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesQUEROB`)*newelev[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newelev[i])
  precc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(querob.elev.precc.oneelev),
                               fs.2=quantile(querob.elev.precc.oneelev, 0.2), fs.98=quantile(querob.elev.precc.oneelev, 0.98))
  postcc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(querob.elev.postcc.oneelev),
                                fs.2=quantile(querob.elev.postcc.oneelev, 0.2), fs.98=quantile(querob.elev.postcc.oneelev, 0.98))
  querob.elev.precc <- rbind(querob.elev.precc, precc.df.here)
  querob.elev.postcc <- rbind(querob.elev.postcc, postcc.df.here)
}

aeship.elev.postcc$cc <- "1"
aeship.elev.precc$cc <- "0"
aeship.elev <- rbind(aeship.elev.postcc, aeship.elev.precc)
aeship.elev$species <- "AESHIP"

alnglu.elev.postcc$cc <- "1"
alnglu.elev.precc$cc <- "0"
alnglu.elev <- rbind(alnglu.elev.postcc, alnglu.elev.precc)
alnglu.elev$species <- "ALNGLU"

betpen.elev.postcc$cc <- "1"
betpen.elev.precc$cc <- "0"
betpen.elev <- rbind(betpen.elev.postcc, betpen.elev.precc)
betpen.elev$species <- "aaBETPEN"

fagsyl.elev.postcc$cc <- "1"
fagsyl.elev.precc$cc <- "0"
fagsyl.elev <- rbind(fagsyl.elev.postcc, fagsyl.elev.precc)
fagsyl.elev$species <- "FAGSYL"

fraexc.elev.postcc$cc <- "1"
fraexc.elev.precc$cc <- "0"
fraexc.elev <- rbind(fraexc.elev.postcc, fraexc.elev.precc)
fraexc.elev$species <- "zFRAEXC"

querob.elev.postcc$cc <- "1"
querob.elev.precc$cc <- "0"
querob.elev <- rbind(querob.elev.postcc, querob.elev.precc)
querob.elev$species <- "QUEROB"

elevxcc <- rbind(aeship.elev, alnglu.elev, betpen.elev, fagsyl.elev, fraexc.elev, querob.elev)


# To use any of these values we convert to log-odds scales
elevxcc$fsmean_trans <- inverselogit(elevxcc$fs.mean)
elevxcc$fs2_trans <- inverselogit(elevxcc$fs.2)
elevxcc$fs98_trans <- inverselogit(elevxcc$fs.98)
elevxcc$elev_trans <- (elevxcc$elev)*sd(bb$mst)*2 + mean(bb$mst)

elevxcc$fsavg_sp <- ave(elevxcc$fsmean_trans, elevxcc$species, elevxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
elevxcc$fs_sp <- ave(elevxcc$fs.mean, elevxcc$species, elevxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
elevxcc$fsavg_all <- ave(elevxcc$fsmean_trans, elevxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
elevxcc$fs_all <- ave(elevxcc$fs.mean, elevxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
elevapc <- dplyr::select(elevxcc, cc, species, fsavg_sp, fs_sp, fsavg_all, fs_all)
elevapc <- elevapc[!duplicated(elevapc),]
#write.csv(elevapc, file="~/Documents/git/regionalrisk/analyses/output/elev_apcoutput.csv", row.names=FALSE)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
elevations <- ggplot(elevxcc, aes(x=elev_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                        labels=c("0"="1950-1983",
                                 "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) + xlab("Elevation (m)") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE) +
  theme(legend.text.align = 0, plot.title = element_text(face="bold"),
        legend.position = "none") + ggtitle("(c)")

####################### Now for NAO ###################
newnao <- seq(from=range(bb$nao.z)[1], to=range(bb$nao.z)[2], length.out=200)  

### Repeat for each species... Now let's do the same thing but combine all into one loop
aeship.nao.precc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
aeship.nao.postcc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

alnglu.nao.precc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
alnglu.nao.postcc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

betpen.nao.precc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
betpen.nao.postcc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fagsyl.nao.precc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fagsyl.nao.postcc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fraexc.nao.precc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fraexc.nao.postcc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

querob.nao.precc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
querob.nao.postcc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())


for(i in 1:length(newnao)){
  
  ## AESHIP here..
  aeship.nao.precc.onenao <- orig_sum$b_Intercept + orig_sum$b_nao.z*newnao[i] + orig_sum$b_cc.z*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newnao[i])
  aeship.nao.postcc.onenao <-orig_sum$b_Intercept + orig_sum$b_nao.z*newnao[i] + orig_sum$b_cc.z*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newnao[i])
  precc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(aeship.nao.precc.onenao),
                               fs.2=quantile(aeship.nao.precc.onenao, 0.2), fs.98=quantile(aeship.nao.precc.onenao, 0.98))
  postcc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(aeship.nao.postcc.onenao),
                                fs.2=quantile(aeship.nao.postcc.onenao, 0.2), fs.98=quantile(aeship.nao.postcc.onenao, 0.98))
  aeship.nao.precc <- rbind(aeship.nao.precc, precc.df.here)
  aeship.nao.postcc <- rbind(aeship.nao.postcc, postcc.df.here)
  
  ## ALNGLU...
  alnglu.nao.precc.onenao <- (orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesALNGLU`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newnao[i])
  alnglu.nao.postcc.onenao <-(orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesALNGLU`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newnao[i])
  precc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(alnglu.nao.precc.onenao),
                               fs.2=quantile(alnglu.nao.precc.onenao, 0.2), fs.98=quantile(alnglu.nao.precc.onenao, 0.98))
  postcc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(alnglu.nao.postcc.onenao),
                                fs.2=quantile(alnglu.nao.postcc.onenao, 0.2), fs.98=quantile(alnglu.nao.postcc.onenao, 0.98))
  alnglu.nao.precc <- rbind(alnglu.nao.precc, precc.df.here)
  alnglu.nao.postcc <- rbind(alnglu.nao.postcc, postcc.df.here)
  
  ## BETPEN...
  betpen.nao.precc.onenao <- (orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesBETPEN`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newnao[i])
  betpen.nao.postcc.onenao <-(orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesBETPEN`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newnao[i])
  precc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(betpen.nao.precc.onenao),
                               fs.2=quantile(betpen.nao.precc.onenao, 0.2), fs.98=quantile(betpen.nao.precc.onenao, 0.98))
  postcc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(betpen.nao.postcc.onenao),
                                fs.2=quantile(betpen.nao.postcc.onenao, 0.2), fs.98=quantile(betpen.nao.postcc.onenao, 0.98))
  betpen.nao.precc <- rbind(betpen.nao.precc, precc.df.here)
  betpen.nao.postcc <- rbind(betpen.nao.postcc, postcc.df.here)
  
  ## FAGSYL...
  fagsyl.nao.precc.onenao <- (orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesFAGSYL`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newnao[i])
  fagsyl.nao.postcc.onenao <-(orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesFAGSYL`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newnao[i])
  precc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(fagsyl.nao.precc.onenao),
                               fs.2=quantile(fagsyl.nao.precc.onenao, 0.2), fs.98=quantile(fagsyl.nao.precc.onenao, 0.98))
  postcc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(fagsyl.nao.postcc.onenao),
                                fs.2=quantile(fagsyl.nao.postcc.onenao, 0.2), fs.98=quantile(fagsyl.nao.postcc.onenao, 0.98))
  fagsyl.nao.precc <- rbind(fagsyl.nao.precc, precc.df.here)
  fagsyl.nao.postcc <- rbind(fagsyl.nao.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.nao.precc.onenao <- (orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesFRAEXC`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newnao[i])
  fraexc.nao.postcc.onenao <-(orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesFRAEXC`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newnao[i])
  precc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(fraexc.nao.precc.onenao),
                               fs.2=quantile(fraexc.nao.precc.onenao, 0.2), fs.98=quantile(fraexc.nao.precc.onenao, 0.98))
  postcc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(fraexc.nao.postcc.onenao),
                                fs.2=quantile(fraexc.nao.postcc.onenao, 0.2), fs.98=quantile(fraexc.nao.postcc.onenao, 0.98))
  fraexc.nao.precc <- rbind(fraexc.nao.precc, precc.df.here)
  fraexc.nao.postcc <- rbind(fraexc.nao.postcc, postcc.df.here)
  
  ## QUEROB...
  querob.nao.precc.onenao <- (orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesQUEROB`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[1] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newnao[i])
  querob.nao.postcc.onenao <-(orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesQUEROB`)*newnao[i] + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newnao[i])
  precc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(querob.nao.precc.onenao),
                               fs.2=quantile(querob.nao.precc.onenao, 0.2), fs.98=quantile(querob.nao.precc.onenao, 0.98))
  postcc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(querob.nao.postcc.onenao),
                                fs.2=quantile(querob.nao.postcc.onenao, 0.2), fs.98=quantile(querob.nao.postcc.onenao, 0.98))
  querob.nao.precc <- rbind(querob.nao.precc, precc.df.here)
  querob.nao.postcc <- rbind(querob.nao.postcc, postcc.df.here)
}

aeship.nao.postcc$cc <- "1"
aeship.nao.precc$cc <- "0"
aeship.nao <- rbind(aeship.nao.postcc, aeship.nao.precc)
aeship.nao$species <- "AESHIP"

alnglu.nao.postcc$cc <- "1"
alnglu.nao.precc$cc <- "0"
alnglu.nao <- rbind(alnglu.nao.postcc, alnglu.nao.precc)
alnglu.nao$species <- "ALNGLU"

betpen.nao.postcc$cc <- "1"
betpen.nao.precc$cc <- "0"
betpen.nao <- rbind(betpen.nao.postcc, betpen.nao.precc)
betpen.nao$species <- "aaBETPEN"

fagsyl.nao.postcc$cc <- "1"
fagsyl.nao.precc$cc <- "0"
fagsyl.nao <- rbind(fagsyl.nao.postcc, fagsyl.nao.precc)
fagsyl.nao$species <- "FAGSYL"

fraexc.nao.postcc$cc <- "1"
fraexc.nao.precc$cc <- "0"
fraexc.nao <- rbind(fraexc.nao.postcc, fraexc.nao.precc)
fraexc.nao$species <- "zFRAEXC"

querob.nao.postcc$cc <- "1"
querob.nao.precc$cc <- "0"
querob.nao <- rbind(querob.nao.postcc, querob.nao.precc)
querob.nao$species <- "QUEROB"

naoxcc <- rbind(aeship.nao, alnglu.nao, betpen.nao, fagsyl.nao, fraexc.nao, querob.nao)


# To use any of these values we convert to log-odds scales
naoxcc$fsmean_trans <- inverselogit(naoxcc$fs.mean)
naoxcc$fs2_trans <- inverselogit(naoxcc$fs.2)
naoxcc$fs98_trans <- inverselogit(naoxcc$fs.98)
naoxcc$nao_trans <- (naoxcc$nao)*sd(bb$mst)*2 + mean(bb$mst)

naoxcc$fsavg_sp <- ave(naoxcc$fsmean_trans, naoxcc$species, naoxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
naoxcc$fs_sp <- ave(naoxcc$fs.mean, naoxcc$species, naoxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
naoxcc$fsavg_all <- ave(naoxcc$fsmean_trans, naoxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
naoxcc$fs_all <- ave(naoxcc$fs.mean, naoxcc$cc, FUN=function(x) mean(x, na.rm=TRUE))
naoapc <- dplyr::select(naoxcc, cc, species, fsavg_sp, fs_sp, fsavg_all, fs_all)
naoapc <- naoapc[!duplicated(naoapc),]
#write.csv(naoapc, file="~/Documents/git/regionalrisk/analyses/output/nao_apcoutput.csv", row.names=FALSE)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
quartz()
naoindex <- ggplot(naoxcc, aes(x=nao_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                        labels=c("0"="1950-1983",
                                 "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) + xlab("NAO Index") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  ggtitle("(d)") +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE, linetype=FALSE, alpha=FALSE) +
  theme(legend.text.align = 0,
        legend.position = "none",
        plot.title = element_text(face="bold")) 

if(TRUE){
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

spplegend<-g_legend(naoindex)
#cclegend<-g_legend(naoindex)
}

quartz()
g1 <- grid.arrange(meantemp, distances, ncol=2)
g2 <- grid.arrange(elevations, naoindex, spplegend, ncol=3, widths=c(1, 1, 0.55))
grid.arrange(g1, g2, heights=c(2, 1.5))


png("~/Documents/git/regionalrisk/analyses/figures/APC_allpred_allspp_baseR_98.png", 
    width=8.5,
    height=6, units="in", res = 350 )
grid.arrange(g1, g2, heights=c(2, 1.5))
dev.off()



######################################################################
######################################################################
######################################################################
### FOR ONLY TWO SPECIES BELOW - NOT CURRENTLY USED IN MANUSCRIPT ####
###### Now let's compare the species extremes
#cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
ext_spp <- c("aaBETPEN", "zFRAEXC")
matxcc_sm <- matxcc[(matxcc$species %in% ext_spp),]
meantemp_sm <- ggplot(matxcc_sm, aes(x=mat_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                        labels=c("0"="1950-1983",
                                 "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) + xlab("Mean Spring Temperature (°C)") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE, linetype=FALSE, alpha=FALSE) +
  theme(legend.text.align = 0, legend.position = "none") + ggtitle("A.")

distxcc_sm <- distxcc[(distxcc$species %in% ext_spp),]
distances_sm <- ggplot(distxcc_sm, aes(x=dist_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                        labels=c("0"="1950-1983",
                                 "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) + xlab("Distance from Coast (km)") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE, col=FALSE) +
  theme(legend.text.align = 0, legend.position = c(0.80, 0.85), legend.background = element_rect(color="black")) + ggtitle("B.")

elevxcc_sm <- elevxcc[(elevxcc$species %in% ext_spp),]
elevations_sm <- ggplot(elevxcc_sm, aes(x=elev_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                        labels=c("0"="1950-1983",
                                 "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) + xlab("Elevation (m)") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE, col=FALSE) +
  theme(legend.text.align = 0, legend.position = "none") + ggtitle("C.")


naoxcc_sm <- naoxcc[(naoxcc$species %in% ext_spp),]
naoindex_sm <- ggplot(naoxcc_sm, aes(x=nao_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                        labels=c("0"="1950-1983",
                                 "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) + xlab("NAO Index") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE, linetype=FALSE, alpha=FALSE) +
  theme(legend.text.align = 0, legend.position=c(0.80, 0.85), legend.background = element_rect(color="black")) + ggtitle("D.")

#spplegend<-g_legend(naoindex_sm)

quartz()
g1 <- grid.arrange(meantemp_sm, distances_sm, ncol=2)
g2 <- grid.arrange(elevations_sm, naoindex_sm, ncol=2, widths=c(1, 1))
grid.arrange(g1, g2, heights=c(2, 2), nrow=2)





#################################################################################
############################ Let's check out CC #################################
#################################################################################
newcc <- seq(from=range(bb$cc)[1], to=range(bb$cc)[2], length.out=200)  

### Repeat for each species... Now let's do the same thing but combine all into one loop
aeship.cc.precc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
aeship.cc.postcc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

alnglu.cc.precc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
alnglu.cc.postcc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

betpen.cc.precc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
betpen.cc.postcc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fagsyl.cc.precc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fagsyl.cc.postcc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

fraexc.cc.precc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
fraexc.cc.postcc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

querob.cc.precc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())
querob.cc.postcc <- data.frame(cc=numeric(), fs.mean=numeric(), fs.2=numeric(), fs.98=numeric())

if(FALSE){
for(i in 1:length(newcc)){ #i=2
  
  ## AESHIP here..
  aeship.cc.precc.onecc <- orig_sum$b_Intercept + 
    orig_sum[["b_cc.z"]]*(sort(unique(bb$cc.z))[1]) 
  aeship.cc.postcc.onecc <-orig_sum$b_Intercept + 
    orig_sum[["b_cc.z"]]*(sort(unique(bb$cc.z))[2]) 
  precc.df.here <-  data.frame(cc=sort(unique(bb$cc.z))[1], fs.mean=mean(aeship.cc.precc.onecc),
                               fs.2=quantile(aeship.cc.precc.onecc, 0.2), fs.98=quantile(aeship.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=sort(unique(bb$cc.z))[2], fs.mean=mean(aeship.cc.postcc.onecc),
                                fs.2=quantile(aeship.cc.postcc.onecc, 0.2), fs.98=quantile(aeship.cc.postcc.onecc, 0.98))
  aeship.cc.precc <- rbind(aeship.cc.precc, precc.df.here)
  aeship.cc.postcc <- rbind(aeship.cc.postcc, postcc.df.here)
  
  ## ALNGLU...
  alnglu.cc.precc.onecc <- (orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[1] 
  alnglu.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc.z))[2] 
  precc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(alnglu.cc.precc.onecc),
                               fs.2=quantile(alnglu.cc.precc.onecc, 0.2), fs.98=quantile(alnglu.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(alnglu.cc.postcc.onecc),
                                fs.2=quantile(alnglu.cc.postcc.onecc, 0.2), fs.98=quantile(alnglu.cc.postcc.onecc, 0.98))
  alnglu.cc.precc <- rbind(alnglu.cc.precc, precc.df.here)
  alnglu.cc.postcc <- rbind(alnglu.cc.postcc, postcc.df.here)
  
  ## BETPEN...
  betpen.cc.precc.onecc <- (orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] 
  betpen.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] 
  precc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(betpen.cc.precc.onecc),
                               fs.2=quantile(betpen.cc.precc.onecc, 0.2), fs.98=quantile(betpen.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(betpen.cc.postcc.onecc),
                                fs.2=quantile(betpen.cc.postcc.onecc, 0.2), fs.98=quantile(betpen.cc.postcc.onecc, 0.98))
  betpen.cc.precc <- rbind(betpen.cc.precc, precc.df.here)
  betpen.cc.postcc <- rbind(betpen.cc.postcc, postcc.df.here)
  
  ## FAGSYL...
  fagsyl.cc.precc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[1] 
  fagsyl.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[2] 
  precc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(fagsyl.cc.precc.onecc),
                               fs.2=quantile(fagsyl.cc.precc.onecc, 0.2), fs.98=quantile(fagsyl.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(fagsyl.cc.postcc.onecc),
                                fs.2=quantile(fagsyl.cc.postcc.onecc, 0.2), fs.98=quantile(fagsyl.cc.postcc.onecc, 0.98))
  fagsyl.cc.precc <- rbind(fagsyl.cc.precc, precc.df.here)
  fagsyl.cc.postcc <- rbind(fagsyl.cc.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.cc.precc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] 
  fraexc.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[2] 
  precc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(fraexc.cc.precc.onecc),
                               fs.2=quantile(fraexc.cc.precc.onecc, 0.2), fs.98=quantile(fraexc.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(fraexc.cc.postcc.onecc),
                                fs.2=quantile(fraexc.cc.postcc.onecc, 0.2), fs.98=quantile(fraexc.cc.postcc.onecc, 0.98))
  fraexc.cc.precc <- rbind(fraexc.cc.precc, precc.df.here)
  fraexc.cc.postcc <- rbind(fraexc.cc.postcc, postcc.df.here)
  
  ## QUEROB...
  querob.cc.precc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[1] 
  querob.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc.z))[2] 
  precc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(querob.cc.precc.onecc),
                               fs.2=quantile(querob.cc.precc.onecc, 0.2), fs.98=quantile(querob.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=newcc[i], fs.mean=mean(querob.cc.postcc.onecc),
                                fs.2=quantile(querob.cc.postcc.onecc, 0.2), fs.98=quantile(querob.cc.postcc.onecc, 0.98))
  querob.cc.precc <- rbind(querob.cc.precc, precc.df.here)
  querob.cc.postcc <- rbind(querob.cc.postcc, postcc.df.here)
}


aeship.cc.postcc$cc <- "1"
aeship.cc.precc$cc <- "0"
aeship.cc <- rbind(aeship.cc.postcc, aeship.cc.precc)
aeship.cc$species <- "AESHIP"

alnglu.cc.postcc$cc <- "1"
alnglu.cc.precc$cc <- "0"
alnglu.cc <- rbind(alnglu.cc.postcc, alnglu.cc.precc)
alnglu.cc$species <- "ALNGLU"

betpen.cc.postcc$cc <- "1"
betpen.cc.precc$cc <- "0"
betpen.cc <- rbind(betpen.cc.postcc, betpen.cc.precc)
betpen.cc$species <- "aaBETPEN"

fagsyl.cc.postcc$cc <- "1"
fagsyl.cc.precc$cc <- "0"
fagsyl.cc <- rbind(fagsyl.cc.postcc, fagsyl.cc.precc)
fagsyl.cc$species <- "FAGSYL"

fraexc.cc.postcc$cc <- "1"
fraexc.cc.precc$cc <- "0"
fraexc.cc <- rbind(fraexc.cc.postcc, fraexc.cc.precc)
fraexc.cc$species <- "zFRAEXC"

querob.cc.postcc$cc <- "1"
querob.cc.precc$cc <- "0"
querob.cc <- rbind(querob.cc.postcc, querob.cc.precc)
querob.cc$species <- "QUEROB"

ccxcc <- rbind(aeship.cc, alnglu.cc, betpen.cc, fagsyl.cc, fraexc.cc, querob.cc)


# To use any of these values we convert to log-odds scales
ccxcc$fsmean_trans <- inverselogit(ccxcc$fs.mean)
ccxcc$fs2_trans <- inverselogit(ccxcc$fs.2)
ccxcc$fs98_trans <- inverselogit(ccxcc$fs.98)
#ccxcc$cc_trans <- (ccxcc$cc)*sd(bb$cc)*2 + mean(bb$cc)
ccxcc$cc_trans <- as.numeric(ccxcc$cc)

ccapcs <- ccxcc[!duplicated(ccxcc),]
#write.csv(ccapcs, file="~/Documents/git/regionalrisk/analyses/output/cc_apcoutput.csv", row.names=FALSE)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
quartz()
ccindex <- ggplot(ccxcc, aes(x=cc_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs2_trans, ymax=fs98_trans, alpha=cc, fill=species)) + theme_classic() +
  scale_linetype_manual(name="Climate Change", values=c("dashed", "solid"),
                        labels=c("0"="1950-1983",
                                 "1"="1984-2016")) +
  scale_alpha_manual(name="Climate Change", values=c(0.3, 1),
                     labels=c("0"="1950-1983",
                              "1"="1984-2016")) + xlab("Climate Change") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE, linetype=FALSE, alpha=FALSE) +
  theme(legend.text.align = 0, legend.position = "none") + ggtitle("E.")
}


##### Now try adding in everything for CC...
for(i in 1:length(newcc)){ #i=2
  
  ## AESHIP here.. #### This is a coding issue!! Need to fix precc [1] vs [2]
  aeship.cc.precc.onecc <- orig_sum$b_Intercept + orig_sum$b_dist.z + orig_sum$`b_dist.z:cc.z`*sort(unique(bb$cc))[1] +
    orig_sum[["b_cc.z"]]*(sort(unique(bb$cc))[1]) +
    orig_sum$b_elev.z + orig_sum$`b_elev.z:cc.z`*sort(unique(bb$cc))[1] +
    orig_sum$b_nao.z + orig_sum$`b_nao.z:cc.z`*sort(unique(bb$cc))[1] +
    orig_sum$b_mat.z + orig_sum$`b_mat.z:cc.z`*sort(unique(bb$cc))[1] +
    orig_sum$b_space.z + orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[1] 
  aeship.cc.postcc.onecc <-orig_sum$b_Intercept + orig_sum$b_dist.z + orig_sum$`b_dist.z:cc.z`*sort(unique(bb$cc))[2] +
    orig_sum[["b_cc.z"]]*(sort(unique(bb$cc))[2]) +
    orig_sum$b_elev.z + orig_sum$`b_elev.z:cc.z`*sort(unique(bb$cc))[2] +
    orig_sum$b_nao.z[i] + orig_sum$`b_nao.z:cc.z`*sort(unique(bb$cc))[2] +
    orig_sum$b_mat.z[i] + orig_sum$`b_mat.z:cc.z`*sort(unique(bb$cc))[2] +
    orig_sum$b_space.z + orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[2] 
  precc.df.here <-  data.frame(cc=sort(unique(bb$cc))[1], fs.mean=mean(aeship.cc.precc.onecc),
                               fs.02=quantile(aeship.cc.precc.onecc, 0.02), fs.98=quantile(aeship.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=sort(unique(bb$cc))[2], fs.mean=mean(aeship.cc.postcc.onecc),
                                fs.02=quantile(aeship.cc.postcc.onecc, 0.02), fs.98=quantile(aeship.cc.postcc.onecc, 0.98))
  aeship.cc.precc <- rbind(aeship.cc.precc, precc.df.here)
  aeship.cc.postcc <- rbind(aeship.cc.postcc, postcc.df.here)
  
  ## ALNGLU...
  alnglu.cc.precc.onecc <- (orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesALNGLU`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesALNGLU`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesALNGLU`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesALNGLU`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesALNGLU`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[1] 
  alnglu.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesALNGLU) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesALNGLU`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesALNGLU`)*sort(unique(bb$cc))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesALNGLU`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesALNGLU`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesALNGLU`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[2])+
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesALNGLU`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[2] 
  precc.df.here <-  data.frame(cc=0, fs.mean=mean(alnglu.cc.precc.onecc),
                               fs.02=quantile(alnglu.cc.precc.onecc, 0.02), fs.98=quantile(alnglu.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=1, fs.mean=mean(alnglu.cc.postcc.onecc),
                                fs.02=quantile(alnglu.cc.postcc.onecc, 0.02), fs.98=quantile(alnglu.cc.postcc.onecc, 0.98))
  alnglu.cc.precc <- rbind(alnglu.cc.precc, precc.df.here)
  alnglu.cc.postcc <- rbind(alnglu.cc.postcc, postcc.df.here)
  
  ## BETPEN...
  betpen.cc.precc.onecc <- (orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesBETPEN`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesBETPEN`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesBETPEN`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesBETPEN`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesBETPEN`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[1] 
  betpen.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesBETPEN) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesBETPEN`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesBETPEN`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesBETPEN`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesBETPEN`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesBETPEN`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[2] 
  precc.df.here <-  data.frame(cc=0, fs.mean=mean(betpen.cc.precc.onecc),
                               fs.02=quantile(betpen.cc.precc.onecc, 0.02), fs.98=quantile(betpen.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=1, fs.mean=mean(betpen.cc.postcc.onecc),
                                fs.02=quantile(betpen.cc.postcc.onecc, 0.02), fs.98=quantile(betpen.cc.postcc.onecc, 0.98))
  betpen.cc.precc <- rbind(betpen.cc.precc, precc.df.here)
  betpen.cc.postcc <- rbind(betpen.cc.postcc, postcc.df.here)
  
  ## FAGSYL...
  fagsyl.cc.precc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesFAGSYL`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesFAGSYL`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesFAGSYL`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesFAGSYL`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesFAGSYL`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[1] 
  fagsyl.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesFAGSYL) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesFAGSYL`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc.z))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesFAGSYL`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesFAGSYL`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesFAGSYL`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesFAGSYL`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[2] 
  precc.df.here <-  data.frame(cc=0, fs.mean=mean(fagsyl.cc.precc.onecc),
                               fs.02=quantile(fagsyl.cc.precc.onecc, 0.02), fs.98=quantile(fagsyl.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=1, fs.mean=mean(fagsyl.cc.postcc.onecc),
                                fs.02=quantile(fagsyl.cc.postcc.onecc, 0.02), fs.98=quantile(fagsyl.cc.postcc.onecc, 0.98))
  fagsyl.cc.precc <- rbind(fagsyl.cc.precc, precc.df.here)
  fagsyl.cc.postcc <- rbind(fagsyl.cc.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.cc.precc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesFRAEXC`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesFRAEXC`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesFRAEXC`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesFRAEXC`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesFRAEXC`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[1] 
  fraexc.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesFRAEXC) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesFRAEXC`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesFAGSYL`)*sort(unique(bb$cc))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesFRAEXC`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesFRAEXC`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesFRAEXC`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesFRAEXC`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[2] 
  precc.df.here <-  data.frame(cc=0, fs.mean=mean(fraexc.cc.precc.onecc),
                               fs.02=quantile(fraexc.cc.precc.onecc, 0.02), fs.98=quantile(fraexc.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=1, fs.mean=mean(fraexc.cc.postcc.onecc),
                                fs.02=quantile(fraexc.cc.postcc.onecc, 0.02), fs.98=quantile(fraexc.cc.postcc.onecc, 0.98))
  fraexc.cc.precc <- rbind(fraexc.cc.precc, precc.df.here)
  fraexc.cc.postcc <- rbind(fraexc.cc.postcc, postcc.df.here)
  
  ## QUEROB...
  querob.cc.precc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesQUEROB`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc))[1] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesQUEROB`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesQUEROB`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesQUEROB`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[1]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesQUEROB`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[1] 
  querob.cc.postcc.onecc <-(orig_sum$b_Intercept + orig_sum$b_speciesQUEROB) + (orig_sum$b_dist.z + orig_sum$`b_dist.z:speciesQUEROB`) + 
    (orig_sum$b_cc.z + orig_sum$`b_cc.z:speciesQUEROB`)*sort(unique(bb$cc))[2] +
    orig_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_elev.z + orig_sum$`b_elev.z:speciesQUEROB`) + 
    orig_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_mat.z + orig_sum$`b_mat.z:speciesQUEROB`) + 
    orig_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_nao.z + orig_sum$`b_nao.z:speciesQUEROB`) + 
    orig_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc))[2]) +
    (orig_sum$b_space.z + orig_sum$`b_space.z:speciesQUEROB`) +
    orig_sum$`b_space.z:cc.z`*sort(unique(bb$cc))[2] 
  precc.df.here <-  data.frame(cc=0, fs.mean=mean(querob.cc.precc.onecc),
                               fs.02=quantile(querob.cc.precc.onecc, 0.02), fs.98=quantile(querob.cc.precc.onecc, 0.98))
  postcc.df.here <-  data.frame(cc=1, fs.mean=mean(querob.cc.postcc.onecc),
                                fs.02=quantile(querob.cc.postcc.onecc, 0.02), fs.98=quantile(querob.cc.postcc.onecc, 0.98))
  querob.cc.precc <- rbind(querob.cc.precc, precc.df.here)
  querob.cc.postcc <- rbind(querob.cc.postcc, postcc.df.here)
}


#aeship.cc.postcc$cc <- "1"
#aeship.cc.precc$cc <- "0"
aeship.cc <- rbind(aeship.cc.postcc, aeship.cc.precc)
aeship.cc$species <- "AESHIP"

#alnglu.cc.postcc$cc <- "1"
#alnglu.cc.precc$cc <- "0"
alnglu.cc <- rbind(alnglu.cc.postcc, alnglu.cc.precc)
alnglu.cc$species <- "ALNGLU"

#betpen.cc.postcc$cc <- "1"
#betpen.cc.precc$cc <- "0"
betpen.cc <- rbind(betpen.cc.postcc, betpen.cc.precc)
betpen.cc$species <- "aaBETPEN"

#fagsyl.cc.postcc$cc <- "1"
#fagsyl.cc.precc$cc <- "0"
fagsyl.cc <- rbind(fagsyl.cc.postcc, fagsyl.cc.precc)
fagsyl.cc$species <- "FAGSYL"

#fraexc.cc.postcc$cc <- "1"
#fraexc.cc.precc$cc <- "0"
fraexc.cc <- rbind(fraexc.cc.postcc, fraexc.cc.precc)
fraexc.cc$species <- "zFRAEXC"

#querob.cc.postcc$cc <- "1"
#querob.cc.precc$cc <- "0"
querob.cc <- rbind(querob.cc.postcc, querob.cc.precc)
querob.cc$species <- "QUEROB"

ccxcc <- rbind(aeship.cc, alnglu.cc, betpen.cc, fagsyl.cc, fraexc.cc, querob.cc)


# To use any of these values we convert to log-odds scales
ccxcc$fsmean_trans <- inverselogit(ccxcc$fs.mean)
ccxcc$fs02_trans <- inverselogit(ccxcc$fs.02)
ccxcc$fs98_trans <- inverselogit(ccxcc$fs.98)
#ccxcc$cc_trans <- (ccxcc$cc)*sd(bb$cc)*2 + mean(bb$cc)
#ccxcc$cc_trans <- as.numeric(ccxcc$cc)

ccapcs <- ccxcc[!duplicated(ccxcc),]

sppbycc <- data.frame(cc=rep(c(0,1), times=6), species=rep(c("aaBETPEN", "AESHIP", "ALNGLU", "FAGSYL",
                                                         "zFRAEXC", "QUEROB"), each=2),
                      sppbycc=c(-0.83, -0.41, -0.88, -0.53, -0.82, -0.41, -1.23, -1.2, -2.31, -2.48, -1.91, -1.97))

ccapcs <- full_join(ccapcs, sppbycc)

write.csv(ccapcs, file="~/Documents/git/regionalrisk/analyses/output/ccapcs_combined.csv", row.names=FALSE)

ccapcs$fsmeanperspp <- ave(ccapcs$fsmean_trans, ccapcs$species, ccapcs$cc)
ccapcs$fs02perspp <- ave(ccapcs$fs02_trans, ccapcs$species, ccapcs$cc)
ccapcs$fs98perspp <- ave(ccapcs$fs98_trans, ccapcs$species, ccapcs$cc)

checkcc <- subset(ccapcs, select=c("cc", "species", "fsmeanperspp", "fs02perspp", "fs98perspp"))

checkcc <- checkcc[!duplicated(checkcc),]

aeshipmean <- mean(ccapcs$fsmean_trans[ccapcs$species=="AESHIP"]) ## .286 vs 0.33
alnglumean <- mean(ccapcs$fsmean_trans[ccapcs$species=="ALNGLU"]) ## .345 vs 0.352
betpenmean <- mean(ccapcs$fsmean_trans[ccapcs$species=="aaBETPEN"]) ## .331 vs 0.353
fagsylmean <- mean(ccapcs$fsmean_trans[ccapcs$species=="FAGSYL"]) ## .210 vs 0.228
fraexcmean <- mean(ccapcs$fsmean_trans[ccapcs$species=="zFRAEXC"]) ## .0989 vs 0.083
querobmean <- mean(ccapcs$fsmean_trans[ccapcs$species=="QUEROB"]) ## .120 vs 0.126


mean(ccapcs$fs.mean[ccapcs$species=="AESHIP"]) ## -0.802 (all: 0.08) vs -0.88 (before) vs -0.53 (0.35)
mean(ccapcs$fs.mean[ccapcs$species=="ALNGLU"]) ## -0.544 (all: 0.286) vs -0.83 (before) vs -0.42 (0.4)
mean(ccapcs$fs.mean[ccapcs$species=="aaBETPEN"]) ## -0.587 (all: 0.223) vs -0.81 (before) vs -0.41 (0.41)
mean(ccapcs$fs.mean[ccapcs$species=="FAGSYL"]) ## -1.202 (all: 0.03) vs -1.23 (before) vs -1.2 (0.03)
mean(ccapcs$fs.mean[ccapcs$species=="zFRAEXC"]) ## -2.185 (all: ) vs -2.31 (before) vs -2.48 (-0.17)
mean(ccapcs$fs.mean[ccapcs$species=="QUEROB"]) ## -1.939 (all) vs -1.91 (before) vs -1.97 (-0.06)


library(ggeffects)
ccsp<- ggpredict(orig.full, terms = c("cc.z", "species"), ci.lvl=0.98) 
nocc <- ccsp[1,1]
ccsp$cc <- ifelse(ccsp$x==nocc, 0, 1)

ccsp$x <- NULL
names(ccsp) <- c("risk", "2%", "98%", "species", "cc")


library(xtable)
print(xtable(ccsp, digits = 4),
      format.args = list(big.mark = " ", decimal.mark = "."))
