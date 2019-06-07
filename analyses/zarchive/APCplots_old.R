## Cat - 4 March 2019
# Final model - now some plots!
##### Use the same script for all three models!!!! Need to adjust model choice depending on question 
##(i.e., original model, duration of veg risk model or temp threshold model?)

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(rstan)
library(brms)
library(ggplot2)
library(egg) 
library(RColorBrewer)

setwd("~/Documents/git/regionalrisk")

#load("orig_full.Rdata")
load("dvr_full.Rdata")
#load("five_full.Rdata")

bb <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)
#bb <- read.csv("analyses/output/fs_newspace_dvr.csv", header=TRUE)
#bb <- read.csv("analyses/output/fs_newspace_five.csv", header=TRUE)

if(FALSE){
## Let's just check out the model
sort(unique(bb$species))
#summary(orig.full)
#mod_sum <- posterior_samples(orig.full)
#summary(dvr.full)
mod_sum <- posterior_samples(dvr.full)
#summary(five.full)
#mod_sum <- posterior_samples(five.full)

#str(mod_sum)

# Okay, I want to plot AESHIP's fs by distance from coast, pre and post climate change

# Let's review the model
# fs ~ nao.z + mat.z + dist.z + elev.z + space.z + cc.z + species + nao.z:species +  mat.z:species + dist.z:species + elev.z:species + space.z:species + cc.z:species + nao.z:cc.z + mat.z:cc.z + dist.z:cc.z + elev.z:cc.z + space.z:cc.z
# the intercept will be AESHIP with 0 for nao, mat, dist, elev, space, cc and 0 for all interactions ...
# So, one APC could be evaluating pre-cc AESHIP across a range of distances, holding everything else at its mean

# To plot with distance on the x axis we need to set up a vector of distances to predict: 
newdist <- seq(from=range(bb$dist.z)[1], to=range(bb$dist.z)[2], length.out=200)  

# what values for cc?
#sort(unique(bb$cc.z)) 

# Okay, now we need to loop through newdist, and while we're here let's inverse the logit and grab the percentiles we want (you can change these)
inverselogit <- function(x){ exp(x) / (1+exp(x) ) }


### Repeat for two extreme species... and combine in one loop
betpen.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
betpen.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

aeship.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
aeship.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

alnglu.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
alnglu.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

frasyl.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
frasyl.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

querob.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
querob.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

fraexc.dist.precc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
fraexc.dist.postcc <- data.frame(dist=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())


for(i in 1:length(newdist)){
  
  ## BETPEN...
  betpen.dist.precc.onedist <- (mod_sum$b_Intercept + mod_sum$b_speciesBETPEN) + (mod_sum$b_dist.z + mod_sum$`b_dist.z:speciesBETPEN`)*newdist[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  betpen.dist.postcc.onedist <-(mod_sum$b_Intercept + mod_sum$b_speciesBETPEN) + (mod_sum$b_dist.z + mod_sum$`b_dist.z:speciesBETPEN`)*newdist[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(betpen.dist.precc.onedist),
                               fs.25=quantile(betpen.dist.precc.onedist, 0.25), fs.75=quantile(betpen.dist.precc.onedist, 0.75))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(betpen.dist.postcc.onedist),
                                fs.25=quantile(betpen.dist.postcc.onedist, 0.25), fs.75=quantile(betpen.dist.postcc.onedist, 0.75))
  betpen.dist.precc <- rbind(betpen.dist.precc, precc.df.here)
  betpen.dist.postcc <- rbind(betpen.dist.postcc, postcc.df.here)
  
  ## AESHIP...
  aeship.dist.precc.onedist <- (mod_sum$b_Intercept ) + (mod_sum$b_dist.z )*newdist[i] + 
    (mod_sum$b_cc.z )*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  betpen.dist.postcc.onedist <-(mod_sum$b_Intercept) + (mod_sum$b_dist.z )*newdist[i] + 
    (mod_sum$b_cc.z )*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(aeship.dist.precc.onedist),
                               fs.25=quantile(aeship.dist.precc.onedist, 0.25), fs.75=quantile(aeship.dist.precc.onedist, 0.75))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(aeship.dist.postcc.onedist),
                                fs.25=quantile(aeship.dist.postcc.onedist, 0.25), fs.75=quantile(aeship.dist.postcc.onedist, 0.75))
  betpen.dist.precc <- rbind(aeship.dist.precc, precc.df.here)
  betpen.dist.postcc <- rbind(aeship.dist.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.dist.precc.onedist <- (mod_sum$b_Intercept + mod_sum$b_speciesFRAEXC) + (mod_sum$b_dist.z + mod_sum$`b_dist.z:speciesFRAEXC`)*newdist[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newdist[i])
  fraexc.dist.postcc.onedist <-(mod_sum$b_Intercept + mod_sum$b_speciesFRAEXC) + (mod_sum$b_dist.z + mod_sum$`b_dist.z:speciesFRAEXC`)*newdist[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_dist.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newdist[i])
  precc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(fraexc.dist.precc.onedist),
                               fs.25=quantile(fraexc.dist.precc.onedist, 0.25), fs.75=quantile(fraexc.dist.precc.onedist, 0.75))
  postcc.df.here <-  data.frame(dist=newdist[i], fs.mean=mean(fraexc.dist.postcc.onedist),
                                fs.25=quantile(fraexc.dist.postcc.onedist, 0.25), fs.75=quantile(fraexc.dist.postcc.onedist, 0.75))
  fraexc.dist.precc <- rbind(fraexc.dist.precc, precc.df.here)
  fraexc.dist.postcc <- rbind(fraexc.dist.postcc, postcc.df.here)
  
}

betpen.dist.postcc$cc <- "1"
betpen.dist.precc$cc <- "0"
betpen.dist <- rbind(betpen.dist.postcc, betpen.dist.precc)
betpen.dist$species <- "aaBETPEN"

fraexc.dist.postcc$cc <- "1"
fraexc.dist.precc$cc <- "0"
fraexc.dist <- rbind(fraexc.dist.postcc, fraexc.dist.precc)
fraexc.dist$species <- "zFRAEXC"

distxcc <- rbind(betpen.dist, fraexc.dist)

# To use any of these values we convert to log-odds scales
distxcc$fsmean_trans <- inverselogit(distxcc$fs.mean)
distxcc$fs25_trans <- inverselogit(distxcc$fs.25)
distxcc$fs75_trans <- inverselogit(distxcc$fs.75)
distxcc$dist_trans <- (distxcc$dist)*sd(bb$distkm)*2 + mean(bb$distkm)

cols <- c("#7FC97F", "#BF5B17")
distances <- ggplot(distxcc, aes(x=dist_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs25_trans, ymax=fs75_trans, alpha=cc, fill=species)) + theme_classic() +
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
  theme(legend.text.align = 0, legend.position = c(0.75, 0.85)) + ggtitle("C.")


####################### Now for MEAN SPRING TEMPERATURE ###################
newmat <- seq(from=range(bb$mat.z)[1], to=range(bb$mat.z)[2], length.out=200)  

### Repeat for each species... Now let's do the same thing but combine all into one loop
betpen.mat.precc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
betpen.mat.postcc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

fraexc.mat.precc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
fraexc.mat.postcc <- data.frame(mat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

for(i in 1:length(newmat)){
  
  ## BETPEN...
  betpen.mat.precc.onemat <- (mod_sum$b_Intercept + mod_sum$b_speciesBETPEN) + (mod_sum$b_mat.z + mod_sum$`b_mat.z:speciesBETPEN`)*newmat[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newmat[i])
  betpen.mat.postcc.onemat <-(mod_sum$b_Intercept + mod_sum$b_speciesBETPEN) + (mod_sum$b_mat.z + mod_sum$`b_mat.z:speciesBETPEN`)*newmat[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newmat[i])
  precc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(betpen.mat.precc.onemat),
                               fs.25=quantile(betpen.mat.precc.onemat, 0.25), fs.75=quantile(betpen.mat.precc.onemat, 0.75))
  postcc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(betpen.mat.postcc.onemat),
                                fs.25=quantile(betpen.mat.postcc.onemat, 0.25), fs.75=quantile(betpen.mat.postcc.onemat, 0.75))
  betpen.mat.precc <- rbind(betpen.mat.precc, precc.df.here)
  betpen.mat.postcc <- rbind(betpen.mat.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.mat.precc.onemat <- (mod_sum$b_Intercept + mod_sum$b_speciesFRAEXC) + (mod_sum$b_mat.z + mod_sum$`b_mat.z:speciesFRAEXC`)*newmat[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newmat[i])
  fraexc.mat.postcc.onemat <-(mod_sum$b_Intercept + mod_sum$b_speciesFRAEXC) + (mod_sum$b_mat.z + mod_sum$`b_mat.z:speciesFRAEXC`)*newmat[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_mat.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newmat[i])
  precc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(fraexc.mat.precc.onemat),
                               fs.25=quantile(fraexc.mat.precc.onemat, 0.25), fs.75=quantile(fraexc.mat.precc.onemat, 0.75))
  postcc.df.here <-  data.frame(mat=newmat[i], fs.mean=mean(fraexc.mat.postcc.onemat),
                                fs.25=quantile(fraexc.mat.postcc.onemat, 0.25), fs.75=quantile(fraexc.mat.postcc.onemat, 0.75))
  fraexc.mat.precc <- rbind(fraexc.mat.precc, precc.df.here)
  fraexc.mat.postcc <- rbind(fraexc.mat.postcc, postcc.df.here)
}

betpen.mat.postcc$cc <- "1"
betpen.mat.precc$cc <- "0"
betpen.mat <- rbind(betpen.mat.postcc, betpen.mat.precc)
betpen.mat$species <- "aaBETPEN"

fraexc.mat.postcc$cc <- "1"
fraexc.mat.precc$cc <- "0"
fraexc.mat <- rbind(fraexc.mat.postcc, fraexc.mat.precc)
fraexc.mat$species <- "zFRAEXC"

matxcc <- rbind(betpen.mat, fraexc.mat)

# To use any of these values we convert to log-odds scales
matxcc$fsmean_trans <- inverselogit(matxcc$fs.mean)
matxcc$fs25_trans <- inverselogit(matxcc$fs.25)
matxcc$fs75_trans <- inverselogit(matxcc$fs.75)
matxcc$mat_trans <- (matxcc$mat)*sd(bb$mst)*2 + mean(bb$mst)

meantemp <- ggplot(matxcc, aes(x=mat_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs25_trans, ymax=fs75_trans, alpha=cc, fill=species)) + theme_classic() +
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
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE) +
  theme(legend.text.align = 0, legend.position="none") + ggtitle("B.")

####################### Now for ELEVATION ###################
newelev <- seq(from=range(bb$elev.z)[1], to=range(bb$elev.z)[2], length.out=200)  

### Repeat for each species... Now let's do the same thing but combine all into one loop
betpen.elev.precc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
betpen.elev.postcc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

fraexc.elev.precc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
fraexc.elev.postcc <- data.frame(elev=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

for(i in 1:length(newelev)){
  
  ## BETPEN...
  betpen.elev.precc.oneelev <- (mod_sum$b_Intercept + mod_sum$b_speciesBETPEN) + (mod_sum$b_elev.z + mod_sum$`b_elev.z:speciesBETPEN`)*newelev[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newelev[i])
  betpen.elev.postcc.oneelev <-(mod_sum$b_Intercept + mod_sum$b_speciesBETPEN) + (mod_sum$b_elev.z + mod_sum$`b_elev.z:speciesBETPEN`)*newelev[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newelev[i])
  precc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(betpen.elev.precc.oneelev),
                               fs.25=quantile(betpen.elev.precc.oneelev, 0.25), fs.75=quantile(betpen.elev.precc.oneelev, 0.75))
  postcc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(betpen.elev.postcc.oneelev),
                                fs.25=quantile(betpen.elev.postcc.oneelev, 0.25), fs.75=quantile(betpen.elev.postcc.oneelev, 0.75))
  betpen.elev.precc <- rbind(betpen.elev.precc, precc.df.here)
  betpen.elev.postcc <- rbind(betpen.elev.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.elev.precc.oneelev <- (mod_sum$b_Intercept + mod_sum$b_speciesFRAEXC) + (mod_sum$b_elev.z + mod_sum$`b_elev.z:speciesFRAEXC`)*newelev[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newelev[i])
  fraexc.elev.postcc.oneelev <-(mod_sum$b_Intercept + mod_sum$b_speciesFRAEXC) + (mod_sum$b_elev.z + mod_sum$`b_elev.z:speciesFRAEXC`)*newelev[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_elev.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newelev[i])
  precc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(fraexc.elev.precc.oneelev),
                               fs.25=quantile(fraexc.elev.precc.oneelev, 0.25), fs.75=quantile(fraexc.elev.precc.oneelev, 0.75))
  postcc.df.here <-  data.frame(elev=newelev[i], fs.mean=mean(fraexc.elev.postcc.oneelev),
                                fs.25=quantile(fraexc.elev.postcc.oneelev, 0.25), fs.75=quantile(fraexc.elev.postcc.oneelev, 0.75))
  fraexc.elev.precc <- rbind(fraexc.elev.precc, precc.df.here)
  fraexc.elev.postcc <- rbind(fraexc.elev.postcc, postcc.df.here)
}

betpen.elev.postcc$cc <- "1"
betpen.elev.precc$cc <- "0"
betpen.elev <- rbind(betpen.elev.postcc, betpen.elev.precc)
betpen.elev$species <- "aaBETPEN"

fraexc.elev.postcc$cc <- "1"
fraexc.elev.precc$cc <- "0"
fraexc.elev <- rbind(fraexc.elev.postcc, fraexc.elev.precc)
fraexc.elev$species <- "zFRAEXC"

elevxcc <- rbind(betpen.elev, fraexc.elev)


# To use any of these values we convert to log-odds scales
elevxcc$fsmean_trans <- inverselogit(elevxcc$fs.mean)
elevxcc$fs25_trans <- inverselogit(elevxcc$fs.25)
elevxcc$fs75_trans <- inverselogit(elevxcc$fs.75)
elevxcc$elev_trans <- (elevxcc$elev)*sd(bb$mst)*2 + mean(bb$mst)

elevations <- ggplot(elevxcc, aes(x=elev_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs25_trans, ymax=fs75_trans, alpha=cc, fill=species)) + theme_classic() +
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
  ylab("Probability of False Spring") + coord_cartesian(ylim = c(0, 1), expand=c(0, 0)) + guides(fill=FALSE) +
  theme(legend.text.align = 0, legend.position = "none") + ggtitle("D.")

####################### Now for NAO ###################
newnao <- seq(from=range(bb$nao.z)[1], to=range(bb$nao.z)[2], length.out=200)  

### Repeat for each species... Now let's do the same thing but combine all into one loop
betpen.nao.precc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
betpen.nao.postcc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

fraexc.nao.precc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())
fraexc.nao.postcc <- data.frame(nao=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())


for(i in 1:length(newnao)){
  
  ## BETPEN...
  betpen.nao.precc.onenao <- (mod_sum$b_Intercept + mod_sum$b_speciesBETPEN) + (mod_sum$b_nao.z + mod_sum$`b_nao.z:speciesBETPEN`)*newnao[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newnao[i])
  betpen.nao.postcc.onenao <-(mod_sum$b_Intercept + mod_sum$b_speciesBETPEN) + (mod_sum$b_nao.z + mod_sum$`b_nao.z:speciesBETPEN`)*newnao[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesBETPEN`)*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newnao[i])
  precc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(betpen.nao.precc.onenao),
                               fs.25=quantile(betpen.nao.precc.onenao, 0.25), fs.75=quantile(betpen.nao.precc.onenao, 0.75))
  postcc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(betpen.nao.postcc.onenao),
                                fs.25=quantile(betpen.nao.postcc.onenao, 0.25), fs.75=quantile(betpen.nao.postcc.onenao, 0.75))
  betpen.nao.precc <- rbind(betpen.nao.precc, precc.df.here)
  betpen.nao.postcc <- rbind(betpen.nao.postcc, postcc.df.here)
  
  ## FRAEXC...
  fraexc.nao.precc.onenao <- (mod_sum$b_Intercept + mod_sum$b_speciesFRAEXC) + (mod_sum$b_nao.z + mod_sum$`b_nao.z:speciesFRAEXC`)*newnao[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[1] +
    mod_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[1]*newnao[i])
  fraexc.nao.postcc.onenao <-(mod_sum$b_Intercept + mod_sum$b_speciesFRAEXC) + (mod_sum$b_nao.z + mod_sum$`b_nao.z:speciesFRAEXC`)*newnao[i] + 
    (mod_sum$b_cc.z + mod_sum$`b_cc.z:speciesFRAEXC`)*sort(unique(bb$cc.z))[2] +
    mod_sum[["b_nao.z:cc.z"]]*(sort(unique(bb$cc.z))[2]*newnao[i])
  precc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(fraexc.nao.precc.onenao),
                               fs.25=quantile(fraexc.nao.precc.onenao, 0.25), fs.75=quantile(fraexc.nao.precc.onenao, 0.75))
  postcc.df.here <-  data.frame(nao=newnao[i], fs.mean=mean(fraexc.nao.postcc.onenao),
                                fs.25=quantile(fraexc.nao.postcc.onenao, 0.25), fs.75=quantile(fraexc.nao.postcc.onenao, 0.75))
  fraexc.nao.precc <- rbind(fraexc.nao.precc, precc.df.here)
  fraexc.nao.postcc <- rbind(fraexc.nao.postcc, postcc.df.here)
  
}

betpen.nao.postcc$cc <- "1"
betpen.nao.precc$cc <- "0"
betpen.nao <- rbind(betpen.nao.postcc, betpen.nao.precc)
betpen.nao$species <- "aaBETPEN"

fraexc.nao.postcc$cc <- "1"
fraexc.nao.precc$cc <- "0"
fraexc.nao <- rbind(fraexc.nao.postcc, fraexc.nao.precc)
fraexc.nao$species <- "zFRAEXC"

naoxcc <- rbind(betpen.nao, fraexc.nao)


# To use any of these values we convert to log-odds scales
naoxcc$fsmean_trans <- inverselogit(naoxcc$fs.mean)
naoxcc$fs25_trans <- inverselogit(naoxcc$fs.25)
naoxcc$fs75_trans <- inverselogit(naoxcc$fs.75)
naoxcc$nao_trans <- (naoxcc$nao)*sd(bb$mst)*2 + mean(bb$mst)


naoindex <- ggplot(naoxcc, aes(x=nao_trans, y=fsmean_trans)) + geom_line(aes(linetype=cc, alpha=cc, col=species)) +
  geom_ribbon(aes(ymin=fs25_trans, ymax=fs75_trans, alpha=cc, fill=species)) + theme_classic() +
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
  theme(legend.text.align = 0, legend.position=c(0.75, 0.85)) + 
  ggtitle("E.")



quartz()
g1 <- grid.arrange(meantemp, distances, ncol=2)
g2 <- grid.arrange(elevations, naoindex, ncol=2, widths=c(1, 1))
inters <- grid.arrange(g1, g2, heights=c(2, 1.5))
}


