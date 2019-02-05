## Cat - 5 November 2018
# Final model - now some plots!

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(rstan)
library(brms)
library(ggplot2)
library(egg)
library(RColorBrewer)
library(tidybayes)
library(broom)
library(dplyr)
library(ggeffects)

setwd("~/Documents/git/regionalrisk/")

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

bb<-bb[sample(nrow(bb), 10000), ]

#species <- data.frame(species = c("BETPEN", "FRAEXC"))

#goo <- marginal_effects(orig.full, probs = c(0.25, 0.75), method = "fitted",
 #                       effects = c("elev.z", "elev.z:cc.z"), conditions = species) 



me.elev <- bb %>%  add_predicted_draws(orig.full, method = "predict") %>%
  filter(species==c("BETPEN", "FRAEXC"))

#check<-check[sample(nrow(check), 10000), ]


quartz()
ggplot(me.elev, aes(x=elev, y=.prediction, col=species, linetype=as.factor(cc))) + stat_smooth(method="lm", span=0.5, se=TRUE, 
                                                                                        aes(fill=species, linetype=as.factor(cc))) +
  theme_classic() +
  scale_colour_manual(name="Species", values=c("#7FC97F","#BF5B17"),
                      labels=c("BETPEN"=expression(paste(italic("Betula pendula"))),
                               "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + xlab("Elevation") + ylab("Probability of False Spring") +
  guides(fill=FALSE, linetype=FALSE) + scale_linetype_manual(name="Climate Change", values=c("solid", "dotted"),
                                         labels=c("0"="1950-1983",
                                                  "1"="1984-2016")) + 
  theme(legend.key = element_rect(colour = "transparent", fill = "transparent"), legend.text.align = 0) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_manual(name="Species", values=c("#7FC97F","#BF5B17"),
                      labels=c("BETPEN"=expression(paste(italic("Betula pendula"))),
                               "FRAEXC"=expression(paste(italic("Fraxinus excelsior")))))

me.mat <- bb %>%  add_predicted_draws(orig.full, method = "predict") %>%
  filter(species==c("ALNGLU", "QUEROB"))

quartz()
ggplot(me.mat, aes(x=mst, y=.prediction, col=species, linetype=as.factor(cc))) + stat_smooth(method="lm", span=0.5, se=TRUE, 
                                                                                               aes(fill=species, linetype=as.factor(cc))) +
  theme_classic() +
  scale_colour_manual(name="Species", values=c("goldenrod3","#CB1788"),
                      labels=c("ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) + xlab("Mean Spring Temperature") + ylab("Probability of False Spring") +
  guides(fill=FALSE, linetype=FALSE) + scale_linetype_manual(name="Climate Change", values=c("solid", "dotted"),
                                                             labels=c("0"="1950-1983",
                                                                      "1"="1984-2016")) + 
  theme(legend.key = element_rect(colour = "transparent", fill = "transparent"), legend.text.align = 0) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_manual(name="Species", values=c("goldenrod3","#CB1788"),
                    labels=c("ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "QUEROB"=expression(paste(italic("Quercus robur")))))


me.dist <- bb %>%  add_predicted_draws(orig.full, method = "predict") %>%
  filter(species==c("ALNGLU", "QUEROB"))

quartz()
ggplot(me.dist, aes(x=distkm, y=.prediction, col=species, linetype=as.factor(cc))) + stat_smooth(method="lm", span=0.5, se=TRUE, 
                                                                                             aes(fill=species, linetype=as.factor(cc))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="transparent"),
        legend.text.align = 0) +
  scale_colour_manual(name="Species", values=c("goldenrod3","#CB1788"),
                      labels=c("ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) + xlab("Distance from Coast") + ylab("Probability of False Spring") +
  guides(fill=FALSE) + scale_linetype_manual(name="Climate Change", values=c("solid", "dotted"),
                                                             labels=c("0"="1950-1983",
                                                                      "1"="1984-2016")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_manual(name="Species", values=c("goldenrod3","#CB1788"),
                    labels=c("ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "QUEROB"=expression(paste(italic("Quercus robur")))))





five <- read.csv("analyses/output/fs_newspace_five.csv", header=TRUE)


ggplot(five, aes(x=elev, y=fs, col=species)) + stat_smooth(method="lm", span=0.5, se=TRUE, 
                                                           aes(fill=species))
