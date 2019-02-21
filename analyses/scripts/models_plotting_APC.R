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
library(dplyr)


setwd("~/Documents/git/regionalrisk/")

load("orig_full_fagus.Rdata")

bb <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$eigen-mean(bb$eigen,na.rm=TRUE))/(2*sd(bb$eigen,na.rm=TRUE))

bb$species <- as.character(ifelse(bb$species=="FAGSYL", "aaFAGSYL", bb$species))

bb<-bb[sample(nrow(bb), 10000), ]

#species <- data.frame(species = c("BETPEN", "FRAEXC"))

### This function never compiles. Error about memory space, I've tried changing the memory space but to no avail... yet.
#goo <- marginal_effects(orig.full, probs = c(0.25, 0.75), method = "fitted", 
 #                       effects = c("elev.z", "elev.z:cc.z"), conditions = species) 


##### LIZZIE, start here through..... 
me.elev <- bb %>%  add_predicted_draws(orig.full.fagus, method = "predict") %>%
  filter(species==c("BETPEN", "FRAEXC"))


me.elev<-read.csv("~/Documents/git/regionalrisk/me.elev.csv", header=TRUE)
me.elev$species <- as.character(me.elev$species)
me.bpfr <- filter(me.elev, species==c("BETPEN", "FRAEXC"))

### Quick Plotting to see! Need to edit the standard error plotting but just to get a feel for now ###
quartz()
cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
me.species <- me.elev
me.species$species <- ifelse(me.species$species=="BETPEN", "aaBETPEN", me.species$species)
me.species$species <- ifelse(me.species$species=="aaFAGSYL", "FAGSYL", me.species$species)
me.species$species <- ifelse(me.species$species=="FRAEXC", "zFRAEXC", me.species$species)

all_elev <- ggplot(me.species, aes(x=elev, y=.prediction, col=species, linetype=as.factor(cc))) + 
  stat_smooth(method="lm", span=0.9, se=TRUE, aes(fill=species, linetype=as.factor(cc))) +
  theme_classic() +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  xlab("Elevation") + ylab("Probability of False Spring") +
  guides(fill=FALSE, linetype=FALSE) + scale_linetype_manual(name="Climate Change", values=c("solid", "dotted"),
                                                             labels=c("0"="1950-1983",
                                                                      "1"="1984-2016")) + 
  theme(legend.key = element_rect(colour = "transparent", fill = "transparent"), legend.text.align = 0, legend.position = "none") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  coord_cartesian(ylim=c(0,1)) + ggtitle("C.")

all_mat <- ggplot(me.species, aes(x=mst, y=.prediction, col=species, linetype=as.factor(cc))) + 
  stat_smooth(method="lm", span=0.9, se=TRUE, aes(fill=species, linetype=as.factor(cc))) +
  theme_classic() +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  xlab("Mean Spring Temperature") + ylab("Probability of False Spring") +
  guides(fill=FALSE) + scale_linetype_manual(name="Climate Change", values=c("solid", "dotted"),
                                                             labels=c("0"="1950-1983",
                                                                      "1"="1984-2016")) + 
  theme(legend.key = element_rect(colour = "transparent", fill = "transparent"), legend.text.align = 0, legend.position = "none") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  coord_cartesian(ylim=c(0,1)) + ggtitle("A.")

all_dist <- ggplot(me.species, aes(x=distkm, y=.prediction, col=species, linetype=as.factor(cc))) + 
  stat_smooth(method="lm", span=0.9, se=TRUE, aes(fill=species, linetype=as.factor(cc))) +
  theme_classic() +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  xlab("Distance from Coast") + ylab("Probability of False Spring") +
  guides(fill=FALSE) + scale_linetype_manual(name="Climate Change", values=c("solid", "dotted"),
                                             labels=c("0"="1950-1983",
                                                      "1"="1984-2016")) + 
  theme(legend.key = element_rect(colour = "transparent", fill = "transparent"), legend.text.align = 0, legend.position = "none") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  coord_cartesian(ylim=c(0,1)) + ggtitle("B.")

all_nao <- ggplot(me.species, aes(x=nao, y=.prediction, col=species, linetype=as.factor(cc))) + 
  stat_smooth(method="lm", span=0.9, se=TRUE, aes(fill=species, linetype=as.factor(cc))) +
  theme_classic() +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  xlab("NAO Index") + ylab("Probability of False Spring") +
  guides(fill=FALSE) + scale_linetype_manual(name="Climate Change", values=c("solid", "dotted"),
                                             labels=c("0"="1950-1983",
                                                      "1"="1984-2016")) + 
  theme(legend.key = element_rect(colour = "transparent", fill = "transparent"), legend.text.align = 0, legend.position="none") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  coord_cartesian(ylim=c(0,1)) + ggtitle("D.")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(all_nao)

grid.arrange(allyrs, climate, nrow=3, heights = c(3, 0.5, 1.3), layout_matrix=rbind(c(1, 1, 1),
                                                                                    c(NA),
                                                                                    c(NA, 2, 2, 2, NA)))


quartz()
grid.arrange(all_mat, all_dist, all_elev, all_nao, mylegend, heights=c(2, 1.5),
             layout_matrix=rbind(c(2, NA, 2),
                                 c(1, 1, 1)))
             

#### HERE! 




if(FALSE){
launch_shinystan(orig.full)

me.elev.fits <- fitted(orig.full, nsamples = 10,
                       probs=c(0.1,0.9))

fittedness <- as.data.frame(cbind(Y = standata(orig.full)$Y, elev = standata(orig.full)$elev.z, 
                                   species = standata(orig.full)$species, cc = standata(orig.full)$cc.z,
                                    me.elev.fits))

quartz()
ggplot(fittedness, aes(x=elev.z, y=Y, col=species, linetype=as.factor(cc))) + 
  stat_smooth(method="lm", span=0.9, se=TRUE, aes(fill=species, linetype=as.factor(cc))) +
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




ggplot(fittedness) + geom_point(aes(x = Estimate, y = Y))


}







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
