## Cat - 12 August 2018
# Ben Goodrich suggested I try the stan_biglm function
# Working on honing the model and making plots!

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(rstan)
library(brms)
library(sjPlot)
library(sjmisc)
library(jtools)
#library(MASS)
library(RColorBrewer)
library(dplyr)
library(bayesplot)
library(ggplot2)
library(egg)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/output")

bb<-read.csv("fs_space_new.csv", header=TRUE)
#bs<-read.csv("regrisk.fixed.csv", header=TRUE)
#bb<-read.csv("bb_latprep_nov.csv", header=TRUE)
#bb<-read.csv("bb_latprep_nov_5.csv", header=TRUE)

#bb$sm.elev<-bb$elev/100
#bb<-na.omit(bb)
#write.csv(bb, file="~/Documents/git/regionalrisk/analyses/output/bb.brm2.csv", row.names = FALSE)
#bb$species<-as.numeric(as.factor(bb$species))

### Lines 27-67 are following Ben's example on the stan_biglm documentation
## Need a coastal parameter!

bb$nao.z <- abs((bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE)))
bb$mat.z <- abs((bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE)))
bb$cc.z <- abs((bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE)))
bb$elev.z <- abs((bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE)))
bb$lat.z <- abs((bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE)))
bb$dist.z <-abs((bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE)))
bb$space.z <-abs((bb$eigen-mean(bb$eigen,na.rm=TRUE))/(2*sd(bb$eigen,na.rm=TRUE)))

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

bb$species<-ifelse(bb$species=="FAGSYL", "aaFAGSYL", bb$species)

fit<-glm(fs.count~ nao.z + mat.z + elev.z + dist.z + space.z +
          cc.z + species + nao.z:species + 
          mat.z:species + elev.z:species + dist.z:species + space.z:species + cc.z:species + 
          nao.z:cc.z + mat.z:cc.z + elev.z:cc.z + dist.z:cc.z + space.z:cc.z, data=bb, family=quasipoisson())

b <- coef(fit)[-1]
R <- qr.R(fit$qr)[-1,-1]
SSR <- crossprod(fit$residuals)[1]
not_NA <- !is.na(fitted(fit))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$nao.z)), as.numeric(mean(bb$mat.z)), as.numeric(mean(bb$elev.z)),  as.numeric(mean(bb$dist.z)), as.numeric(mean(bb$space.z)), as.numeric(mean(bb$cc.z)),  
          
          as.numeric(as.factor("AESHIP")), as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")), 
          
          as.numeric(mean(bb$nao.z[bb$species=="AESHIP"])), as.numeric(mean(bb$nao.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$nao.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$nao.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$nao.z[bb$species=="QUEROB"])), 
          
          as.numeric(mean(bb$mat.z[bb$species=="AESHIP"])), as.numeric(mean(bb$mat.z[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$mat.z[bb$species=="BETPEN"])), as.numeric(mean(bb$mat.z[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$mat.z[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$elev.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$elev.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$elev.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$elev.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$elev.z[bb$species=="QUEROB"])), 
          
          as.numeric(mean(bb$dist.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$dist.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$dist.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$dist.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$dist.z[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$space.z[bb$species=="AESHIP"])),
          as.numeric(mean(bb$space.z[bb$species=="ALNGLU"])), as.numeric(mean(bb$space.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$space.z[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$space.z[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$cc.z[bb$species=="AESHIP"])), 
          as.numeric(mean(bb$cc.z[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc.z[bb$species=="BETPEN"])),
          as.numeric(mean(bb$cc.z[bb$species=="FRAEXC"])), as.numeric(mean(bb$cc.z[bb$species=="QUEROB"])),
          as.numeric(mean(bb$nao.z*bb$cc.z)), as.numeric(mean(bb$mat.z*bb$cc.z)), 
          as.numeric(mean(bb$elev.z*bb$cc.z)), as.numeric(mean(bb$dist.z*bb$cc.z)), as.numeric(mean(bb$space.z*bb$cc.z)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
post.inter <- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                             # the next line is only to make the example go fast
                             chains = 4, iter = 2000)
cbind(lm = b, stan_lm = rstan::get_posterior_mean(post.inter)[1:26,]) # shrunk
# }
launch_shinystan(post.inter)

names(post.inter)
mod<-stan_glm(mst~m.index*cc, data=bb)

#lm(formula = mst ~ m.index * cc, data = bb)
#coef.est coef.se
#(Intercept)  7.68     0.00  
#m.index      0.59     0.01  
#cc           0.85     0.00  
#m.index:cc  -0.01     0.01  
#---
#  n = 754786, k = 4
#residual sd = 1.47, R-Squared = 0.11

mod<-lm(~m.index*cc, data=bb)


cols <- colorRampPalette(brewer.pal(9,"Set1"))(6)
##### Interaction Plots code
nao<- plot_model(bernszeroonepriors, type = "pred", terms = c("nao.z", "species")) + xlab("NAO") + 
  ylab("Number of False Springs") + ggtitle("A.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.8))+
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "BETPEN"=expression(paste(italic("Betula lenta"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "BETPEN"=expression(paste(italic("Betula lenta"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) 
elev<- plot_model(bernsshort, type = "pred", terms = c("elev.z", "species")) + xlab("Elevation") + 
  ylab("Number of False Springs") + ggtitle("B.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.8)) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "BETPEN"=expression(paste(italic("Betula lenta"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "BETPEN"=expression(paste(italic("Betula lenta"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) 
mat<- plot_model(bernsshort, type = "pred", terms = c("mat.z", "species")) + xlab("Mean Spring Temperature") + 
  ylab("Number of False Springs") + ggtitle("C.") + theme_classic()+ theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.8)) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "BETPEN"=expression(paste(italic("Betula lenta"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "BETPEN"=expression(paste(italic("Betula lenta"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) 
space<- plot_model(bernsshort, type = "pred", terms = c("dist.z", "species")) + xlab("Distance from Coast") + ylab("Number of False Springs") + 
  ggtitle("D.") + scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.8)) + 
  theme_classic() + theme(legend.position = "none") + 
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "BETPEN"=expression(paste(italic("Betula lenta"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "BETPEN"=expression(paste(italic("Betula lenta"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) 
ccsp<- plot_model(bernsshort, type = "pred", terms = c("cc.z", "species")) + xlab("Climate Change") + ylab("Number of False Springs") + ggtitle("E.") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4))  + 
  theme_classic() + theme(legend.position = "none") + 
  #theme(legend.text.align = 0, legend.key = element_rect(fill="white")) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "BETPEN"=expression(paste(italic("Betula lenta"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "BETPEN"=expression(paste(italic("Betula lenta"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "FRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) 

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(ccsp)

quartz()
grid.arrange(nao, mat, ccsp, elev, space, mylegend, ncol=3, nrow=2)

colz <- colorRampPalette(brewer.pal(9,"Set1"))(2)
colz<-rev(colz)
nao<- marginal_effects(bernszeroonepriors, type = "pred", terms = c("nao.z", "cc.z")) + xlab("NAO") + 
  ylab("Number of False Springs") + ggtitle("") + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016"))
elev<- plot_model(fit, type = "pred", terms = c("elev.z", "cc.z")) + xlab("Elevation") + 
  ylab("Number of False Springs") + ggtitle("") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))
mat<- plot_model(fit, type = "pred", terms = c("mat.z", "cc.z")) + xlab("Mean Spring Temperature") + 
  ylab("Number of False Springs") + ggtitle("") + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))
space<- plot_model(fit, type = "pred", terms = c("dist.z", "cc.z")) + xlab("Distance from Coast") + ylab("Number of False Springs") + 
  ggtitle("") + #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))

quartz()
ggarrange(nao, elev, mat, space, ncol=2, nrow=2)

### A cleaner version! ####

### Going to make Fagus sylvatica the baseline because it is the most phenologically in the middle
# Avg budburst overall for the entire dataset is 104.8766 and Fagus is 106.7, Betula (the next closest) is 99.24

bb$species<-ifelse(bb$species=="FAGSYL", "aaFAGSYL", bb$species)

fit<-lm(fs.count~m.index + m.index:species + sp.temp + sp.temp:species + sm.elev + sm.elev:species + 
          space + space:species + cc  + cc:species + species + m.index:cc + sp.temp:cc +sm.elev:cc +
          space:cc, data=bb)             # not necessary in this case

b <- coef(fit)[-1]
R <- qr.R(fit$qr)[-1,-1]
SSR <- crossprod(fit$residuals)[1]
not_NA <- !is.na(fitted(fit))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$m.index)),  as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$sm.elev)), as.numeric(mean(bb$space)), 
          as.numeric(mean(bb$cc)),  
          
          as.numeric(as.factor("AESHIP")),
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")),
    
          as.numeric(mean(bb$m.index[bb$species=="AESHIP"])),
          as.numeric(mean(bb$m.index[bb$species=="ALNGLU"])), as.numeric(mean(bb$m.index[bb$species=="BETPEN"])),
          as.numeric(mean(bb$m.index[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$m.index[bb$species=="QUEROB"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="AESHIP"])),
          as.numeric(mean(bb$sp.temp[bb$species=="ALNGLU"])), as.numeric(mean(bb$sp.temp[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sp.temp[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="QUEROB"])),
          
          as.numeric(mean(bb$sm.elev[bb$species=="AESHIP"])), 
          as.numeric(mean(bb$sm.elev[bb$species=="ALNGLU"])), as.numeric(mean(bb$sm.elev[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sm.elev[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$sm.elev[bb$species=="QUEROB"])), 
          
          as.numeric(mean(bb$space[bb$species=="AESHIP"])),
          as.numeric(mean(bb$space[bb$species=="ALNGLU"])), as.numeric(mean(bb$space[bb$species=="BETPEN"])),
          as.numeric(mean(bb$space[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$space[bb$species=="QUEROB"])),
          
          
          as.numeric(mean(bb$cc[bb$species=="AESHIP"])),
          as.numeric(mean(bb$cc[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc[bb$species=="BETPEN"])), 
          as.numeric(mean(bb$cc[bb$species=="FRAEXC"])), as.numeric(mean(bb$cc[bb$species=="QUEROB"])),
           
          as.numeric(mean(bb$m.index*bb$cc)), as.numeric(mean(bb$sp.temp*bb$cc)), as.numeric(mean(bb$sm.elev*bb$cc)), 
          as.numeric(mean(bb$space*bb$cc)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
post.inter <- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                             # the next line is only to make the example go fast
                             chains = 4, iter = 2000)
cbind(lm = b, stan_lm = rstan::get_posterior_mean(post.inter)[1:26,]) # shrunk
# }



##### Ugly but workable code for plotting ####
plotting <- as.data.frame(summary(post.inter)$summary)
simple<-plotting
simple$var<- rownames(simple)
rownames(simple)<-1:45
simple<-simple[2:40,]
#simple<-simple[!(simple$var=="speciesALNGLU"|simple$var=="speciesBETPEN"|simple$var=="speciesFAGSYL"|
 #simple$var=="speciesFRAEXC"|simple$var=="speciesQUEROB"),]
simple<-subset(simple, select=c("var", "mean", "2.5%", "97.5%"))
simple$species<-c(1,1,1,1,1,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,0,0,0,0)
simple$Jvar<-NA
#simple$Jvar<-ifelse(simple$var=="(Intercept)", 10, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="nao.z", 9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesALNGLU", 8.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesBETPEN", 8.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesFAGSYL", 8.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesFRAEXC", 8.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="m.index:speciesQUEROB", 8.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="mat.z", 8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesALNGLU", 7.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesBETPEN", 7.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesFAGSYL", 7.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesFRAEXC", 7.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sp.temp:speciesQUEROB", 7.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="elev.z", 7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesALNGLU", 6.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesBETPEN", 6.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesFAGSYL", 6.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesFRAEXC", 6.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="sm.elev:speciesQUEROB", 6.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="dist.z", 6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesALNGLU", 5.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesBETPEN", 5.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesFAGSYL", 5.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesFRAEXC", 5.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="space:speciesQUEROB", 5.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="cc.z", 5, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesALNGLU", 4.9, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesBETPEN", 4.8, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesFAGSYL", 4.7, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesFRAEXC", 4.6, simple$Jvar)
#simple$Jvar<-ifelse(simple$var=="cc:speciesQUEROB", 4.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="nao.z:cc.z", 4, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="mat.z:cc.z", 3, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="elev.z:cc.z", 2, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="dist.z:cc.z", 1, simple$Jvar)

species<-unique(simple$species)
simple$est<-simple$mean
simple$var2<-gsub("(species).*","\\1",simple$var)
for(i in c(1:length(species))) {
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="m.index:species", simple$mean[simple$var=="m.index" & simple$species==1] +
                        simple$mean[simple$var2=="m.index:species" & simple$species==species[i]], simple$est)
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="sp.temp:species", simple$mean[simple$var=="sp.temp" & simple$species==1] +
                        simple$mean[simple$var2=="sp.temp:species" & simple$species==species[i]], simple$est)
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="sm.elev:species", simple$mean[simple$var=="sm.elev" & simple$species==1] +
                       simple$mean[simple$var2=="sm.elev:species" & simple$species==species[i]], simple$est)
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="space:species", simple$mean[simple$var=="space"& simple$species==1] +
                       simple$mean[simple$var2=="space:species" & simple$species==species[i]], simple$est)
  simple$est<-ifelse(simple$species==species[i] & simple$var2=="cc:species", simple$mean[simple$var=="cc"& simple$species==1] +
                       simple$mean[simple$var2=="cc:species" & simple$species==species[i]], simple$est)
  
}

for(i in c(1:length(species))) {
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="m.index:species", simple$mean[simple$var=="m.index" & simple$species==1] +
                       simple$mean[simple$var2=="m.index:species" & simple$species==species[i]], simple$`2.5%`)
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="sp.temp:species", simple$mean[simple$var=="sp.temp" & simple$species==1] +
                       simple$mean[simple$var2=="sp.temp:species" & simple$species==species[i]], simple$`2.5%`)
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="sm.elev:species", simple$mean[simple$var=="sm.elev" & simple$species==1] +
                       simple$mean[simple$var2=="sm.elev:species" & simple$species==species[i]], simple$`2.5%`)
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="space:species", simple$mean[simple$var=="space"& simple$species==1] +
                       simple$mean[simple$var2=="space:species" & simple$species==species[i]], simple$`2.5%`)
  simple$`2.5%`<-ifelse(simple$species==species[i] & simple$var2=="cc:species", simple$mean[simple$var=="cc"& simple$species==1] +
                       simple$mean[simple$var2=="cc:species" & simple$species==species[i]], simple$`2.5%`)
  
}

for(i in c(1:length(species))) {
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="m.index:species", simple$mean[simple$var=="m.index" & simple$species==1] +
                          simple$mean[simple$var2=="m.index:species" & simple$species==species[i]], simple$`97.5%`)
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="sp.temp:species", simple$mean[simple$var=="sp.temp" & simple$species==1] +
                          simple$mean[simple$var2=="sp.temp:species" & simple$species==species[i]], simple$`97.5%`)
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="sm.elev:species", simple$mean[simple$var=="sm.elev" & simple$species==1] +
                          simple$mean[simple$var2=="sm.elev:species" & simple$species==species[i]], simple$`97.5%`)
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="space:species", simple$mean[simple$var=="space"& simple$species==1] +
                          simple$mean[simple$var2=="space:species" & simple$species==species[i]], simple$`97.5%`)
  simple$`97.5%`<-ifelse(simple$species==species[i] & simple$var2=="cc:species", simple$mean[simple$var=="cc"& simple$species==1] +
                          simple$mean[simple$var2=="cc:species" & simple$species==species[i]], simple$`97.5%`)
  
}


simple<-simple[!(simple$var=="speciesALNGLU"|simple$var=="speciesBETPEN"|simple$var=="speciesFAGSYL"|
                   simple$var=="speciesFRAEXC"|simple$var=="speciesQUEROB"),]


cols <- colorRampPalette(brewer.pal(9,"Set1"))(7)
estimates<-c("NAO Index", "Mean Spring Temperature", "Elevation", "Distance from Coast", "Climate Change",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change",
             "Elevation x \nClimate Chnage", "Distance from Coast \nx Climate Change")
estimates<-rev(estimates)
regrisk<-ggplot(simple, aes(x=`2.5%`, xend=`97.5%`, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=mean, y=Jvar), col="blue3") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc")), col="blue3") +
  scale_y_discrete(limits = sort(unique(simple$var)), labels=estimates) +
  xlab("Model Estimate of Change in \nNumber of False Springs") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0)  #+ coord_cartesian(ylim=c(1,5), xlim=c(-20, 10))
quartz()
regrisk

############ Model Output
brms<-as.data.frame(tidy(bernszeroonepriors,robust = TRUE))
brms<-brms[2:47,]
brms$term<-gsub(".*b_","",brms$term)
brms$term<-gsub(".*r_species","",brms$term)
brms<-brms[!(brms$term=="sd_species__nao.z" | brms$term=="sd_species__mat.z" | brms$term=="sd_species__elev.z"
             | brms$term=="sd_species__dist.z" | brms$term=="sd_species__space.z" | brms$term=="sd_species__cc.z" | brms$term=="sigma"),]

brms$species<-c(0,0,0,0,0,0, 2,3,4,5,6, 2,3,4,5,6, 2,3,4,5,6, 2,3,4,5,6, 2,3,4,5,6, 2,3,4,5,6, 2,3,4,5,6, 0,0,0,0,0)
brms$Jvar<-NA
brms$Jvar<-ifelse(brms$term=="nao.z", 11, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[AESHIP,nao.z]", 8.9, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[ALNGLU,nao.z]", 8.8, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[BETPEN,nao.z]", 8.7, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FAGSYL,nao.z]", 8.6, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FRAEXC,nao.z]", 8.5, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[QUEROB,nao.z]", 8.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="mat.z", 10, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[AESHIP,mat.z]", 7.9, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[ALNGLU,mat.z]", 7.8, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[BETPEN,mat.z]", 7.7, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FAGSYL,mat.z]", 7.6, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FRAEXC,mat.z]", 7.5, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[QUEROB,mat.z]", 7.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="elev.z", 9, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[AESHIP,elev.z]", 6.9, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[ALNGLU,elev.z]", 6.8, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[BETPEN,elev.z]", 6.7, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FAGSYL,elev.z]", 6.6, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FRAEXC,elev.z]", 6.5, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[QUEROB,elev.z]", 6.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="dist.z", 8, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[AESHIP,dist.z]", 6.9, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[ALNGLU,dist.z]", 6.8, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[BETPEN,dist.z]", 6.7, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FAGSYL,dist.z]", 6.6, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FRAEXC,dist.z]", 6.5, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[QUEROB,dist.z]", 6.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="space.z", 7, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[AESHIP,space]", 5.9, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[ALNGLU,space]", 5.8, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[BETPEN,space]", 5.7, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FAGSYL,space]", 5.6, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FRAEXC,space]", 5.5, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[QUEROB,space]", 5.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="cc.z", 6, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[AESHIP,cc]", 4.9, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[ALNGLU,cc]", 4.8, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[BETPEN,cc]", 4.7, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FAGSYL,cc]", 4.6, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[FRAEXC,cc]", 4.5, brms$Jvar)
#brms$Jvar<-ifelse(brms$term=="[QUEROB,cc]", 4.4, brms$Jvar)

brms$Jvar<-ifelse(brms$term=="nao.z:cc.z", 5, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="mat.z:cc.z", 4, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="elev.z:cc.z", 3, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="dist.z:cc.z", 2, brms$Jvar)
brms$Jvar<-ifelse(brms$term=="space.z:cc.z", 1, brms$Jvar)

bb<-brms[(brms$species==0),]
cols <- colorRampPalette(brewer.pal(9,"Set1"))(7)
estimates<-c("NAO Index", "Mean Spring Temperature", "Elevation", "Distanct from Coast", "Space Parameter", "Climate Change",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change",
             "Elevation x \nClimate Chnage", "Distance from Coast \nx Climate Change", "Space Parameter \nx Climate Change")
estimates<-rev(estimates)
regrisk<-ggplot(bb, aes(x=lower, xend=upper, y=Jvar, yend=Jvar, col=as.factor(species))) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=Jvar, col=as.factor(species), size=as.factor(species))) +
  #scale_colour_manual(name="Species", values=cols,
  #                   labels=c("1"=expression(paste(italic("Aesculus hippocastanum"))),
  #                           "2"=expression(paste(italic("Alnus glutinosa"))),
  #                          "3"=expression(paste(italic("Betula lenta"))),
  #                         "4"=expression(paste(italic("Fagus sylvatica"))),
  #                        "5"=expression(paste(italic("Fraxinus excelsior"))),
  #                       "6"=expression(paste(italic("Quercus robur"))),
  #                      "0"="Overall Effects"))+
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  scale_y_discrete(limits = sort(unique(brms$term)), labels=estimates) +
  xlab("Change in Number of False Springs") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0) + coord_cartesian(ylim=c(1,11))
#scale_size_manual(values=c(3, 1, 1, 1, 1, 1, 1, 1, 1, 1), name="Species",
#                 labels=c("1"=expression(paste(italic("Aesculus hippocastanum"))),
#                         "2"=expression(paste(italic("Alnus glutinosa"))),
#                        "3"=expression(paste(italic("Betula lenta"))),
#                       "4"=expression(paste(italic("Fagus sylvatica"))),
#                      "5"=expression(paste(italic("Fraxinus excelsior"))),
#                     "6"=expression(paste(italic("Quercus robur"))),
#                    "0"="Overall Effects"))
quartz()
regrisk

nao<-interact_plot(model = bernszeroonepriors, pred = nao.z, modx = cc.z) + xlab("NAO Index") + ylab("Num. False Springs") + theme(legend.position = "none")
elev<-interact_plot(model = slopes.fast, pred = sm.elev, modx = cc) + xlab("Elevation") + ylab("Num. False Springs") + theme(legend.position = "none")
mat<-interact_plot(model = slopes.fast, pred = sp.temp, modx = cc) + xlab("Mean Spring Temperature") + ylab("Num. False Springs") 
spa<-interact_plot(model = slopes.fast, pred = space, modx = cc) + xlab("Space") + ylab("Num. False Springs")

ggarrange(nao, mat, elev, spa, ncol=2, nrow=2)

test<-lm(fs.count~m.index+sp.temp+sm.elev+space+cc+species+m.index:cc+
           sp.temp:cc+sm.elev:cc+space:cc, data=bb)
b <- coef(test)[-1]
R <- qr.R(test$qr)[-1,-1]
SSR <- crossprod(test$residuals)[1]
not_NA <- !is.na(fitted(test))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$m.index)),  as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$sm.elev)), as.numeric(mean(bb$space)), 
          as.numeric(mean(bb$cc)),
          
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FAGSYL")), as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")),
          
          as.numeric(mean(bb$m.index*bb$cc)), as.numeric(mean(bb$sp.temp*bb$cc)), as.numeric(mean(bb$sm.elev*bb$cc)), 
          as.numeric(mean(bb$space*bb$cc)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
test.big<- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                             # the next line is only to make the example go fast
                             chains = 4, iter = 2000)

