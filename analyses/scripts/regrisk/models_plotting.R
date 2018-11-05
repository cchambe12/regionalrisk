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
library(sjmisc)
library(sjPlot)
library(ggeffects)


cols <- colorRampPalette(brewer.pal(9,"Set1"))(6)
##### Interaction Plots code
naosp<- ggpredict(bernszeroonepriors, terms = c("nao.z", "species")) 
naosp.p<-ggplot(nao, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("NAO") + 
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
elev<- plot_model(bernszeroonepriors, type = "pred", terms = c("elev.z", "species")) + xlab("Elevation") + 
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
mat<- plot_model(bernszeroonepriors, type = "pred", terms = c("mat.z", "species")) + xlab("Mean Spring Temperature") + 
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
space<- plot_model(bernszeroonepriors, type = "pred", terms = c("dist.z", "species")) + xlab("Distance from Coast") + ylab("Number of False Springs") + 
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
ccsp<- plot_model(bernszeroonepriors, type = "pred", terms = c("cc.z", "species")) + xlab("Climate Change") + ylab("Number of False Springs") + ggtitle("E.") + 
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
nao<-ggpredict(bernszeroonepriors, terms=c("nao.z", "cc.z"))
nao.p<- ggplot(nao, aes(x=x, predicted=predicted, col=group)) + xlab("NAO") + 
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



# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/output")

bb<-read.csv("fs_space_new.csv", header=TRUE)

bb$nao.z <- abs((bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE)))
bb$mat.z <- abs((bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE)))
bb$cc.z <- abs((bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE)))
bb$elev.z <- abs((bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE)))
bb$lat.z <- abs((bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE)))
bb$dist.z <-abs((bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE)))
bb$space.z <-abs((bb$eigen-mean(bb$eigen,na.rm=TRUE))/(2*sd(bb$eigen,na.rm=TRUE)))

bb$fs<-ifelse(bb$fs.count>0, 1, 0)


bernie<-broom::tidy(bernszeroonepriors)

naocc<-bb %>% data_grid(nao = seq_range(nao.z, n=101), cc.z) %>%
  add_fitted_draws(bernszeroonepriors, n=100) %>%
  ggplot(aes(x=nao.z, y=fs, col=species)) +
  geom_line(aes(y=.value, group=.draw), alpha=0.2, col="red") +
  geom_point(data=bb)



naopred<-ggpredict(bernie, terms = c("nao.z"))
naosp<-ggplot(naopred, aes(x, predicted, col=group)) + geom_stat()

plot_model(bernszeroonepriors)

