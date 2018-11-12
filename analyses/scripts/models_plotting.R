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
library(broom)

setwd("~/Documents/git/regionalrisk/analyses/output")

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)

##### Interaction Plots code

############################################################################
################### Minus Five #############################################

naosp<- ggpredict(bernsnewdvr, terms = c("nao.z", "species"), prob=0.5) 
#write.csv(naosp, file="~/Documents/git/regionalrisk/analyses/output/naosp_predicted_five.csv", row.names = FALSE)
naosp<-read.csv("~/Documents/git/regionalrisk/analyses/output/naosp_predicted_five.csv", header=TRUE)
naosp.p<-ggplot(naosp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("NAO") + 
  ylab("Number of False Springs") + ggtitle("A.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  #coord_cartesian(ylim=c(0,0.4))+
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
elevsp<- ggpredict(bernsfive, terms = c("elev.z", "species")) 
#write.csv(elevsp, file="~/Documents/git/regionalrisk/analyses/output/elevsp_predicted_five.csv", row.names = FALSE)
elevsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/elevsp_predicted_five.csv", header=TRUE)
elevsp.p<-ggplot(elevsp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Elevation") + 
  ylab("Number of False Springs") + ggtitle("B.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) +
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
matsp<- ggpredict(bernsfive, terms = c("mat.z", "species")) 
#write.csv(matsp, file="~/Documents/git/regionalrisk/analyses/output/matsp_predicted_five.csv", row.names = FALSE)
matsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/matsp_predicted_five.csv", header=TRUE)
matsp.p<-ggplot(matsp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Mean Spring Temperature") + 
  ylab("Number of False Springs") + ggtitle("C.") + theme_classic()+ theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) +
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
spacesp<- ggpredict(bernsfive, terms = c("space.z", "species")) 
#write.csv(spacesp, file="~/Documents/git/regionalrisk/analyses/output/spacesp_predicted_five.csv", row.names = FALSE)
spacesp<-read.csv("~/Documents/git/regionalrisk/analyses/output/spacesp_predicted_five.csv", header=TRUE)
spacesp.p<-ggplot(spacesp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Distance from Coast") + ylab("Number of False Springs") + 
  ggtitle("D.") + scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
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
ccsp<- ggpredict(bernsfive, terms = c("cc.z", "species")) 
#write.csv(ccsp, file="~/Documents/git/regionalrisk/analyses/output/ccsp_predicted_five.csv", row.names = FALSE)
ccsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/ccsp_predicted_five.csv", header=TRUE)
ccsp.p<-ggplot(ccsp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Climate Change") + ylab("Number of False Springs") + ggtitle("E.") + 
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

mylegend<-g_legend(ccsp.p)

quartz()
grid.arrange(naosp.p, matsp.p, ccsp.p, elevsp.p, spacesp.p, mylegend, ncol=3, nrow=2)

colz <- colorRampPalette(brewer.pal(9,"Set1"))(2)
colz<-rev(colz)
nao<-ggpredict(bernsnewdvr, terms=c("nao.z", "cc.z"), prob=0.5)
#write.csv(nao, file="naopredict_five.csv", row.names=FALSE)
#nao<-read.csv("~/Documents/git/regionalrisk/analyses/output/naopredict_five.csv", header=TRUE)
#nao$group<-as.character(nao$group)
nao.p<- ggplot(nao, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("NAO") + 
  ylab("Number of False Springs") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  #coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))

test<-read.csv("fs_newdvr_space.csv", header=TRUE)
test$nao.z <- (test$nao-mean(test$nao,na.rm=TRUE))/(2*sd(test$nao,na.rm=TRUE))
test$cc.z <- (test$cc-mean(test$cc,na.rm=TRUE))/(2*sd(test$cc,na.rm=TRUE))
naocc.test<-ggplot(test, aes(x=nao.z, y=fs, col=as.character(cc.z))) + geom_line(aes(col=as.character(cc.z)), stat="smooth", method="lm") + xlab("NAO") + 
  ylab("Probability of False Spring") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  #coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))

elev<-ggpredict(bernsfive, terms=c("elev.z", "cc.z"))
#write.csv(elev, file="elevpredict_five.csv", row.names=FALSE)
#elev<-read.csv("~/Documents/git/regionalrisk/analyses/output/elevpredict_five.csv", header=TRUE)
#elev$group<-as.character(elev$group)
elev.p<- ggplot(elev, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Elevation") + 
  ylab("Number of False Springs") + ggtitle("") + theme_classic() +
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))
mat<-ggpredict(bernsfive, terms=c("mat.z", "cc.z"))
#write.csv(mat, file="~/Documents/git/regionalrisk/analyses/output/matpredict_five.csv", row.names=FALSE)
#mat<-read.csv("~/Documents/git/regionalrisk/analyses/output/matpredict_five.csv", header=TRUE)
#mat$group<-as.character(mat$group)
mat.p<- ggplot(mat, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Mean Spring Temperature") + 
  ylab("Number of False Springs") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))
dist<-ggpredict(bernsfive, terms=c("dist.z", "cc.z"))
#write.csv(dist, file="~/Documents/git/regionalrisk/analyses/output/distpredict_five.csv", row.names=FALSE)
#dist<-read.csv("~/Documents/git/regionalrisk/analyses/output/distpredict_five.csv", header=TRUE)
#dist$group<-as.character(dist$group)
dist.p<- ggplot(dist, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Distance from Coast") + ylab("Number of False Springs") + 
  ggtitle("") + theme_classic() + #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))

quartz()
ggarrange(nao.p, elev.p, mat.p, dist.p, ncol=2, nrow=2)


############################################################################
################### Original ###############################################

naosp<- ggpredict(bernszeroonepriors, terms = c("nao.z", "species")) 
#write.csv(naosp, file="~/Documents/git/regionalrisk/analyses/output/naosp_predicted.csv", row.names = FALSE)
naosp<-read.csv("~/Documents/git/regionalrisk/analyses/output/naosp_predicted.csv", header=TRUE)
naosp.p<-ggplot(naosp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("NAO") + 
  ylab("Number of False Springs") + ggtitle("A.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4))+
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
elevsp<- ggpredict(bernszeroonepriors, terms = c("elev.z", "species")) 
#write.csv(elevsp, file="~/Documents/git/regionalrisk/analyses/output/elevsp_predicted.csv", row.names = FALSE)
elevsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/elevsp_predicted.csv", header=TRUE)
elevsp.p<-ggplot(elevsp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Elevation") + 
  ylab("Number of False Springs") + ggtitle("B.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) +
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
matsp<- ggpredict(bernszeroonepriors, terms = c("mat.z", "species")) 
#write.csv(matsp, file="~/Documents/git/regionalrisk/analyses/output/matsp_predicted.csv", row.names = FALSE)
matsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/matsp_predicted.csv", header=TRUE)
matsp.p<-ggplot(matsp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Mean Spring Temperature") + 
  ylab("Number of False Springs") + ggtitle("C.") + theme_classic()+ theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) +
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
spacesp<- ggpredict(bernszeroonepriors, terms = c("space.z", "species")) 
#write.csv(spacesp, file="~/Documents/git/regionalrisk/analyses/output/spacesp_predicted.csv", row.names = FALSE)
spacesp<-read.csv("~/Documents/git/regionalrisk/analyses/output/spacesp_predicted.csv", header=TRUE)
spacesp.p<-ggplot(spacesp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Distance from Coast") + ylab("Number of False Springs") + 
  ggtitle("D.") + scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
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
ccsp<- ggpredict(bernszeroonepriors, terms = c("cc.z", "species")) 
#write.csv(ccsp, file="~/Documents/git/regionalrisk/analyses/output/ccsp_predicted.csv", row.names = FALSE)
ccsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/ccsp_predicted.csv", header=TRUE)
ccsp.p<-ggplot(ccsp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Climate Change") + ylab("Number of False Springs") + ggtitle("E.") + 
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

mylegend<-g_legend(ccsp.p)

quartz()
grid.arrange(naosp.p, matsp.p, ccsp.p, elevsp.p, spacesp.p, mylegend, ncol=3, nrow=2)

colz <- colorRampPalette(brewer.pal(9,"Set1"))(2)
colz<-rev(colz)
nao<-ggpredict(bernszeroonepriors, terms=c("nao.z", "cc.z"))
#write.csv(nao, file="naopredict.csv", row.names=FALSE)
#nao<-read.csv("~/Documents/git/regionalrisk/analyses/output/naopredict.csv", header=TRUE)
#nao$group<-as.character(nao$group)
nao.p<- ggplot(nao, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("NAO") + 
  ylab("Probability of False Spring") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))
elev<-ggpredict(bernszeroonepriors, terms=c("elev.z", "cc.z"))
#write.csv(elev, file="elevpredict.csv", row.names=FALSE)
#elev<-read.csv("~/Documents/git/regionalrisk/analyses/output/elevpredict.csv", header=TRUE)
#elev$group<-as.character(elev$group)
elev.p<- ggplot(elev, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Elevation") + 
  ylab("Probability of False Spring") + ggtitle("") + theme_classic() +
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))
mat<-ggpredict(bernszeroonepriors, terms=c("mat.z", "cc.z"))
#write.csv(mat, file="~/Documents/git/regionalrisk/analyses/output/matpredict.csv", row.names=FALSE)
#mat<-read.csv("~/Documents/git/regionalrisk/analyses/output/matpredict.csv", header=TRUE)
#mat$group<-as.character(mat$group)
mat.p<- ggplot(mat, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Mean Spring Temperature") + 
  ylab("Probability of False Spring") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))
dist<-ggpredict(bernszeroonepriors, terms=c("dist.z", "cc.z"))
#write.csv(dist, file="~/Documents/git/regionalrisk/analyses/output/distpredict.csv", row.names=FALSE)
#dist<-read.csv("~/Documents/git/regionalrisk/analyses/output/distpredict.csv", header=TRUE)
#dist$group<-as.character(dist$group)
dist.p<- ggplot(dist, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Distance from Coast") + ylab("Probability of False Spring") + 
  ggtitle("") + theme_classic() + #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))

quartz()
ggarrange(nao.p, elev.p, mat.p, dist.p, ncol=2, nrow=2)

############################################################################
################### DVR ####################################################
cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
naosp<- ggpredict(berndvrfinal, terms = c("nao.z", "species")) 
#write.csv(naosp, file="~/Documents/git/regionalrisk/analyses/output/naosp_predicted_dvr.csv", row.names = FALSE)
naosp<-read.csv("~/Documents/git/regionalrisk/analyses/output/naosp_predicted_dvr.csv", header=TRUE)
naosp$group<-ifelse(naosp$group=="BETPEN", "afBETPEN", naosp$group)
naosp$group<-ifelse(naosp$group=="FRAEXC", "zFRAEXC", naosp$group)
naosp$group<-ifelse(naosp$group=="QUEROB", "cQUEROB", naosp$group)
naosp.p<-ggplot(naosp, aes(x=x, y=predicted))+ geom_line(aes(col=group), size=1.2) + xlab("NAO") + 
  ylab("Probability of False Spring") + ggtitle("A.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1))+
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "afBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "cQUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "afBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "cQUEROB"=expression(paste(italic("Quercus robur"))))) 
elevsp<- ggpredict(berndvrfinal, terms = c("elev.z", "species")) 
#write.csv(elevsp, file="~/Documents/git/regionalrisk/analyses/output/elevsp_predicted_dvr.csv", row.names = FALSE)
elevsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/elevsp_predicted_dvr.csv", header=TRUE)
elevsp$group<-ifelse(elevsp$group=="BETPEN", "afBETPEN", elevsp$group)
elevsp$group<-ifelse(elevsp$group=="FRAEXC", "zFRAEXC", elevsp$group)
elevsp$group<-ifelse(elevsp$group=="QUEROB", "cQUEROB", elevsp$group)
elevsp.p<-ggplot(elevsp, aes(x=x, y=predicted))+ geom_line(aes(col=group), size=1.2) + xlab("Elevation") + 
  ylab("Probability of False Spring") + ggtitle("C.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "afBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "cQUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "afBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "cQUEROB"=expression(paste(italic("Quercus robur"))))) 
matsp<- ggpredict(berndvrfinal, terms = c("mat.z", "species")) 
#write.csv(matsp, file="~/Documents/git/regionalrisk/analyses/output/matsp_predicted_dvr.csv", row.names = FALSE)
matsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/matsp_predicted_dvr.csv", header=TRUE)
matsp$group<-ifelse(matsp$group=="BETPEN", "afBETPEN", matsp$group)
matsp$group<-ifelse(matsp$group=="FRAEXC", "zFRAEXC", matsp$group)
matsp$group<-ifelse(matsp$group=="QUEROB", "cQUEROB", matsp$group)
matsp.p<-ggplot(matsp, aes(x=x, y=predicted))+ geom_line(aes(col=group), size=1.2) + xlab("Mean Spring Temperature") + 
  ylab("Probability of False Spring") + ggtitle("B.") + theme_classic()+ theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "afBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "cQUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "afBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "cQUEROB"=expression(paste(italic("Quercus robur"))))) 
spacesp<- ggpredict(berndvrfinal, terms = c("space.z", "species")) 
#write.csv(spacesp, file="~/Documents/git/regionalrisk/analyses/output/spacesp_predicted_dvr.csv", row.names = FALSE)
spacesp<-read.csv("~/Documents/git/regionalrisk/analyses/output/spacesp_predicted_dvr.csv", header=TRUE)
spacesp$group<-ifelse(spacesp$group=="BETPEN", "afBETPEN", spacesp$group)
spacesp$group<-ifelse(spacesp$group=="FRAEXC", "zFRAEXC", spacesp$group)
spacesp$group<-ifelse(spacesp$group=="QUEROB", "cQUEROB", spacesp$group)
spacesp.p<-ggplot(spacesp, aes(x=x, y=predicted))+ geom_line(aes(col=group), size=1.2) + xlab("Distance from Coast") + ylab("Probability of False Spring") + 
  ggtitle("D.") + scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_classic() + theme(legend.position = "none") + 
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "afBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "cQUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "afBETPEN"=expression(paste(italic("Betula lenta"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "cQUEROB"=expression(paste(italic("Quercus robur"))))) 
ccsp<- ggpredict(berndvrfinal, terms = c("cc.z", "species")) 
#write.csv(ccsp, file="~/Documents/git/regionalrisk/analyses/output/ccsp_predicted_dvr.csv", row.names = FALSE)
ccsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/ccsp_predicted_dvr.csv", header=TRUE)
ccsp$group<-ifelse(ccsp$group=="BETPEN", "afBETPEN", ccsp$group)
ccsp$group<-ifelse(ccsp$group=="FRAEXC", "zFRAEXC", ccsp$group)
ccsp$group<-ifelse(ccsp$group=="QUEROB", "cQUEROB", ccsp$group)
ccsp.p<-ggplot(ccsp, aes(x=x, y=predicted))+ geom_line(aes(col=group), size=1.2) + xlab("Climate Change") + ylab("Probability of False Spring") + ggtitle("E.") + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1))  + 
  theme_classic() + theme(legend.position = "none") + 
  #theme(legend.text.align = 0, legend.key = element_blank()) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "afBETPEN"=expression(paste(italic("Betula pendula"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                               "cQUEROB"=expression(paste(italic("Quercus robur"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "afBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "cQUEROB"=expression(paste(italic("Quercus robur"))))) 

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(ccsp.p)

quartz()
grid.arrange(naosp.p, matsp.p, elevsp.p, spacesp.p, ccsp.p, mylegend, ncol=3, nrow=2)

colz <- colorRampPalette(brewer.pal(9,"Set1"))(2)
colz<-rev(colz)
nao<-ggpredict(berndvrfinal, terms=c("nao.z", "cc.z"))
#write.csv(nao, file="naopredict_dvr.csv", row.names=FALSE)
#nao<-read.csv("~/Documents/git/regionalrisk/analyses/output/naopredict_dvr.csv", header=TRUE)
#nao$group<-as.character(nao$group)
nao.p<- ggplot(nao, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("NAO") + 
  ylab("Probability of False Spring") + ggtitle("A.") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459208492649012"="1950-1983",
                              "0.544414297170614"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459208492649012"="1950-1983",
                             "0.544414297170614"="1984-2016"))
elev<-ggpredict(berndvrfinal, terms=c("elev.z", "cc.z"))
#write.csv(elev, file="elevpredict_dvr.csv", row.names=FALSE)
#elev<-read.csv("~/Documents/git/regionalrisk/analyses/output/elevpredict_dvr.csv", header=TRUE)
#elev$group<-as.character(elev$group)
elev.p<- ggplot(elev, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Elevation") + 
  ylab("Probability of False Spring") + ggtitle("C.") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459201306681485"="1950-1983",
                              "0.544422816636883"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459201306681485"="1950-1983",
                             "0.544422816636883"="1984-2016"))
mat<-ggpredict(berndvrfinal, terms=c("mat.z", "cc.z"))
#write.csv(mat, file="~/Documents/git/regionalrisk/analyses/output/matpredict_dvr.csv", row.names=FALSE)
#mat<-read.csv("~/Documents/git/regionalrisk/analyses/output/matpredict_dvr.csv", header=TRUE)
#mat$group<-as.character(mat$group)
mat.p<- ggplot(mat, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Mean Spring Temperature") + 
  ylab("Probability of False Spring") + ggtitle("B.") + theme_classic() + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459201306681485"="1950-1983",
                              "0.544422816636883"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459201306681485"="1950-1983",
                             "0.544422816636883"="1984-2016"))
dist<-ggpredict(berndvrfinal, terms=c("dist.z", "cc.z"))
#write.csv(dist, file="~/Documents/git/regionalrisk/analyses/output/distpredict_dvr.csv", row.names=FALSE)
#dist<-read.csv("~/Documents/git/regionalrisk/analyses/output/distpredict_dvr.csv", header=TRUE)
#dist$group<-as.character(dist$group)
dist.p<- ggplot(dist, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Distance from Coast") + ylab("Probability of False Spring") + 
  ggtitle("D.") + theme_classic() + #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459201306681485"="1950-1983",
                              "0.544422816636883"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459201306681485"="1950-1983",
                             "0.544422816636883"="1984-2016"))

quartz()
ggarrange(nao.p, mat.p, elev.p, dist.p, ncol=2, nrow=2)


############ Model Output
brms<-as.data.frame(tidy(bernsnewdvr, prob = 0.5))
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
estimates<-c("NAO Index", "Mean Spring Temperature", "Elevation", "Distance from Coast", "Space Parameter", "Climate Change",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change", "Elevation x \nClimate Change",
             "Distance from Coast \nx Climate Change", "Space Parameter \nx Climate Change")
estimates<-rev(estimates)
regrisk.dvr<-ggplot(bb, aes(x=lower, xend=upper, y=Jvar, yend=Jvar, col=as.factor(species))) +
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
        legend.text.align = 0) + coord_cartesian(ylim=c(1,11), xlim=c(-2, 1))
#scale_size_manual(values=c(3, 1, 1, 1, 1, 1, 1, 1, 1, 1), name="Species",
#                 labels=c("1"=expression(paste(italic("Aesculus hippocastanum"))),
#                         "2"=expression(paste(italic("Alnus glutinosa"))),
#                        "3"=expression(paste(italic("Betula lenta"))),
#                       "4"=expression(paste(italic("Fagus sylvatica"))),
#                      "5"=expression(paste(italic("Fraxinus excelsior"))),
#                     "6"=expression(paste(italic("Quercus robur"))),
#                    "0"="Overall Effects"))
quartz()
regrisk.dvr

brms<-as.data.frame(tidy(bernsfive,robust = TRUE))
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
estimates<-c("NAO Index", "Mean Spring Temperature", "Distance from Coast", "Elevation", "Space Parameter", "Climate Change",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change",
             "Distance from Coast \nx Climate Change", "Elevation x \nClimate Change", "Space Parameter \nx Climate Change")
estimates<-rev(estimates)
regrisk.five<-ggplot(bb, aes(x=lower, xend=upper, y=Jvar, yend=Jvar, col=as.factor(species))) +
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
        text=element_text(family="sans"), legend.position = "none") + coord_cartesian(ylim=c(1,11))
#scale_size_manual(values=c(3, 1, 1, 1, 1, 1, 1, 1, 1, 1), name="Species",
#                 labels=c("1"=expression(paste(italic("Aesculus hippocastanum"))),
#                         "2"=expression(paste(italic("Alnus glutinosa"))),
#                        "3"=expression(paste(italic("Betula lenta"))),
#                       "4"=expression(paste(italic("Fagus sylvatica"))),
#                      "5"=expression(paste(italic("Fraxinus excelsior"))),
#                     "6"=expression(paste(italic("Quercus robur"))),
#                    "0"="Overall Effects"))
quartz()
regrisk.five


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
estimates<-c("NAO Index", "Mean Spring Temperature", "Elevation", "Distance from Coast", "Space Parameter", "Climate Change",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change", "Elevation x \nClimate Change",
             "Distance from Coast \nx Climate Change", "Space Parameter \nx Climate Change")
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
        legend.text.align = 0)  + #ggtitle("Original Parameters") +
  coord_cartesian(xlim=c(-2, 1), ylim=c(1,11))
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

ggarrange(regrisk, regrisk.five, regrisk.dvr, ncol=3)

