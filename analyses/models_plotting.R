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
#library(sjmisc)
#library(sjPlot)
library(ggeffects)
library(broom)

setwd("~/Documents/git/regionalrisk/analyses/output")
bb <- read.csv("fs_newspace_orig.csv", header=TRUE)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)

load("~/Documents/git/regionalrisk/orig_full.Rdata")

##### Interaction Plots code

#####################################################################################################
################### Original - predictors with species ##############################################

#naosp<- ggpredict(orig.full, terms = c("nao.z", "species"), ci.lvl=0.5) 
#write.csv(naosp, file="~/Documents/git/regionalrisk/analyses/output/naosp_predicted.csv", row.names = FALSE)
#write.csv(naosp, file="~/Documents/git/regionalrisk/analyses/output/naosp_predicted_50.csv", row.names = FALSE)
naosp0<-read.csv("~/Documents/git/regionalrisk/analyses/output/naosp_predicted_90.csv", header=TRUE)
naosp0$group<-ifelse(naosp0$group=="BETPEN", "aaBETPEN", naosp0$group)
naosp0$group<-ifelse(naosp0$group=="FRAEXC", "zFRAEXC", naosp0$group)
naosp0$x <- (naosp0$x)*sd(bb$nao)*2 + mean(bb$nao)
naosp.p<-ggplot(naosp0, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("NAO Index") + 
  ylab("Probability of False Spring") + ggtitle("D.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  coord_cartesian(ylim=c(0,0.4)) +
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
                             "QUEROB"=expression(paste(italic("Quercus robur")))))

#elevsp<- ggpredict(orig.full, terms = c("elev.z", "species"), ci.lvl=0.9) 
#write.csv(elevsp, file="~/Documents/git/regionalrisk/analyses/output/elevsp_predicted.csv", row.names = FALSE)
#write.csv(elevsp, file="~/Documents/git/regionalrisk/analyses/output/elevsp_predicted_90.csv", row.names = FALSE)
elevsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/elevsp_predicted_90.csv", header=TRUE)
elevsp$group<-ifelse(elevsp$group=="BETPEN", "aaBETPEN", elevsp$group)
elevsp$group<-ifelse(elevsp$group=="FRAEXC", "zFRAEXC", elevsp$group)
elevsp$x <- ((elevsp$x)*sd(bb$elev)*2) + mean(bb$elev)
elevsp.p<-ggplot(elevsp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Elevation (m)") + 
  ylab("Probability of False Spring") + ggtitle("C.") + theme_classic() + theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  coord_cartesian(ylim=c(0,1)) +
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
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) 
#matsp<- ggpredict(orig.full, terms = c("mat.z", "species"), ci.lvl = 0.5) 
#write.csv(matsp, file="~/Documents/git/regionalrisk/analyses/output/matsp_predicted.csv", row.names = FALSE)
#write.csv(matsp, file="~/Documents/git/regionalrisk/analyses/output/matsp_predicted_50.csv", row.names = FALSE)
matsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/matsp_predicted_90.csv", header=TRUE)
matsp$group<-ifelse(matsp$group=="BETPEN", "aaBETPEN", matsp$group)
matsp$group<-ifelse(matsp$group=="FRAEXC", "zFRAEXC", matsp$group)
matsp$x <- (matsp$x)*sd(bb$mst)*2 + mean(bb$mst)
matsp.p<-ggplot(matsp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Mean Spring \nTemperature (°C)") + 
  ylab("Probability of False Spring") + ggtitle("A.") + theme_classic()+ theme(legend.position = "none") + 
  scale_y_continuous(expand = c(0, 0)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  coord_cartesian(ylim=c(0,1)) +
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
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) 
#load("~/Documents/git/regionalrisk/orig_full.Rdata")
#spacesp<- ggpredict(orig.full, terms = c("dist.z", "species"), ci.lvl = 0.9) 
#write.csv(spacesp, file="~/Documents/git/regionalrisk/analyses/output/spacesp_predicted.csv", row.names = FALSE)
#write.csv(spacesp, file="~/Documents/git/regionalrisk/analyses/output/spacesp_predicted_90.csv", row.names = FALSE)
spacesp<-read.csv("~/Documents/git/regionalrisk/analyses/output/spacesp_predicted_90.csv", header=TRUE)
spacesp$group<-ifelse(spacesp$group=="BETPEN", "aaBETPEN", spacesp$group)
spacesp$group<-ifelse(spacesp$group=="FRAEXC", "zFRAEXC", spacesp$group)
spacesp$x <- (spacesp$x)*sd(bb$distkm)*2 + mean(bb$distkm)
spacesp.p<-ggplot(spacesp, aes(x=x, y=predicted))+ geom_line(aes(col=group)) + xlab("Distance from Coast (km)") + ylab("Probability of False Spring") + 
  ggtitle("B.") + scale_y_continuous(expand = c(0, 0)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_classic() + theme(legend.position = "none") + 
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
                             "QUEROB"=expression(paste(italic("Quercus robur"))))) 
#ccsp<- ggpredict(orig.full, terms = c("cc.z", "species"), ci.lvl = 0.9) 
#write.csv(ccsp, file="~/Documents/git/regionalrisk/analyses/output/ccsp_predicted_90.csv", row.names = FALSE)
#write.csv(ccsp, file="~/Documents/git/regionalrisk/analyses/output/ccsp_predicted_50.csv", row.names = FALSE)
ccsp<-read.csv("~/Documents/git/regionalrisk/analyses/output/ccsp_predicted_90.csv", header=TRUE)
ccsp$group<-ifelse(ccsp$group=="BETPEN", "aaBETPEN", ccsp$group)
ccsp$group<-ifelse(ccsp$group=="FRAEXC", "zFRAEXC", ccsp$group)
ccsp$x <- ifelse(ccsp$x <0, 0, 1)
ccsp.p<-ggplot(ccsp, aes(x=x, y=predicted))+ geom_point(aes(col=group))+ geom_line(aes(col=group), linetype="dotted") + 
  xlab("Climate Change") + ylab("Probability of False Spring") + ggtitle("E.") + 
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(breaks=c(0,1)) +
  #geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  coord_cartesian(ylim=c(0,0.4))  + theme(legend.key = element_rect(fill="transparent")) +
  theme_classic() + theme(legend.position = "none") + 
  theme(legend.text.align = 0) +
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
                             "QUEROB"=expression(paste(italic("Quercus robur")))))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(ccsp.p)

quartz()
g1<-grid.arrange(matsp.p, spacesp.p, elevsp.p, ncol=3, widths=c(1.5, 1.5, 1.5))
g2<-grid.arrange(naosp.p, ccsp.p, mylegend, ncol=3, widths=c(1.5,1.5,1))
grid.arrange(g1, g2, nrow=2, heights=c(1.5, 1))


############################# Now interaction plots with CC ##################################
colz <- colorRampPalette(brewer.pal(9,"Set1"))(2)
colz<-rev(colz)
#nao<-ggpredict(orig.full, terms=c("nao.z", "cc.z"), ci.lvl = 0.5)
#write.csv(nao, file="naopredict.csv", row.names=FALSE)
#write.csv(nao, file="naopredict_50.csv", row.names=FALSE)
nao<-read.csv("~/Documents/git/regionalrisk/analyses/output/naopredict_50.csv", header=TRUE)
nao$group<-as.character(nao$group)
nao$x <- (nao$x)*sd(bb$nao)*2 + mean(bb$nao)
nao.p<- ggplot(nao, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("NAO") + 
  ylab("Probability of \nFalse Spring") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,1)) + ggtitle("D.") +
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459191120180519"="1950-1983",
                              "0.544434894155798"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459191120180519"="1950-1983",
                             "0.544434894155798"="1984-2016"))
#elev<-ggpredict(orig.full, terms=c("elev.z", "cc.z"), ci.lvl=0.5)
#write.csv(elev, file="elevpredict.csv", row.names=FALSE)
#write.csv(elev, file="elevpredict_50.csv", row.names=FALSE)
elev<-read.csv("~/Documents/git/regionalrisk/analyses/output/elevpredict_50.csv", header=TRUE)
elev$group<-as.character(elev$group)
elev$x <- (elev$x)*sd(bb$elev)*2 + mean(bb$elev)
elev.p<- ggplot(elev, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Elevation (m)") + 
  ylab("Probability of False Spring") + ggtitle("") + theme_classic() + theme(legend.position = "none") +
  #scale_y_continuous(expand = c(0, 0)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  coord_cartesian(ylim=c(0,1)) + ggtitle("C.") +
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459191120180519"="1950-1983",
                              "0.544434894155798"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459191120180519"="1950-1983",
                             "0.544434894155798"="1984-2016"))
#mat<-ggpredict(orig.full, terms=c("mat.z", "cc.z"), ci.lvl=0.5)
#write.csv(mat, file="~/Documents/git/regionalrisk/analyses/output/matpredict.csv", row.names=FALSE)
#write.csv(mat, file="~/Documents/git/regionalrisk/analyses/output/matpredict_50.csv", row.names=FALSE)
mat<-read.csv("~/Documents/git/regionalrisk/analyses/output/matpredict_50.csv", header=TRUE)
mat$group<-as.character(mat$group)
mat$x <- (mat$x)*sd(bb$mst)*2 + mean(bb$mst)
mat.p<- ggplot(mat, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Mean Spring Temperature (°C)") + 
  ylab("Probability of False Spring") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  coord_cartesian(ylim=c(0,1)) + ggtitle("A.") +
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459191120180519"="1950-1983",
                              "0.544434894155798"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459191120180519"="1950-1983",
                             "0.544434894155798"="1984-2016"))
#dist<-ggpredict(orig.full, terms=c("dist.z", "cc.z"), ci.lvl=0.5)
#write.csv(dist, file="~/Documents/git/regionalrisk/analyses/output/distpredict.csv", row.names=FALSE)
#write.csv(dist, file="~/Documents/git/regionalrisk/analyses/output/distpredict_50.csv", row.names=FALSE)
dist<-read.csv("~/Documents/git/regionalrisk/analyses/output/distpredict_50.csv", header=TRUE)
dist$group<-as.character(dist$group)
dist$x <- (dist$x)*sd(bb$distkm)*2 + mean(bb$distkm)
dist.p<- ggplot(dist, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("Distance from Coast (km)") + ylab("Probability of \nFalse Spring") + 
  ggtitle("B.") + theme_classic() + #scale_y_continuous(expand = c(0, 0)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, col=group, fill=group), linetype=0, alpha=0.4) +
  coord_cartesian(ylim=c(0,1)) + theme(legend.position = "none") +
  scale_color_manual(name="Climate Change", values=colz,
                     labels=c("-0.459191120180519"="1950-1983",
                              "0.544434894155798"="1984-2016")) +
  scale_fill_manual(name="Climate Change", values=colz,
                    labels=c("-0.459191120180519"="1950-1983",
                             "0.544434894155798"="1984-2016"))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(dist.p)

quartz()
g1<-ggarrange(mat.p, dist.p, ncol=2, widths=c(1.5,1.5))
g2<-grid.arrange(elev.p, nao.p, mylegend, ncol=3, widths=c(1, 1, 0.5))
grid.arrange(g1, g2, nrow=2, heights=c(1.5, 1))


########################################################################################
###################### NOW FOR THE MODEL Plots #########################################
########################################################################################
setwd("~/Documents/git/regionalrisk")
load("orig_full.Rdata")
load("dvr_full.Rdata")
load("five_full.Rdata")
load("fullleaf_full.Rdata")

if(FALSE){
modorig<-as.data.frame(tidy(orig.full, prob=0.9))
names(modorig)<-c("term", "estimate", "error", "10%", "90%")
modorig50<-as.data.frame(tidy(orig.full, prob=0.5))
names(modorig50)<-c("term", "estimate", "error", "25%", "75%")
modorig <- full_join(modorig, modorig50)
#modorig25<-as.data.frame(tidy(orig.full, prob=0.25))
#names(modorig25)<-c("term", "estimate", "error", "25%", "75%")
#modorig <- full_join(modorig, modorig25)
modorig <- subset(modorig, select=c("term", "estimate", "10%", "25%", "75%", "90%"))
#write.csv(modorig, file="analyses/output/orig_full_modeloutput.csv", row.names=FALSE)
}
modorig <- read.csv("analyses/output/orig_full_modeloutput.csv", header=TRUE)

if(FALSE){
moddvr<-as.data.frame(tidy(dvr.full, prob=0.9))
names(moddvr)<-c("term", "estimate", "error", "10%", "90%")
moddvr50<-as.data.frame(tidy(dvr.full, prob=0.5))
names(moddvr50)<-c("term", "estimate", "error", "25%", "75%")
moddvr <- full_join(moddvr, moddvr50)
#moddvr25<-as.data.frame(tidy(dvr.full, prob=0.25))
#names(moddvr25)<-c("term", "estimate", "error", "25%", "75%")
#moddvr <- full_join(moddvr, moddvr25)
moddvr <- subset(moddvr, select=c("term", "estimate", "10%", "25%", "75%", "90%"))
#write.csv(moddvr, file="analyses/output/dvr_full_modeloutput.csv", row.names=FALSE)
}
moddvr <- read.csv("analyses/output/dvr_full_modeloutput.csv", header=TRUE)

if(FALSE){
modfive<-as.data.frame(tidy(five.full, prob=0.9))
names(modfive)<-c("term", "estimate", "error", "10%", "90%")
modfive50<-as.data.frame(tidy(five.full, prob=0.5))
names(modfive50)<-c("term", "estimate", "error", "25%", "75%")
modfive <- full_join(modfive, modfive50)
#modfive25<-as.data.frame(tidy(five.full, prob=0.25))
#names(modfive25)<-c("term", "estimate", "error", "25%", "75%")
#modfive <- full_join(modfive, modfive25)
modfive <- subset(modfive, select=c("term", "estimate", "10%", "25%", "75%", "90%"))
#write.csv(modfive, file="analyses/output/five_full_modeloutput.csv", row.names=FALSE)
}
modfive <- read.csv("analyses/output/five_full_modeloutput.csv", header=TRUE)

if(FALSE){
modfullleaf<-as.data.frame(tidy(fullleaf.full, prob=0.9))
names(modfullleaf)<-c("term", "estimate", "error", "10%", "90%")
modfullleaf50<-as.data.frame(tidy(fullleaf.full, prob=0.5))
names(modfullleaf50)<-c("term", "estimate", "error", "25%", "75%")
modfullleaf <- full_join(modfullleaf, modfullleaf50)
#modfive25<-as.data.frame(tidy(five.full, prob=0.25))
#names(modfive25)<-c("term", "estimate", "error", "25%", "75%")
#modfive <- full_join(modfive, modfive25)
modfullleaf <- subset(modfullleaf, select=c("term", "estimate", "10%", "25%", "75%", "90%"))
#write.csv(modfullleaf, file="analyses/output/fullleaf_full_modeloutput.csv", row.names=FALSE)
}


### Now to make the plots
modoutput <- modorig #modelhere

modoutput$term <- ifelse(modoutput$term=="b_Intercept", "b_speciesAESHIP", modoutput$term)
modoutput<-modoutput[1:47,]
modoutput$term<-gsub(".*b_","",modoutput$term)
#modoutput$term <- gsub(".*species", "", modoutput$term)

modoutput<-modoutput[!(modoutput$term=="sd_species__nao.z" | modoutput$term=="sd_species__mat.z" | modoutput$term=="sd_species__elev.z"
                       | modoutput$term=="sd_species__dist.z" | modoutput$term=="sd_species__space.z" | modoutput$term=="sd_species__cc.z" | 
                         modoutput$term=="sigma"),]


modoutput$species <- ifelse(grepl("species",modoutput$term),gsub(".*species", "", modoutput$term), modoutput$term)
makeaeship <- c("nao.z", "mat.z", "dist.z", "elev.z", "space.z", "cc.z")
modoutput$species <- ifelse(modoutput$term%in%makeaeship, "AESHIP", modoutput$species)

modoutput$termclean <- gsub(":species.*", "", modoutput$term)

modoutput$estclean <- NA
modoutput$estclean <- ifelse(modoutput$termclean=="mat.z" & modoutput$species != "AESHIP",
                             modoutput$estimate[(modoutput$term=="mat.z")]+
                               modoutput$estimate, modoutput$estimate)
modoutput$estclean <- ifelse(modoutput$termclean=="nao.z" & modoutput$species != "AESHIP",
                             modoutput$estimate[(modoutput$term=="nao.z")]+
                               modoutput$estimate, modoutput$estclean)
modoutput$estclean <- ifelse(modoutput$termclean=="dist.z" & modoutput$species != "AESHIP",
                             modoutput$estimate[(modoutput$term=="dist.z")]+
                               modoutput$estimate, modoutput$estclean)
modoutput$estclean <- ifelse(modoutput$termclean=="elev.z" & modoutput$species != "AESHIP",
                             modoutput$estimate[(modoutput$term=="elev.z")]+
                               modoutput$estimate, modoutput$estclean)
modoutput$estclean <- ifelse(modoutput$termclean=="space.z" & modoutput$species != "AESHIP",
                             modoutput$estimate[(modoutput$term=="space.z")]+
                               modoutput$estimate, modoutput$estclean)
modoutput$estclean <- ifelse(modoutput$termclean=="cc.z" & modoutput$species != "AESHIP",
                             modoutput$estimate[(modoutput$term=="cc.z")]+
                               modoutput$estimate, modoutput$estclean)

modoutput$lowclean <- NA
modoutput$lowclean <- ifelse(modoutput$termclean=="mat.z" & modoutput$species != "AESHIP",
                             modoutput$X10.[(modoutput$term=="mat.z")]+
                               modoutput$X10., modoutput$X10.)
modoutput$lowclean <- ifelse(modoutput$termclean=="nao.z" & modoutput$species != "AESHIP",
                             modoutput$X10.[(modoutput$term=="nao.z")]+
                               modoutput$X10., modoutput$lowclean)
modoutput$lowclean <- ifelse(modoutput$termclean=="dist.z" & modoutput$species != "AESHIP",
                             modoutput$X10.[(modoutput$term=="dist.z")]+
                               modoutput$X10., modoutput$lowclean)
modoutput$lowclean <- ifelse(modoutput$termclean=="elev.z" & modoutput$species != "AESHIP",
                             modoutput$X10.[(modoutput$term=="elev.z")]+
                               modoutput$X10., modoutput$lowclean)
modoutput$lowclean <- ifelse(modoutput$termclean=="space.z" & modoutput$species != "AESHIP",
                             modoutput$X10.[(modoutput$term=="space.z")]+
                               modoutput$X10., modoutput$X10.)
modoutput$lowclean <- ifelse(modoutput$termclean=="cc.z" & modoutput$species != "AESHIP",
                             modoutput$X10.[(modoutput$term=="cc.z")]+
                               modoutput$X10., modoutput$lowclean)

modoutput$highclean <- NA
modoutput$highclean <- ifelse(modoutput$termclean=="mat.z" & modoutput$species != "AESHIP",
                             modoutput$X90.[(modoutput$term=="mat.z")]+
                               modoutput$X90., modoutput$X90.)
modoutput$highclean <- ifelse(modoutput$termclean=="nao.z" & modoutput$species != "AESHIP",
                             modoutput$X90.[(modoutput$term=="nao.z")]+
                               modoutput$X90., modoutput$highclean)
modoutput$highclean <- ifelse(modoutput$termclean=="dist.z" & modoutput$species != "AESHIP",
                             modoutput$X90.[(modoutput$term=="dist.z")]+
                               modoutput$X90., modoutput$highclean)
modoutput$highclean <- ifelse(modoutput$termclean=="elev.z" & modoutput$species != "AESHIP",
                             modoutput$X90.[(modoutput$term=="elev.z")]+
                               modoutput$X90., modoutput$highclean)
modoutput$highclean <- ifelse(modoutput$termclean=="space.z" & modoutput$species != "AESHIP",
                             modoutput$X90.[(modoutput$term=="space.z")]+
                               modoutput$X90., modoutput$highclean)
modoutput$highclean <- ifelse(modoutput$termclean=="cc.z" & modoutput$species != "AESHIP",
                             modoutput$X90.[(modoutput$term=="cc.z")]+
                               modoutput$X90., modoutput$highclean)

modoutput$estavg <- ave(modoutput$estclean, modoutput$termclean)
modoutput$lowavg <- ave(modoutput$lowclean, modoutput$termclean)
modoutput$highavg <- ave(modoutput$highclean, modoutput$termclean)

                    
modoutput$Jvar<-NA
modoutput$Jvar<-ifelse(modoutput$termclean=="nao.z", 8, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="mat.z", 11, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="elev.z", 9, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="dist.z", 10, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="space.z", 7, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="cc.z", 6, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="nao.z:cc.z", 2, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="mat.z:cc.z", 5, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="elev.z:cc.z", 3, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="dist.z:cc.z", 4, modoutput$Jvar)
modoutput$Jvar<-ifelse(modoutput$termclean=="space.z:cc.z", 1, modoutput$Jvar)

modoutput$Jvar <- ifelse(modoutput$species=="ALNGLU", modoutput$Jvar - 0.1, modoutput$Jvar)
modoutput$Jvar <- ifelse(modoutput$species=="BETPEN", modoutput$Jvar - 0.2, modoutput$Jvar)
modoutput$Jvar <- ifelse(modoutput$species=="FAGSYL", modoutput$Jvar - 0.3, modoutput$Jvar)
modoutput$Jvar <- ifelse(modoutput$species=="FRAEXC", modoutput$Jvar - 0.4, modoutput$Jvar)
modoutput$Jvar <- ifelse(modoutput$species=="QUEROB", modoutput$Jvar - 0.5, modoutput$Jvar)


estimates<-c("Mean Spring Temperature", "Distance from Coast", "Elevation", "NAO Index", "Space Parameter", "Climate Change",
             "Mean Spring Temperature \nx Climate Change", "Distance from Coast \nx Climate Change",
             "Elevation x \nClimate Change", "NAO Index x \nClimate Change", "Space Parameter \nx Climate Change")
estimates<-rev(estimates)
modoutput <- modoutput[!is.na(modoutput$Jvar),]


###### VERY CLOSE! NEED TO MAKE MAIN DOTS BIGGER FOR ESTAVG AND THEN SMALLER DIFF COL DOTS FOR EACH SPECIES (ESTCLEAN)#####
regrisk<-ggplot(modoutput, aes(x=X10., xend=X90., y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estavg, y=Jvar), col="black") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(modoutput$termclean)), labels=estimates) +
  xlab("Change in Number of False Springs") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(3,3,1,1), "lines")) +  #+ ggtitle("Original Parameters") +
  coord_cartesian(xlim=c(-1, 1), ylim=c(1,11), clip = 'off') + #ggtitle("A.") 
  annotate("segment", x = 0.05, xend = 1.1, y = 11.75, yend = 11.75, colour = "black", size=0.2, arrow=arrow(length=unit(0.20,"cm"))) +
  annotate("segment", x = -0.05, xend = -1.1, y = 11.75, yend = 11.75, colour = "black", size=0.2, arrow=arrow(length=unit(0.20,"cm"))) +
  annotate("text", x = 0.5, y = 12, colour = "black", size=3, label="More False Spring Risk") +
  annotate("text", x = -0.5, y = 12, colour = "black", size=3, label="Less False Spring Risk") 


quartz()
regrisk


png("analyses/figures/model_output_90_five.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=6,
    height=6, units="in", res = 350 )
grid.draw(regrisk)
dev.off()
