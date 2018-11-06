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
#write.csv(nao, file="naopredict.csv", row.names=FALSE)
#nao<-read.csv("~/Documents/git/regionalrisk/analyses/output/naopredict.csv", header=TRUE)
#nao$group<-as.character(nao$group)
nao.p<- ggplot(nao, aes(x=x, y=predicted, col=group)) + geom_line(aes(col=group)) + xlab("NAO") + 
  ylab("Number of False Springs") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
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
  ylab("Number of False Springs") + ggtitle("") + theme_classic() +
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
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
  ylab("Number of False Springs") + ggtitle("") + theme_classic() + theme(legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0,0.4)) + 
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

