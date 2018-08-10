### Trying to find best way to plot all of the data...
## Cat - 10 August 2018

## Housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)
library(RColorBrewer)

# Setting Working Directory
setwd("~/Documents/git/regionalrisk/analyses/")

#### Get the Data
bb.stan<-read.csv("output/bb.brm.nointer.csv", header=TRUE)
bb.stan$elev<-bb.stan$sm.elev*100
bb<-bb.stan[sample(nrow(bb.stan),500),]

bb.stan$sp<-NA
bb.stan$sp<-ifelse(bb.stan$species=="BETPEN", "aBETPEN", bb.stan$sp)
bb.stan$sp<-ifelse(bb.stan$species=="ALNGLU", "bALNGLU", bb.stan$sp)
bb.stan$sp<-ifelse(bb.stan$species=="AESHIP", "cAESHIP", bb.stan$sp)
bb.stan$sp<-ifelse(bb.stan$species=="FAGSYL", "dFAGSYL", bb.stan$sp)
bb.stan$sp<-ifelse(bb.stan$species=="QUEROB", "eQUEROB", bb.stan$sp)
bb.stan$sp<-ifelse(bb.stan$species=="FRAEXC", "fFRAEXC", bb.stan$sp)
cols <- colorRampPalette(brewer.pal(8,"Set1"))(6)

quartz()
elev<-ggplot(bb, aes(x=elev, y=fs.count)) + geom_line(aes(linetype=as.factor(cc), x=sm.elev*100, y=fs.count,
                                                          col=sp),
                                                           data=bb.stan, stat="smooth",method="lm") + 
  xlab("Elevation") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(), legend.position="none") +
  coord_cartesian(xlim=c(0, 1400), ylim=c(0, 3)) + 
  scale_linetype_manual(values=c("solid", "dashed"), labels=c("Before 1983", "After 1983"), name="Climate Change") + 
  scale_color_manual(values=cols, labels= c("aBETPEN"=expression(paste(italic("Betula pendula"))),
                                            "bALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                                            "cAESHIP"=expression(paste(italic("Aesculus \nhippocastanum"))),
                                            "dFAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                                            "eQUEROB"=expression(paste(italic("Quercus robur"))),
                                            "fFRAEXC"=expression(paste(italic("Fraxinus excelsior")))), name="Species") +
  #geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="Climate Change") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0)) + ggtitle("A")

mat<-ggplot(bb, aes(x=sp.temp, y=fs.count)) + geom_line(aes(linetype=as.factor(cc), x=sp.temp, y=fs.count,
                                                            col=sp),
                                                        data=bb.stan,stat="smooth",method="lm") +
  xlab("Mean Spring Temperature") + ylab("Average Number \nof False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),legend.text.align = 0) +
  coord_cartesian(xlim=c(-16, 16), ylim=c(0, 3)) + 
  scale_linetype_manual(values=c("solid", "dashed"), labels=c("Before 1983", "After 1983"), name="Climate Change") + 
  scale_color_manual(values=cols, labels= c("aBETPEN"=expression(paste(italic("Betula pendula"))),
                                            "bALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                                            "cAESHIP"=expression(paste(italic("Aesculus \nhippocastanum"))),
                                            "dFAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                                            "eQUEROB"=expression(paste(italic("Quercus robur"))),
                                            "fFRAEXC"=expression(paste(italic("Fraxinus excelsior")))), name="Species") + 
  #geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="Climate Change") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0)) + ggtitle("B") + guides(col=FALSE)

nao<-ggplot(bb, aes(x=m.index, y=fs.count)) + geom_line(aes(linetype=as.factor(cc), x=m.index, y=fs.count,
                                                            col=sp),
                                                             data=bb.stan, stat="smooth",method="lm") + 
  xlab("NAO Index") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(), legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 3)) + 
  scale_linetype_manual(values=c("solid", "dashed"), labels=c("Before 1983", "After 1983"), name="Climate Change") + 
  scale_color_manual(values=cols, labels= c("aBETPEN"=expression(paste(italic("Betula pendula"))),
                                            "bALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                                            "cAESHIP"=expression(paste(italic("Aesculus \nhippocastanum"))),
                                            "dFAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                                            "eQUEROB"=expression(paste(italic("Quercus robur"))),
                                            "fFRAEXC"=expression(paste(italic("Fraxinus excelsior")))), name="Species") +
  #geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="Climate Change") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0)) +ggtitle("C")


space.p<-ggplot(bb, aes(x=space, y=fs.count)) + geom_line(aes(linetype=as.factor(cc), x=space, y=fs.count,
                                                              col=sp),
                                                               data=bb.stan,stat="smooth",method="lm") + 
  xlab("Space") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(), legend.text.align=0) +
  coord_cartesian(xlim=c(-30, 80), ylim=c(0, 3)) + 
  scale_linetype_manual(values=c("solid", "dashed"), labels=c("Before 1983", "After 1983"), name="Climate Change") + 
  scale_color_manual(values=cols, labels= c("aBETPEN"=expression(paste(italic("Betula pendula"))),
                                            "bALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                                            "cAESHIP"=expression(paste(italic("Aesculus \nhippocastanum"))),
                                            "dFAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                                            "eQUEROB"=expression(paste(italic("Quercus robur"))),
                                            "fFRAEXC"=expression(paste(italic("Fraxinus excelsior")))), name="Species") +
  #geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="Climate Change") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0)) +ggtitle("D") + guides(shape=FALSE, linetype=FALSE)


quartz()
ggarrange(elev, mat, nao, space.p, ncol=2, nrow=2)





