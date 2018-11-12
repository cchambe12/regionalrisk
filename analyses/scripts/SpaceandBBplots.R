## Started 2 November 2018 ##
## By Cat ##

## Working on extra plots for raw data - showing MST relationship to BB by species ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)
library(brms)
library(grid)
library(gridExtra)


# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


bb<-read.csv("output/fs_newdvr_space.csv", header=TRUE)
xx<-read.csv("output/BBdata_dvr.csv", header=TRUE)

dxx<-bb
#dxx$fs<-ifelse(dxx$fs.count>0, 1, 0)
#dxx$fs.yr<-ave(dxx$fs.count, dxx$year, FUN=sum)
#dxx$fs.yrspp<-ave(dxx$fs.count, dxx$species, dxx$year, FUN=sum)
### Sites per species -
#length(unique(xx$PEP_ID[xx$species=="AESHIP"])) # lat.long: 10158 - eigen: 10158 - PEP: 10496
#length(unique(xx$PEP_ID[xx$species=="ALNGLU"])) # lat.long: 6775 - eigen: 6775 - PEP: 6915
#length(unique(xx$PEP_ID[xx$species=="BETPEN"])) # lat.long: 10139 - eigen: 10137 - PEP: 10465
#length(unique(xx$PEP_ID[xx$species=="FAGSYL"])) # lat.long: 9099 - eigen: 9097 - PEP: 9363
#length(unique(xx$PEP_ID[xx$species=="FRAEXC"])) # lat.long: 7327 - eigen: 7325 - PEP: 7503
#length(unique(xx$lat.long[xx$species=="QUEROB"])) # lat.long: 8831 - eigen: 8809 - PEP: 9044
#dxx$spp.prop<-NA
#dxx$spp.sites<-as.numeric(ave(dxx$lat.long, dxx$year, dxx$species, FUN=length))
#dxx$spp.prop<-ifelse(dxx$species=="AESHIP", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
#dxx$spp.prop<-ifelse(dxx$species=="ALNGLU", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
#dxx$spp.prop<-ifelse(dxx$species=="BETPEN", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
#dxx$spp.prop<-ifelse(dxx$species=="FAGSYL", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
#dxx$spp.prop<-ifelse(dxx$species=="FRAEXC", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
#dxx$spp.prop<-ifelse(dxx$species=="QUEROB", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
#dxx$spp.ave<-ave(dxx$spp.prop, dxx$species, FUN=median)

#dxx$num.sites<-as.numeric(ave(dxx$lat.long, dxx$year, FUN=length))
#dxx$fs.prop<-dxx$fs.yr/dxx$num.sites

#dxx$fs.ave<-ave(dxx$fs.prop, FUN=median)

xx<-inner_join(xx, dxx)

xx$bb.yr<-ave(xx$bb, xx$species, xx$year)
#xx$x<-scale(xx$bb.yr, center = FALSE, scale = TRUE)
#xx$sx<-xx$bb.yr-90.2963

#xx$tempsite<-ave(xx$mst, xx$lat.long, xx$year)

aeship<-subset(xx, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("C. Aesculus hippocastanum \n(Avg Day of Budburst = 95.29)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
  axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
  plot.title=element_text(colour = "#FDD98D", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

alnglu<-subset(xx, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("B. Alnus glutinosa \n(Avg Day of Budburst = 98.32)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#CAB1C4", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

betpen<-subset(xx, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("A. Betula pendula \n(Avg Day of Budburst = 97.31)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),plot.title=element_text(colour = "#7FC97F", size = 9),
        axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") + scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

fagsyl<-subset(xx, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("D. Fagus sylvatica \n(Avg Day of Budburst = 113.83)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#87A6A6", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

fraexc<-subset(xx, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("F. Fraxinus excelsior \n(Avg Day of Budburst = 117.80)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#BF5B17", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

querob<-subset(xx, species=="QUEROB")
qrob<-ggplot(querob, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("E. Quercus robur \n(Avg Day of Budburst = 111.74)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#CB1788", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

quartz()
mstplots<-ggarrange(ahip, bpen, aglu, qrob, fsyl, fexc, ncol=3, nrow=2)

png("figures/MSTBB_bySpp.png", 
    width=8,
    height=5, units="in", res = 350 )
grid.draw(mstplots)
dev.off()


###### Now let's compare to Tmin!
tm<-read.csv("output/tminprep_boxplots_dvr.csv", header=TRUE)

tm<-full_join(xx, tm)
tm<-na.omit(tm)

aeship<-subset(tm, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=year, y=Tmin)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Minimum Temperature from \nBudburst to Leafout") + 
  ggtitle(expression(paste(italic("C. Aesculus hippocastanum \n(Avg Day of Budburst = 95.29)")))) + 
  coord_cartesian(ylim=c(-5,20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#FDD98D", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,30,60,90,120, 150)))

alnglu<-subset(tm, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=year, y=Tmin)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Minimum Temperature from \nBudburst to Leafout") + 
  ggtitle(expression(paste(italic("B. Alnus glutinosa \n(Avg Day of Budburst = 98.32)")))) + 
  coord_cartesian(ylim=c(-5,20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#CAB1C4", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,30,60,90,120, 150)))

betpen<-subset(tm, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=year, y=Tmin)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Minimum Temperature from \nBudburst to Leafout") + 
  ggtitle(expression(paste(italic("A. Betula pendula \n(Avg Day of Budburst = 97.31)")))) + 
  coord_cartesian(ylim=c(-5,20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),plot.title=element_text(colour = "#7FC97F", size = 9),
        axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") + scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,30,60,90,120, 150)))

fagsyl<-subset(tm, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=year, y=Tmin)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Minimum Temperature from \nBudburst to Leafout") + 
  ggtitle(expression(paste(italic("D. Fagus sylvatica \n(Avg Day of Budburst = 113.83)")))) + 
  coord_cartesian(ylim=c(-5,20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#87A6A6", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,30,60,90,120, 150)))

fraexc<-subset(tm, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=year, y=Tmin)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Minimum Temperature from \nBudburst to Leafout") + 
  ggtitle(expression(paste(italic("F. Fraxinus excelsior \n(Avg Day of Budburst = 117.80)")))) + 
  coord_cartesian(ylim=c(-5,20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#BF5B17", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,30,60,90,120, 150)))

querob<-subset(tm, species=="QUEROB")
qrob<-ggplot(querob, aes(x=year, y=Tmin)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Minimum Temperature from \nBudburst to Leafout") + 
  ggtitle(expression(paste(italic("E. Quercus robur \n(Avg Day of Budburst = 111.74)")))) + 
  coord_cartesian(ylim=c(-5,20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#CB1788", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,30,60,90,120,150)))

quartz()
tminplot<-ggarrange(ahip, bpen, aglu, qrob, fsyl, fexc, ncol=3, nrow=2)


png("figures/TminBB_bySpp.png", 
    width=8,
    height=5, units="in", res = 350 )
grid.draw(tminplot)
dev.off()


