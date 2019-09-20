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
library(RColorBrewer)
library(viridis)


# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


#bb<-read.csv("output/fs_newspace_dvr.csv", header=TRUE)
bb<-read.csv("output/fs_newspace_orig.csv", header=TRUE)

xx<-read.csv("output/BBdata.csv", header=TRUE)

pre<-xx[(xx$year<1984),]
pre$bb.avg<-ave(pre$bb, pre$species)

post<-xx[(xx$year>1983),]
post$bb.avg<-ave(post$bb, post$species)

dxx<-bb
xx<-inner_join(xx, dxx)

xx$bb.yr<-ave(xx$bb, xx$species, xx$year)

pref<-xx[(xx$cc==0),]
pref$numfs<-ave(pref$fs.count, pref$species, FUN=sum)

postf<-xx[(xx$cc==1),]
postf$numfs<-ave(postf$fs.count, postf$species, FUN=sum)

aeship<-subset(xx, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("B. Aesculus hippocastanum \n(Avg Day of Budburst = 99.2)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
  axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
  plot.title=element_text(colour = "#CAB1C4", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

alnglu<-subset(xx, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("C. Alnus glutinosa \n(Avg Day of Budburst = 98.9)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "gold2", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

betpen<-subset(xx, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("A. Betula pendula \n(Avg Day of Budburst = 98.8)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),plot.title=element_text(colour = "#7FC97F", size = 9),
        axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") + scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

fagsyl<-subset(xx, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("D. Fagus sylvatica \n(Avg Day of Budburst = 106.7)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#87A6A6", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

fraexc<-subset(xx, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("F. Fraxinus excelsior \n(Avg Day of Budburst = 116.3)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#BF5B17", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

querob<-subset(xx, species=="QUEROB")
qrob<-ggplot(querob, aes(x=year, y=mst)) + geom_point(alpha=0.08) + xlab("Year") + ylab("Mean Spring Temperature") + 
  ggtitle(expression(paste(italic("E. Quercus robur \n(Avg Day of Budburst = 113.0)")))) + 
  coord_cartesian(ylim=c(-5,15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "#CB1788", size = 9),axis.title = element_text(size=9)) +
  geom_line(aes(y=bb.yr/10), col="blue", stat="smooth", size=2, method="auto") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Avg Day of Budburst", labels=c(0,35,70,105,140)))

quartz()
mstplots<-ggarrange(bpen, ahip, aglu, fsyl, qrob, fexc, ncol=3, nrow=2)

png("figures/MSTBB_bySpp.png", 
    width=8,
    height=5, units="in", res = 350 )
grid.draw(mstplots)
dev.off()


###### Now let's compare to Tmin!
tm<-read.csv("output/tminprep_boxplots.csv", header=TRUE)

tm<-full_join(xx, tm)
tm<-na.omit(tm)

tm$x<-scale((tm$bb.yr)^2, center = FALSE, scale = TRUE)

cols <- rep(viridis_pal(option="viridis")(3), times=2)


##### NEED TO CHANGE SCALE OF SECOND Y AXIS!!! ### IDEALLY FROM 80-120 ##########
aeship<-subset(tm, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=year, y=Tmin)) + geom_smooth(method="auto", se=TRUE, level=0.9, col=cols[1]) +  
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + 
  ylab("Minimum temperature \nfrom budburst to leafout (°C)") + xlab("Year") +
  coord_cartesian(ylim=c(4, 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(size = 9),axis.title = element_text(size=9),
        axis.title.y.right = element_text(color = cols[2]),
        axis.text.y.right = element_text(color = cols[2]),
        axis.line.y.right =  element_line(color = cols[2]),
        axis.ticks.y.right =  element_line(color = cols[2]),
        axis.title.y.left = element_text(color=cols[1]),
        axis.text.y.left = element_text(color=cols[1]),
        axis.line.y.left = element_line(color=cols[1]),
        axis.ticks.y.left = element_line(color=cols[1])) +
  geom_smooth(aes(y=bb.yr/15), method="auto", se=TRUE, level=0.9,  col=cols[2]) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name="Avg day of budburst")) + 
  scale_x_continuous(breaks=seq(1950, 2015, by = 10))

alnglu<-subset(tm, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=year, y=Tmin)) + geom_smooth(method="auto", se=TRUE, level=0.9, col=cols[1]) +  
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + 
  ylab("Minimum temperature \nfrom budburst to leafout (°C)") + xlab("Year") +
  coord_cartesian(ylim=c(4, 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(size = 9),axis.title = element_text(size=9),
        axis.title.y.right = element_text(color = cols[2]),
        axis.text.y.right = element_text(color = cols[2]),
        axis.line.y.right =  element_line(color = cols[2]),
        axis.ticks.y.right =  element_line(color = cols[2]),
        axis.title.y.left = element_text(color=cols[1]),
        axis.text.y.left = element_text(color=cols[1]),
        axis.line.y.left = element_line(color=cols[1]),
        axis.ticks.y.left = element_line(color=cols[1])) +
  geom_smooth(aes(y=bb.yr/12), method="auto", se=TRUE, level=0.9,  col=cols[2]) +
  scale_y_continuous(sec.axis = sec_axis(~.*12, name="Avg day of budburst")) +
  scale_x_continuous(breaks=seq(1950, 2015, by = 10))

betpen<-subset(tm, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=year, y=Tmin)) + geom_smooth(method="auto", se=TRUE, level=0.9, col=cols[1]) +  
  ggtitle(expression(paste(italic("Betula pendula")))) + 
  ylab("Minimum temperature \nfrom budburst to leafout (°C)") + xlab("Year") +
  coord_cartesian(ylim=c(4, 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(size = 9),axis.title = element_text(size=9),
        axis.title.y.right = element_text(color = cols[2]),
        axis.text.y.right = element_text(color = cols[2]),
        axis.line.y.right =  element_line(color = cols[2]),
        axis.ticks.y.right =  element_line(color = cols[2]),
        axis.title.y.left = element_text(color=cols[1]),
        axis.text.y.left = element_text(color=cols[1]),
        axis.line.y.left = element_line(color=cols[1]),
        axis.ticks.y.left = element_line(color=cols[1])) +
  geom_smooth(aes(y=bb.yr/12), method="auto", se=TRUE, level=0.9,  col=cols[2]) +
  scale_y_continuous(sec.axis = sec_axis(~.*12, name="Avg day of budburst")) +
  scale_x_continuous(breaks=seq(1950, 2015, by = 10))

fagsyl<-subset(tm, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=year, y=Tmin)) + geom_smooth(method="auto", se=TRUE, level=0.9, col=cols[1]) +  
  ggtitle(expression(paste(italic("Fagus sylvatica")))) + 
  ylab("Minimum temperature \nfrom budburst to leafout (°C)") + xlab("Year") +
  coord_cartesian(ylim=c(4, 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(size = 9),axis.title = element_text(size=9),
        axis.title.y.right = element_text(color = cols[2]),
        axis.text.y.right = element_text(color = cols[2]),
        axis.line.y.right =  element_line(color = cols[2]),
        axis.ticks.y.right =  element_line(color = cols[2]),
        axis.title.y.left = element_text(color=cols[1]),
        axis.text.y.left = element_text(color=cols[1]),
        axis.line.y.left = element_line(color=cols[1]),
        axis.ticks.y.left = element_line(color=cols[1])) +
  geom_smooth(aes(y=bb.yr/12), method="auto", se=TRUE, level=0.9,  col=cols[2]) +
  scale_y_continuous(sec.axis = sec_axis(~.*12, name="Avg day of budburst"))  +
  scale_x_continuous(breaks=seq(1950, 2015, by = 10))

fraexc<-subset(tm, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=year, y=Tmin)) + geom_smooth(method="auto", se=TRUE, level=0.9, col=cols[1]) +  
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + 
  ylab("Minimum temperature \nfrom budburst to leafout (°C)") + xlab("Year") +
  coord_cartesian(ylim=c(4, 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(size = 9),axis.title = element_text(size=9),
        axis.title.y.right = element_text(color = cols[2]),
        axis.text.y.right = element_text(color = cols[2]),
        axis.line.y.right =  element_line(color = cols[2]),
        axis.ticks.y.right =  element_line(color = cols[2]),
        axis.title.y.left = element_text(color=cols[1]),
        axis.text.y.left = element_text(color=cols[1]),
        axis.line.y.left = element_line(color=cols[1]),
        axis.ticks.y.left = element_line(color=cols[1])) +
  geom_smooth(aes(y=bb.yr/12), method="auto", se=TRUE, level=0.9,  col=cols[2]) +
  scale_y_continuous(sec.axis = sec_axis(~.*12, name="Avg day of budburst"))  +
  scale_x_continuous(breaks=seq(1950, 2015, by = 10))

querob<-subset(tm, species=="QUEROB")
qrob<-ggplot(querob, aes(x=year, y=Tmin)) + geom_smooth(method="auto", se=TRUE, level=0.9, col=cols[1]) +  
  ggtitle(expression(paste(italic("Quercus robur")))) + 
  ylab("Minimum temperature \nfrom budburst to leafout (°C)") + xlab("Year") +
  coord_cartesian(ylim=c(4, 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(size = 9),axis.title = element_text(size=9),
        axis.title.y.right = element_text(color = cols[2]),
        axis.text.y.right = element_text(color = cols[2]),
        axis.line.y.right =  element_line(color = cols[2]),
        axis.ticks.y.right =  element_line(color = cols[2]),
        axis.title.y.left = element_text(color=cols[1]),
        axis.text.y.left = element_text(color=cols[1]),
        axis.line.y.left = element_line(color=cols[1]),
        axis.ticks.y.left = element_line(color=cols[1])) +
  geom_smooth(aes(y=bb.yr/12), method="auto", se=TRUE, level=0.9,  col=cols[2]) +
  scale_y_continuous(sec.axis = sec_axis(~.*12, name="Avg day of budburst"))  +
  scale_x_continuous(breaks=seq(1950, 2015, by = 10))


quartz()
tminplot<-ggarrange(ahip, bpen, aglu, qrob, fsyl, fexc, ncol=3, nrow=2)


png("figures/TminBB_bySpp_lines.png", 
    width=8,
    height=5, units="in", res = 350 )
grid.draw(tminplot)
dev.off()


