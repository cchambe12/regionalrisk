### Additional Plots for Regional Risk
## Looking at MAT and NAO plus others
## 8 June 2018 - Cat

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)
library(brms)


# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")

bb<-read.csv("output/regrisk.cleaned.csv", header=TRUE)
dxx<-read.csv("output/fs_matspring.csv", header=TRUE)
x<-read.csv("output/fs_bb_sitedata.csv", header = TRUE)

dxx$fs.yr<-ave(dxx$fs, dxx$year, FUN=sum)
dxx$fs.yrspp<-ave(dxx$fs, dxx$species, dxx$year, FUN=sum)
dxx$spp.prop<-NA
dxx$spp.sites<-as.numeric(ave(dxx$lat.long, dxx$year, dxx$species, FUN=length))
dxx$spp.prop<-ifelse(dxx$species=="AESHIP", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="ALNGLU", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="BETPEN", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="FAGSYL", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="FRAEXC", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="QUEROB", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.ave<-ave(dxx$spp.prop, dxx$species, FUN=median)

dxx$num.sites<-as.numeric(ave(dxx$lat.long, dxx$year, FUN=length))
dxx$fs.prop<-dxx$fs.yr/dxx$num.sites

dxx$fs.ave<-ave(dxx$fs.prop, FUN=median)

x<-x%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
x<-dplyr::select(x, species, year, bb, bb.yr, lat, long, elev)
x<-x[!duplicated(x),]

dxx<-subset(dxx, select=c("species", "year", "lat", "long", "fs.prop", "spp.prop", "num.sites"))

df<-inner_join(bb, dxx)
df<-inner_join(df, x)
df<-df[!duplicated(df),]

### Some plots!
ggplot(df, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)), stat="smooth", method="auto")

df$mat<-as.integer(round(df$sp.temp, digits=0))
df$matx<-NA
df$matx<-ifelse(df$mat<=-10, 1, df$matx)
df$matx<-ifelse(df$mat>-10&df$mat<=-6, 2, df$matx)
df$matx<-ifelse(df$mat>-6&df$mat<=-2, 3, df$matx)
df$matx<-ifelse(df$mat>-2&df$mat<=2, 4, df$matx)
df$matx<-ifelse(df$mat>2&df$mat<=6, 5, df$matx)
df$matx<-ifelse(df$mat>6&df$mat<=10, 6, df$matx)
df$matx<-ifelse(df$mat>10, 7, df$matx)
df$matx<-as.integer(df$matx)

df$fs<-ave(df$fs.count, df$matx)
df$fs<-as.integer(round(df$fs, digits=0))

ggplot(df, aes(x=fs.count, y=sp.temp)) + geom_bar(aes(col=as.factor(cc)), position="dodge")

### Just using the cleaned bb dataset - ELEVATION
aeship<-subset(bb, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                       panel.background = element_blank(), 
                                                                       axis.line = element_line(colour = "black"), legend.key=element_blank(),
                                                                       plot.margin = unit(c(2,2,2,2), "lines"),
                                                                       plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

alnglu<-subset(bb, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                panel.background = element_blank(), 
                                                                axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                plot.margin = unit(c(2,2,2,2), "lines"),
                                                                plot.title=element_text(colour = "orangered1"),legend.position = "none") +
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

betpen<-subset(bb, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Betula pendula")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "orange3")) +
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

fagsyl<-subset(bb, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fagus sylvatica"))))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "sienna2"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

fraexc<-subset(bb, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank(), 
                                                                   axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                   plot.margin = unit(c(2,2,2,2), "lines"),
                                                                   plot.title=element_text(colour = "green4"), legend.position = "none")+
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

querob<-subset(bb, species=="QUEROB")
qrob<-ggplot(querob, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") + 
  ggtitle(expression(paste(italic("Quercus robur")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                              panel.background = element_blank(), 
                                                              axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                              plot.margin = unit(c(2,2,2,2), "lines"),
                                                              plot.title=element_text(colour = "purple2"))+
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))


ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)


#### Now for Mean spring temp
#aeship<-subset(bb, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                       panel.background = element_blank(), 
                                                                       axis.line = element_line(colour = "black"), legend.key=element_blank(),
                                                                       plot.margin = unit(c(2,2,2,2), "lines"),
                                                                       plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#alnglu<-subset(bb, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                panel.background = element_blank(), 
                                                                axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                plot.margin = unit(c(2,2,2,2), "lines"),
                                                                plot.title=element_text(colour = "orangered1"), legend.position = "none") +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#betpen<-subset(bb, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Betula pendula")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "orange3")) +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

#fagsyl<-subset(bb, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fagus sylvatica"))))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "sienna2"), legend.position = "none") +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#fraexc<-subset(bb, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank(), 
                                                                   axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                   plot.margin = unit(c(2,2,2,2), "lines"),
                                                                   plot.title=element_text(colour = "green4"), legend.position = "none")+
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#querob<-subset(bb, species=="QUEROB")
qrob<-ggplot(querob, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") + 
  ggtitle(expression(paste(italic("Quercus robur")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                              panel.background = element_blank(), 
                                                              axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                              plot.margin = unit(c(2,2,2,2), "lines"),
                                                              plot.title=element_text(colour = "purple2"))+
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

quartz()
ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)


### Now for NAO
#aeship<-subset(bb, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                       panel.background = element_blank(), 
                                                                       axis.line = element_line(colour = "black"), legend.key=element_blank(),
                                                                       plot.margin = unit(c(2,2,2,2), "lines"),
                                                                       plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#alnglu<-subset(bb, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                panel.background = element_blank(), 
                                                                axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                plot.margin = unit(c(2,2,2,2), "lines"),
                                                                plot.title=element_text(colour = "orangered1"),legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#betpen<-subset(bb, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Betula pendula")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "orange3")) +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

#fagsyl<-subset(bb, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fagus sylvatica"))))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "sienna2"), legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#fraexc<-subset(bb, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank(), 
                                                                   axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                   plot.margin = unit(c(2,2,2,2), "lines"),
                                                                   plot.title=element_text(colour = "green4"), legend.position = "none")+
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#querob<-subset(bb, species=="QUEROB")
qrob<-ggplot(querob, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") + 
  ggtitle(expression(paste(italic("Quercus robur")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                              panel.background = element_blank(), 
                                                              axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                              plot.margin = unit(c(2,2,2,2), "lines"),
                                                              plot.title=element_text(colour = "purple2"))+
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))


quartz()
ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)

## Now for Space
#aeship<-subset(bb, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                       panel.background = element_blank(), 
                                                                       axis.line = element_line(colour = "black"), legend.key=element_blank(),
                                                                       plot.margin = unit(c(2,2,2,2), "lines"),
                                                                       plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#alnglu<-subset(bb, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                panel.background = element_blank(), 
                                                                axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                plot.margin = unit(c(2,2,2,2), "lines"),
                                                                plot.title=element_text(colour = "orangered1"),legend.position = "none") +
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#betpen<-subset(bb, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Betula pendula")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "orange3")) +
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

#fagsyl<-subset(bb, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fagus sylvatica"))))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "sienna2"), legend.position = "none") +
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#fraexc<-subset(bb, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank(), 
                                                                   axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                   plot.margin = unit(c(2,2,2,2), "lines"),
                                                                   plot.title=element_text(colour = "green4"), legend.position = "none")+
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#querob<-subset(bb, species=="QUEROB")
qrob<-ggplot(querob, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") + 
  ggtitle(expression(paste(italic("Quercus robur")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                              panel.background = element_blank(), 
                                                              axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                              plot.margin = unit(c(2,2,2,2), "lines"),
                                                              plot.title=element_text(colour = "purple2"))+
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))


ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)


#### Not separated by Species
bb$fs.el<-round(bb$elev, digits=-1)
bb$fs.el<-ave(bb$fs.count, bb$fs.el)
elev<-ggplot(bb, aes(x=elev, y=fs.el)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1800), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"), name="Before or After 1983") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("before", "after"), name="Before or After 1983") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

bb$fs.mat<-round(bb$sp.temp, digits=0)
bb$fs.mat<-ave(bb$fs.count, bb$fs.mat)
mat<-ggplot(bb, aes(x=sp.temp, y=fs.mat)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"), name="Before or After 1983") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("before", "after"), name="Before or After 1983") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))

bb$fs.n<-round(bb$m.index, digits=2)
bb$fs.n<-ave(bb$fs.count, bb$fs.n)
nao<-ggplot(bb, aes(x=m.index, y=fs.n)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"), name="Before or After 1983") + 
geom_jitter(alpha=0.3, aes(shape=as.factor(cc))) +
scale_shape_manual(values=c(3, 5), labels=c("before", "after"), name="Before or After 1983") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
scale_y_continuous(expand = c(0, 0))

bb$fs.sp<-round(bb$space, digits=-1)
bb$fs.sp<-ave(bb$fs.count, bb$fs.sp)
space<-ggplot(bb, aes(x=space, y=fs.sp)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(-50, 100), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"), name="Before or After 1983") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("before", "after"), name="Before or After 1983") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))


quartz()
ggarrange(elev, mat, nao, space, ncol=2, nrow=2)



#bb$fs.yr<-round(bb$space)

mround <- function(x,base){ 
  base*round(x/base) 
} 

bb$fs.yr<-mround(bb$year, 5) 
bb$fs.yr<-ave(bb$fs.count, bb$fs.yr)
#bb$yr<-mround(bb$year, 5)
bb$matx<-bb$sp.temp+13.61348
bb$matx<-ave(bb$matx, bb$year)
bbx<-bb%>%dplyr::select(year, fs.yr, matx, cc)
bbx<-bbx[!duplicated(bbx),]
prop<-subset(dxx, select=c(year, fs.prop))
prop<-prop[!duplicated(prop),]
bbx<-inner_join(bbx, prop)
year<-ggplot(bbx, aes(x=year, y=fs.prop)) + ylab("Number of False Springs") + 
  geom_point(aes(shape=as.factor(cc))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(1950, 2016), ylim=c(0, .5)) + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"), name="Before or After 1983") +
  geom_line(aes(y=matx/60), col="blue", stat="smooth", alpha=0.6, method="loess") + scale_y_continuous(sec.axis = sec_axis(~.*60, name="Mean Spring Temperature"))  +
  scale_shape_manual(values=c(3, 5), labels=c("before", "after"), name="Before or After 1983") + guides(shape = guide_legend(override.aes = list(alpha=1))) #+
  #geom_line(aes(col=as.factor(cc)),stat="smooth",method="auto") + xlab("Year")

















