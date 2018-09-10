## Started 5 September 2018 ##
## By Cat ##

## Making new boxplots for BB, Tmin, and FS across species ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)
library(RColorBrewer)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data
#xx<-read.csv("output/fs_yearsitespp.csv", header=TRUE)
#xx<-subset(xx, year>1950)

mat<-read.csv("output/BBdata.csv", header=TRUE)

#### 5 September 2018 - not lining up for some reason... need to fix ###

mat$cc<-ifelse(mat$year<1984, 0, 1)
mat$species<-ifelse(mat$species=="BETPEN", "aaBETPEN", mat$species)
mat$species<-ifelse(mat$species=="FRAEXC", "zFRAEXC", mat$species)
pre<-subset(mat, mat$cc==0)
post<-subset(mat, mat$cc==1)


cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
pre.plot<- ggplot(pre, aes(x=species, y=bb)) + geom_boxplot(aes(col=as.factor(species))) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), legend.position = "none",
        plot.margin = unit(c(1.5,0,1.5,1.5), "lines"), axis.title.x = element_blank(), 
        axis.text.x.bottom = element_blank(), axis.ticks.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Day of Budburst") + coord_cartesian(ylim=c(0,250)) + 
  geom_hline(yintercept=100, linetype="dotted", col="black") +
  annotate("text", x = 1.5, y = 240, label = "Before 1984", family="Helvetica", size=3, fontface="bold")

post.plot<- ggplot(post, aes(x=species, y=bb)) + geom_boxplot(aes(col=as.factor(species))) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(1.5,1.5,1.5,-0.2), "lines"), axis.title.x = element_blank(),
        axis.text.x.bottom  = element_blank(), axis.ticks.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  coord_cartesian(ylim=c(0,250)) + 
  geom_hline(yintercept=100, linetype="dotted", col="black") +
  annotate("text", x = 1.5, y = 240, label = "After 1984", family="Helvetica", size=3, fontface="bold")

quartz()
ggarrange(pre.plot, post.plot, ncol=2)


