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
  geom_hline(yintercept=107.59, linetype="dotted", col="black") +
  annotate("text", x = 5.75, y = 245, label = "Before 1984", family="Helvetica", size=3, fontface="bold")

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
  geom_hline(yintercept=101.69, linetype="dotted", col="black") +
  annotate("text", x = 5.75, y = 245, label = "After 1984", family="Helvetica", size=3, fontface="bold")

quartz()
ggarrange(pre.plot, post.plot, ncol=2)

###### Tmin boxplots now...
tm<-read.csv("output/tminprep_boxplots.csv", header=TRUE)

tm$cc<-ifelse(tm$year<1984, 0, 1)
tm$species<-ifelse(tm$species=="BETPEN", "aaBETPEN", tm$species)
tm$species<-ifelse(tm$species=="FRAEXC", "zFRAEXC", tm$species)
tm<-tm[!is.na(tm$Tmin),]
pret<-subset(tm, tm$cc==0)
postt<-subset(tm, tm$cc==1)



cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
pret.plot<- ggplot(pret, aes(x=species, y=Tmin)) + geom_boxplot(aes(col=as.factor(species))) +
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
  #scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Minimum Temperatures \nfrom Budburst to Leafout") + #coord_cartesian(ylim=c(0,250)) + 
  geom_hline(yintercept=6.99, linetype="dotted", col="black") +
  annotate("text", x = 5.75, y = 22, label = "Before 1984", family="Helvetica", size=3, fontface="bold")

postt.plot<- ggplot(postt, aes(x=species, y=Tmin)) + geom_boxplot(aes(col=as.factor(species))) +
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
  #scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  #coord_cartesian(ylim=c(0,250)) + 
  geom_hline(yintercept=7.78, linetype="dotted", col="black") +
  annotate("text", x = 5.75, y = 22, label = "After 1984", family="Helvetica", size=3, fontface="bold")

quartz()
ggarrange(pret.plot, postt.plot, ncol=2)


### And finally... False springs!
f<-read.csv("output/fs_yearsitespp.csv", header=TRUE)

f$cc<-ifelse(f$year<1984, 0, 1)
f$species<-ifelse(f$species=="BETPEN", "aaBETPEN", f$species)
f$species<-ifelse(f$species=="FRAEXC", "zFRAEXC", f$species)
f<-f[!is.na(f$fs.count),]
pref<-subset(f, f$cc==0)
postf<-subset(f, f$cc==1)



cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
pref.plot<- ggplot(pref, aes(x=species, y=fs.count)) + geom_boxplot(aes(col=as.factor(species))) +
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
  ylab("Number of False Springs") + coord_cartesian(ylim=c(0,13)) + 
  geom_hline(yintercept=0.04, linetype="dotted", col="black") +
  annotate("text", x = 5.75, y = 12.5, label = "Before 1984", family="Helvetica", size=3, fontface="bold")

postf.plot<- ggplot(postf, aes(x=species, y=fs.count)) + geom_boxplot(aes(col=as.factor(species))) +
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
  coord_cartesian(ylim=c(0,13)) + 
  geom_hline(yintercept=0.01, linetype="dotted", col="black") +
  annotate("text", x = 5.75, y = 12.5, label = "After 1984", family="Helvetica", size=3, fontface="bold")

quartz()
ggarrange(pref.plot, postf.plot, ncol=2)

ggarrange(pre.plot, post.plot,
          pret.plot, postt.plot,
          pref.plot, postf.plot, ncol=2, nrow=3)






