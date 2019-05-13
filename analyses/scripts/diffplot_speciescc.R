## 9 May 2019 - Cat
# Hoping to disentangle species differences before and after CC
# map didn't really work...

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses")

bb <- read.csv("output/fs_newspace_orig.csv", header=TRUE)

sites <- subset(bb, select=c("lat.long", "cc"))
sites <- sites[!duplicated(sites),]
tt <- as.data.frame(table(sites$lat.long))
tt <- tt[(tt$Freq==2),]

goodsites <- unique(tt$Var1)

bb <- bb[(bb$lat.long%in%goodsites),]

bbpre <- bb[(bb$cc==0),]
bbpost <- bb[(bb$cc==1),]
bbpre$fsmeanpre <- ave(bbpre$fs, bbpre$species)
bbpre$fspre.sd<-ave(bbpre$fs, bbpre$species, bbpre$cc, FUN=sd)/sqrt(length(unique(bbpre$fsmeanpre)))
bbpost$fsmeanpost <- ave(bbpost$fs, bbpost$species)
bbpost$fspost.sd<-ave(bbpost$fs, bbpost$species, bbpost$cc, FUN=sd)/sqrt(length(unique(bbpost$fsmeanpost)))

bbpre <- subset(bbpre , select=c("species", "fsmeanpre", "fspre.sd"))
bbpre <- bbpre[!duplicated(bbpre),]
bbpost <- subset(bbpost , select=c("species", "fsmeanpost", "fspost.sd"))
bbpost <- bbpost[!duplicated(bbpost),]

bbdiff <- full_join(bbpre, bbpost)

bbdiff$diff<-bbdiff$fsmeanpost-bbdiff$fsmeanpre
bbdiff$diff.sd<-bbdiff$fspost.sd-bbdiff$fspre.sd


bbdiff$species<-ifelse(bbdiff$species=="BETPEN", "aaBETPEN", bbdiff$species)
bbdiff$species<-ifelse(bbdiff$species=="FRAEXC", "zFRAEXC", bbdiff$species)

bbdiff$ymin <- bbdiff$fsmeanpre-bbdiff$fspre.sd
bbdiff$ymax <- bbdiff$fsmeanpre+bbdiff$fspre.sd
bbdiff$xmin <- bbdiff$fsmeanpost-bbdiff$fspost.sd
bbdiff$xmax <- bbdiff$fsmeanpost+bbdiff$fspost.sd

bbdiff$diff.labels <- as.numeric(as.factor(bbdiff$diff))

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
diff <- ggplot(bbdiff, aes(x=fsmeanpre, y=fsmeanpost, col=species), alpha=2) + 
  geom_point(aes(x=fsmeanpre, y=fsmeanpost, size=as.factor(diff.labels)), shape=21) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species), vjust=2) + 
  xlab("False spring risk (1951-1983)") + 
  ylab("False spring risk (1984-2016)") + 
  scale_color_manual(name="Species", values=cols,
                     labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                              "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                              "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                              "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                              "QUEROB"=expression(paste(italic("Quercus robur"))),
                              "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  scale_size_manual(name=expression(Delta*" in false spring risk"), values=c(1,2,3,4,5,6),
                     labels=c(-0.02, -0.017, -0.011, 0.059, 0.061, 0.069)) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  coord_cartesian(xlim=c(-0.05,0.5), ylim=c(-0.05,0.5))

quartz()
plot(diff)


