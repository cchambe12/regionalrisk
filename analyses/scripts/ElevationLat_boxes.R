

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)
library(RColorBrewer)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")

bb<-read.csv("output/fs_space_new.csv", header=TRUE)

bb$smelev<-bb$elev
bb$species<-ifelse(bb$species=="BETPEN", "aaBETPEN", bb$species)
bb$species<-ifelse(bb$species=="FRAEXC", "zFRAEXC", bb$species)

for(i in unique(bb$species)){
  elev.foo<-bb[(bb$smelev[bb$species==i]<quantile(bb$smelev[bb$species==i], 0.75) & bb$smelev[bb$species==i]>quantile(bb$smelev[bb$species==i], 0.25)),]
}

bb$smlat<-bb$lat
for(i in unique(bb$species)){
  lat.foo<-bb[(bb$smlat[bb$species==i]<quantile(bb$smlat[bb$species==i], 0.75) & bb$smlat[bb$species==i]>quantile(bb$smlat[bb$species==i], 0.25)),]
}

cols <- colorRampPalette(brewer.pal(8,"Set3"))(6)

elevation<- ggplot(elev.foo, aes(x=species, y=smelev)) + geom_boxplot(aes(fill=as.factor(species), col=as.factor(species))) + theme_classic() +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  scale_color_manual(name="Species", values=cols,
                     labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                              "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                              "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                              "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                              "QUEROB"=expression(paste(italic("Quercus robur"))),
                              "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  scale_y_continuous(expand = c(0, 0)) + ylab("Elevation") + xlab("") +
  theme(text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.position = "none") +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior"))
  
  
  
latitude<- ggplot(lat.foo, aes(x=species, y=smlat)) + geom_boxplot(aes(fill=as.factor(species), col=as.factor(species))) + theme_classic() +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  scale_color_manual(name="Species", values=cols,
                     labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                              "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                              "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                              "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                              "QUEROB"=expression(paste(italic("Quercus robur"))),
                              "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +
  scale_y_continuous(expand = c(0, 0)) + ylab("Latitude") + xlab("") +
  theme(text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.position = "none") +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior"))

quartz()
ggarrange(elevation, latitude)
