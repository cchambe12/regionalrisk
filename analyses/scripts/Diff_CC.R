rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")

b<-read.csv("output/regrisk.cleaned.csv", header=TRUE)

b$cc<-ifelse(b$cc==0, "before", "after")

bb<-b
bb$fs<-ave(bb$fs.count, bb$species, bb$cc)
bb$fs.sd<-ave(bb$fs.count, bb$species, bb$cc, FUN=sd)/sqrt(length(unique(bb$fs)))
bb<-dplyr::select(bb, species, cc, fs, fs.sd)
bb<-bb[!duplicated(bb),]
bb$cc.sd<-paste(bb$cc, "se", sep="_")
dm<-bb%>%dplyr::select(species, cc, fs)%>%spread(cc, fs)
ds<-bb%>%dplyr::select(species, cc.sd, fs.sd)%>%spread(cc.sd, fs.sd)
bx<-inner_join(dm, ds)
bx$diff<-bx$after-bx$before
bx$diff.sd<-sqrt((bx$after_se)^2+(bx$before_se)^2) 

bx$species<-ifelse(bx$species=="AESHIP", "Aesculus hippocastanum", bx$species)
bx$species<-ifelse(bx$species=="ALNGLU", "Alnus glutinosa", bx$species)
bx$species<-ifelse(bx$species=="BETPEN", "Betula pendula", bx$species)
bx$species<-ifelse(bx$species=="FAGSYL", "Fagus sylvatica", bx$species)
bx$species<-ifelse(bx$species=="FRAEXC", "Fraxinus excelsior", bx$species)
bx$species<-ifelse(bx$species=="QUEROB", "Quercus robur", bx$species)

diff<-ggplot(bx, aes(x=factor(species), y=diff)) + geom_point() + 
  geom_linerange(aes(ymin=diff-diff.sd, ymax=diff+diff.sd), alpha=0.3) + 
  ylab(expression(Delta*" in Number of False Springs \n Before and After 1983")) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, vjust=0.5), axis.text=element_text(size=10)) +
  scale_x_discrete(limits= c("Betula pendula", "Alnus glutinosa", "Aesculus hippocastanum", "Fagus sylvatica",
                             "Quercus robur", "Fraxinus excelsior"))
plot(diff)



