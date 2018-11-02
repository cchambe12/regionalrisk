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
for(i in unique(pre$species)){
  pre<-pre[(pre$bb[pre$species==i]<quantile(pre$bb[pre$species==i], 0.75) & pre$bb[pre$species==i]>quantile(pre$bb[pre$species==i], 0.25)),]
}

post<-subset(mat, mat$cc==1)
for(i in unique(post$species)){
  post<-post[(post$bb[post$species==i]<quantile(post$bb[post$species==i], 0.75) & post$bb[post$species==i]>quantile(post$bb[post$species==i], 0.25)),]
}

plus<-full_join(pre, post)
cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
budburst<- ggplot(plus, aes(x=species, y=bb, alpha=cc)) + geom_boxplot(aes(alpha=as.factor(cc), fill=as.factor(species), col=as.factor(species))) +
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
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), #legend.position = "none",
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), 
        axis.title.x = element_blank(), 
        axis.text.x.bottom = element_blank(), axis.ticks.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Day of Budburst") + coord_cartesian(ylim=c(50,165)) + 
  geom_hline(yintercept=106.53, linetype="dotted", col="black") +
  #annotate("text", x = 5.75, y = 245, label = "Before 1984", family="Helvetica", size=3, fontface="bold") +
  scale_alpha_manual(name="Climate Change", values=c(0.2, 0.7),
                       labels=c("0"="1951-1983", "1"="1984-2016")) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)




###### Tmin boxplots now...
tm<-read.csv("output/tminprep_boxplots.csv", header=TRUE)

tm$cc<-ifelse(tm$year<1984, 0, 1)
tm$species<-ifelse(tm$species=="BETPEN", "aaBETPEN", tm$species)
tm$species<-ifelse(tm$species=="FRAEXC", "zFRAEXC", tm$species)
tm<-tm[!is.na(tm$Tmin),]
pret<-subset(tm, tm$cc==0)
#for(i in unique(pret$species)){
 # pret<-pret[(pret$Tmin[pret$species==i]<quantile(pret$Tmin[pret$species==i], 0.75) & pret$Tmin[pret$species==i]>quantile(pret$Tmin[pret$species==i], 0.25)),]
#}

postt<-subset(tm, tm$cc==1)
#for(i in unique(postt$species)){
#  postt<-postt[(postt$Tmin[postt$species==i]<quantile(postt$Tmin[postt$species==i], 0.75) & postt$Tmin[postt$species==i]>quantile(postt$Tmin[postt$species==i], 0.25)),]
#}

plust<-full_join(pret, postt)
cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
tmin<- ggplot(plust, aes(x=species, y=Tmin, alpha=cc)) + geom_boxplot(aes(alpha=as.factor(cc), fill=as.factor(species), col=as.factor(species))) +
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
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), #legend.position = "none",
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), 
        axis.title.x = element_blank(), 
        axis.text.x.bottom = element_blank(), axis.ticks.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Minimum Temperature \nfrom Budburst to Leafout") + coord_cartesian(ylim=c(-5,18)) + 
  geom_hline(yintercept=7.66, linetype="dotted", col="black") +
  #annotate("text", x = 5.75, y = 245, label = "Before 1984", family="Helvetica", size=3, fontface="bold") +
  scale_alpha_manual(name="Climate Change", values=c(0.2, 0.7),
                     labels=c("0"="1951-1983", "1"="1984-2016")) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

### And finally... False springs!
f<-read.csv("output/fs_yearsitespp.csv", header=TRUE)

f$cc<-ifelse(f$year<1980, 0, 1)
f$species<-ifelse(f$species=="BETPEN", "aaBETPEN", f$species)
f$species<-ifelse(f$species=="FRAEXC", "zFRAEXC", f$species)
f<-f[!is.na(f$fs.count),]
f$fs<-ifelse(f$fs.count>0, 1, 0)
f$fs<-ave(f$fs,f$PEP_ID, f$species, f$cc, FUN=sum)

plusf<-subset(f, select=c(species, cc, fs))
plusf<-plusf[!duplicated(plusf),]
cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
falsespring<- ggplot(plusf, aes(x=species,alpha=cc, y=fs)) + geom_boxplot(aes(alpha=as.factor(cc), fill=as.factor(species), col=as.factor(species))) +
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
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=35, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), #legend.position = "none",
        plot.margin = unit(c(1.5,1.5, 1.0, 1.5), "lines"),
        axis.title.x = element_blank()) + 
        #axis.text.x.bottom = element_blank(), axis.ticks.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Number of Years \nwith False Springs") + coord_cartesian(ylim=c(0,25)) + 
  #geom_hline(yintercept=7.66, linetype="dotted", col="black") +
  #annotate("text", x = 5.75, y = 245, label = "Before 1984", family="Helvetica", size=3, fontface="bold") +
  scale_alpha_manual(name="Climate Change", values=c(0.2, 0.7),
                     labels=c("0"="1951-1983", "1"="1984-2016")) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)


quartz()
ggarrange(budburst, tmin, falsespring, ncol=1, nrow=3)

## Budburst: took the average day of budburst for each individual and condensed to the 25th-75th quartile
## Tmin: same thing as budburst
## False Spring: counted the number of false spring years from 1951-1983 and for 1984-2016 for each individual





