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
library(ggeffects)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")

bb <- read.csv("output/fs_newspace_orig.csv", header=TRUE)
#bb <- read.csv("output/fs_newspace_fullleaf.csv", header=TRUE)

if(FALSE){
sites <- subset(bb, select=c("lat.long", "cc"))
sites <- sites[!duplicated(sites),]
tt <- as.data.frame(table(sites$lat.long))
tt <- tt[(tt$Freq==2),]

goodsites <- unique(tt$Var1)
}

########################
#### get the data
mat<-read.csv("output/BBdata.csv", header=TRUE) #### mean day of budburst is 104.88
#mat<-read.csv("output/BBdata_dvr.csv", header=TRUE)
#mat<-read.csv("output/bb_fullleaf.csv", header=TRUE)
#mat$lat.long <- paste(mat$lat, mat$long)
#mat <- mat[(mat$lat.long%in%goodsites),]

load("~/Documents/git/regionalrisk/bbmod.Rdata")
bbccsp<- ggpredict(bb.mod, terms = c("cc", "species"), ci.lvl=0.5) 

bbccsp$group <- as.character(bbccsp$group)

bbccsp$species <- NA
bbccsp$species<-ifelse(bbccsp$group=="BETPEN", "aaBETPEN", bbccsp$group)
bbccsp$species<-ifelse(bbccsp$group=="FRAEXC", "zFRAEXC", bbccsp$species)

mat$bb.avg<-ave(mat$bb, mat$species)

mat$cc<-ifelse(mat$year<1984, 0, 1)
mat$species<-ifelse(mat$species=="BETPEN", "aaBETPEN", mat$species)
mat$species<-ifelse(mat$species=="FRAEXC", "zFRAEXC", mat$species)

bbccsp$est.conv <-  (bbccsp$predicted/4)/(2)
bbccsp$low.conv <-  (bbccsp$conf.low/4)/(2)
bbccsp$up.conv <-  (bbccsp$conf.high/4)/(2)
bbccsp$x <- ifelse(bbccsp$x<=0, 0, 1)

bbccsp<-subset(bbccsp, select=c("x", "predicted", "conf.low", "conf.high","species", "est.conv", "low.conv", "up.conv"))
colnames(bbccsp) <- c("cc", "est", "lower", "upper",  "species", "est.conv", "low.conv", "up.conv")

pre<-subset(mat, mat$cc==0)

post<-subset(mat, mat$cc==1)

plus<-full_join(pre, post)
plus <- full_join(plus, bbccsp)
cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
budburst<- ggplot(plus, aes(x=species, y=bb, alpha=cc)) + geom_boxplot(aes(alpha=as.factor(cc), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  scale_fill_manual(name="Species", values=cols,
                      labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                               "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                               "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                               "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "QUEROB"=expression(paste(italic("Quercus robur"))),
                               "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  scale_color_manual(name="Species", values=c("grey48","black", cols),
                     labels=c("0"="1951-1983", "1"="1984-2016",
                              "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                              "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                              "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                              "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                              "QUEROB"=expression(paste(italic("Quercus robur"))),
                              "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) +  
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), #legend.position = "none",
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), 
        axis.title.x = element_blank(), 
        axis.text.x.bottom = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "none") + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "cQUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Day of Budburst") + coord_cartesian(ylim=c(50,165)) + 
  #geom_hline(yintercept=mean(mat$bb), linetype="dotted", col="black") +
  #annotate("text", x = 5.75, y = 245, label = "Before 1984", family="Helvetica", size=3, fontface="bold") +
  scale_alpha_manual(name="Climate Change", values=c(0.2, 0.7),
                       labels=c("0"="1951-1983", "1"="1984-2016")) +
  geom_point(aes(x=as.factor(species), alpha=as.factor(cc), y=est, group=cc,
                 col=as.factor(cc)),
             position = position_dodge(width=0.75), size=1) + 
  geom_linerange(aes(x=as.factor(species), alpha=as.factor(cc), ymin=lower, ymax=upper, group=cc,
                     col=as.factor(cc)), position = position_dodge(width=0.75)) +
  ggtitle("A.") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)




###### Tmin boxplots now...
tm<-read.csv("output/tminprep_boxplots.csv", header=TRUE) #### mean tmin is 7.54
#tm<-read.csv("output/tminprep_boxplots_fullleaf.csv", header=TRUE) 
#tm<-read.csv("output/tminprep_boxplots_dvr.csv", header=TRUE)
#tm$lat.long <- paste(tm$lat, tm$long)
#tm <- tm[(tm$lat.long%in%goodsites),]

load("~/Documents/git/regionalrisk/tminmod.Rdata")
tmccsp<- ggpredict(tmin.mod, terms = c("cc", "species"), ci.lvl=0.5) 

tmccsp$group <- as.character(tmccsp$group)

tmccsp$species <- NA
tmccsp$species<-ifelse(tmccsp$group=="BETPEN", "aaBETPEN", tmccsp$group)
tmccsp$species<-ifelse(tmccsp$group=="FRAEXC", "zFRAEXC", tmccsp$species)

tm$cc<-ifelse(tm$year<1984, 0, 1)
tm$species<-ifelse(tm$species=="BETPEN", "aaBETPEN", tm$species)
tm$species<-ifelse(tm$species=="FRAEXC", "zFRAEXC", tm$species)
#tm$species<-ifelse(tm$species=="ALNGLU", "abALNGLU", tm$species)

tmccsp$est.conv <-  (tmccsp$predicted/4)/(2)
tmccsp$low.conv <-  (tmccsp$conf.low/4)/(2)
tmccsp$up.conv <-  (tmccsp$conf.high/4)/(2)
tmccsp$x <- ifelse(tmccsp$x<=0, 0, 1)


tmccsp<-subset(tmccsp, select=c("x", "predicted", "conf.low", "conf.high","species", "est.conv", "low.conv", "up.conv"))
colnames(tmccsp) <- c("cc", "est", "lower", "upper",  "species", "est.conv", "low.conv", "up.conv")


tm<-tm[!is.na(tm$Tmin),]



pret<-subset(tm, tm$cc==0)

postt<-subset(tm, tm$cc==1)

plust<-full_join(pret, postt)

plust <- full_join(plust, tmccsp)
cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
tmin<- ggplot(plust, aes(x=species, y=Tmin, alpha=cc)) + geom_boxplot(aes(alpha=as.factor(cc), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  scale_color_manual(name="Species", values=c("grey48","black", cols),
                     labels=c("0"="1951-1983", "1"="1984-2016",
                              "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                              "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                              "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                              "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                              "QUEROB"=expression(paste(italic("Quercus robur"))),
                              "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), #legend.position = "none",
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), 
        axis.title.x = element_blank(), 
        legend.position = "none",
        axis.text.x.bottom = element_blank(), axis.ticks.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Minimum Temperature \nfrom Budburst to Leafout") + coord_cartesian(ylim=c(0,15)) + 
  #geom_hline(yintercept=mean(plust$Tmin), linetype="dotted", col="black") +
  #annotate("text", x = 5.75, y = 245, label = "Before 1984", family="Helvetica", size=3, fontface="bold") +
  scale_alpha_manual(name="Climate Change", values=c(0.2, 0.7),
                     labels=c("0"="1951-1983", "1"="1984-2016")) +
  geom_point(aes(x=as.factor(species), alpha=as.factor(cc), y=est, group=cc,
                 col=as.factor(cc)),
             position = position_dodge(width=0.75), size=1) + 
  geom_linerange(aes(x=as.factor(species), alpha=as.factor(cc), ymin=lower, ymax=upper, group=cc,
                     col=as.factor(cc)), position = position_dodge(width=0.75)) +
  ggtitle("B.") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

### And finally... False springs!
###### 14 August 2019 - Cat
# Adding to final panel model results to help reader interpret differences between raw data and considering all climatic and geographical factors
#modoutput <- read.csv("output/ccsp_predicted_90.csv", header=TRUE)

load("~/Documents/git/regionalrisk/fssimpmod.Rdata")
fsccsp<- ggpredict(fssimp.mod, terms = c("cc", "species"), ci.lvl=0.5) 

fsccsp$group <- as.character(fsccsp$group)

fsccsp$species <- NA
fsccsp$species<-ifelse(fsccsp$group=="BETPEN", "aaBETPEN", fsccsp$group)
fsccsp$species<-ifelse(fsccsp$group=="FRAEXC", "zFRAEXC", fsccsp$species)

f<-read.csv("output/fs_newspace_orig.csv", header=TRUE)
#f<-read.csv("output/fs_newspace_dvr.csv", header=TRUE)
#f<-read.csv("output/fs_newspace_five.csv", header=TRUE)
#f<-read.csv("output/fs_newspace_fullleaf.csv", header=TRUE)

#f <- f[(f$lat.long%in%goodsites),]

#f$cc<-ifelse(f$year<1980, 0, 1)
f$species<-ifelse(f$species=="BETPEN", "aaBETPEN", f$species)
f$species<-ifelse(f$species=="FRAEXC", "zFRAEXC", f$species)
f<-f[!is.na(f$fs),]
#f$fs<-ifelse(f$fs.count>0, 1, 0)
#f$fs<-ave(f$fs, f$lat.long, f$species, f$cc, FUN=sum)
f$fs<-ave(f$fs, f$lat.long, f$species, f$cc)

fsccsp$est.conv <-  (fsccsp$predicted/4)/(2)
fsccsp$low.conv <-  (fsccsp$conf.low/4)/(2)
fsccsp$up.conv <-  (fsccsp$conf.high/4)/(2)
fsccsp$x <- ifelse(fsccsp$x<=0, 0, 1)


fsccsp<-subset(fsccsp, select=c("x", "predicted", "conf.low", "conf.high","species", "est.conv", "low.conv", "up.conv"))
colnames(fsccsp) <- c("cc", "est", "lower", "upper",  "species", "est.conv", "low.conv", "up.conv")

plusf<-subset(f, select=c(species, cc, fs))
plusf<-plusf[!duplicated(plusf),]

plusf <- full_join(plusf, fsccsp)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)
falsespring<- ggplot(plusf, aes(x=species,alpha=cc, y=fs)) + geom_boxplot(aes(alpha=as.factor(cc), fill=as.factor(species), col=as.factor(species)), outlier.shape=16) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "QUEROB"=expression(paste(italic("Quercus robur"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  scale_color_manual(name="Species", values=c("grey48","black", cols),
                     labels=c("0"="1951-1983", "1"="1984-2016",
                              "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                              "AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                              "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                              "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                              "QUEROB"=expression(paste(italic("Quercus robur"))),
                              "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))))) + 
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=35, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), legend.position = "none",
        plot.margin = unit(c(1.5,1.5, 1.0, 1.5), "lines"),
        axis.title.x = element_blank()) + 
        #axis.text.x.bottom = element_blank(), axis.ticks.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("aaBETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum",
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "zFRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Probability of \nFalse Spring Risk") + coord_cartesian(ylim=c(0, 1)) + 
  #geom_hline(yintercept=7.66, linetype="dotted", col="black") +
  #annotate("text", x = 5.75, y = 245, label = "Before 1984", family="Helvetica", size=3, fontface="bold") +
  scale_alpha_manual(name="Climate Change", values=c(0.2, 0.7),
                     labels=c("0"="1951-1983", "1"="1984-2016")) +
  geom_point(aes(x=as.factor(species), alpha=as.factor(cc), y=est, group=cc,
                 col=as.factor(cc)),
             position = position_dodge(width=0.75), size=1) + 
  geom_linerange(aes(x=as.factor(species), alpha=as.factor(cc), ymin=lower, ymax=upper, group=cc,
                     col=as.factor(cc)), position = position_dodge(width=0.75)) +
  ggtitle("C.") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(falsespring)

quartz()
g1<-ggarrange(budburst, tmin, falsespring, nrow=3)
g2<-grid.arrange(mylegend)
grid.arrange(g1, g2, ncol=2, widths=c(2.5, 0.75))

png("figures/Boxplot_BBTminFS_noDots_modests.png", 
    width=5,
    height=8, units="in", res = 350 )
grid.arrange(g1, g2, ncol=2, widths=c(2.5, 0.75))
dev.off()


## Budburst: took the average day of budburst for each individual and condensed to the 25th-75th quartile
## Tmin: same thing as budburst
## False Spring: took the average probability of risk for each site, species and cc



