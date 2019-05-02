## Cat - 1 May 2019
##### Make a series of maps showing difference in false spring risk before and after CC

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(ggplot2)
library(dplyr)
library(egg)
library(RColorBrewer)
library(maptools)


setwd("~/Documents/git/regionalrisk")

bb <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)

### Now let's try and make those maps..
fsbefore <- bb[(bb$cc==0),]
fsbefore$meanfspre <- ave(fsbefore$fs, fsbefore$lat.long, fsbefore$species)
fsbefore <- subset(fsbefore, select=c("lat", "long", "meanfspre", "species"))
fsbefore <- fsbefore[!duplicated(fsbefore),]

fsafter <- bb[(bb$cc==1),]
fsafter$meanfspost <- ave(fsafter$fs, fsafter$lat.long, fsafter$species)
fsafter <- subset(fsafter, select=c("lat", "long", "meanfspost", "species"))
fsafter <- fsafter[!duplicated(fsafter),]

fsboth <- full_join (fsbefore, fsafter)
fsboth$diff <- fsboth$meanfspre - fsboth$meanfspost
fsboth <- fsboth[!is.na(fsboth$diff),]

#Using GGPLOT, plot the Base World Map
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35)) # create a layer of borders
myPalette <- colorRampPalette(c("red", "blue"))
site <- fsboth[(fsboth$diff<=0.3 | fsboth$diff>0.3),]
sc <- scale_colour_gradientn(colours = myPalette(30), limits=c(-1, 1)) ### this is the range of difference in budburst between the two time periods (using the 10% to 90% range)
a.site <- site [!(site$meanfspre==0 & site$meanfspost==0),]
a.site <- subset(a.site, select=c("lat", "long", "diff", "species"))
#a.site<-filter(site, species=="AESHIP")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
#mid<-mean(a.site$diff)
pal <- c(-1:0 == "red", 0:1 == "blue")
#a.site$color <- ifelse(a.site$diff < 0, "red", "blue")
a.site <- a.site[!is.na(a.site$diff),]
a.site$diff <- a.site$diff + 1
aes <- ggplot() + # data=a.site, aes(x=a.site$long, y=a.site$lat, fill=a.site$diff)
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +  ### This creates the base map
  #geom_polygon(data=a.site, aes(x=a.site$long, y=a.site$lat, fill=a.site$diff)) +  ### this removes extra background features from ggplot2
  #geom_tile(data=a.site, aes(x=a.site$long, y=a.site$lat, fill=a.site$diff)) +
  geom_jitter(width=3,aes(x=a.site$long, y=a.site$lat, color=a.site$diff), alpha=0.4) + scale_size_area() + ## add in a.site$diff + 1
  scale_color_gradient(low = "red", high = "blue", na.value=NA) + theme_classic() +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))+  ### zooms in on Europe
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        #legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95")) + ### to make sure the continent doesn't blend in with the ocean
  annotate("text",label= "Aesculus \nhippocastanum", col="#CAB1C4", x=-12, y=68,fontface="bold.italic", size=3,
           family="Helvetica", hjust=0) + #sc + 
  #scale_color_manual(values=c("red"="red", "blue"="blue"), labels=c("red" = "More freezes after 1984", 
   #                                                    "blue" = "Fewer freezes after 1984")) +
  labs(color="Difference in \nday of budburst") + ggtitle("")
aes

ag.site<-filter(site, species=="ALNGLU")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(ag.site$diff)
ag.site$color <- ifelse(ag.site$diff < 0, "red", "blue")
aln <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=ag.site$long, y=ag.site$lat, color=ag.site$color), size=0.8, alpha=0.4) + theme_classic() +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95"),
        legend.position = "none") +
  annotate("text",label= "Alnus glutinosa", col="gold2", x=1, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + #sc + 
  scale_color_manual(values=c("red"="red", "blue"="blue"), labels=c("red" = "More freezes after 1984", 
                                                       "blue" = "Fewer freezes after 1984")) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

b.site<-filter(site, species=="BETPEN")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(b.site$diff)
b.site$color <- ifelse(b.site$diff < 0, "red", "blue")
bet <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=b.site$long, y=b.site$lat, color=b.site$color), size=0.8, alpha=0.4) + theme_classic() +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95")) +
  annotate("text",label= "Betula pendula", col="#7FC97F", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + #sc + 
  scale_color_manual(values=c("red"="red", "blue"="blue"), labels=c("red" = "More freezes after 1984", 
                                                       "blue" = "Fewer freezes after 1984")) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

f.site<-filter(site, species=="FAGSYL")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(f.site$diff)
f.site$color <- ifelse(f.site$diff < 0, "red", "blue")
syl <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=f.site$long, y=f.site$lat, color=f.site$color), size=0.8, alpha=0.4) + theme_classic() +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95")) +
  annotate("text",label= "Fagus sylvatica", col="#87A6A6", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + #sc +
  scale_color_manual(values=c("red"="red", "blue"="blue"), labels=c("red" = "More freezes after 1984", 
                                                       "blue" = "Fewer freezes after 1984")) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

fe.site<-filter(site, species=="FRAEXC")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(fe.site$diff)
fe.site$color <- ifelse(fe.site$diff < 0, "red", "blue")
fra <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=fe.site$long, y=fe.site$lat, color=fe.site$color), size=0.8, alpha=0.4) + theme_classic() +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95"),
        legend.position = "none") +
  annotate("text",label= "Fraxinus excelsior", col="#BF5B17", x=3, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + #sc + 
  scale_color_manual(values=c("red"="red", "blue"="blue"), labels=c("red" = "More freezes after 1984", 
                                                       "blue" = "Fewer freezes after 1984")) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

q.site<-filter(site, species=="QUEROB")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(q.site$diff)
q.site$color <- ifelse(q.site$diff < 0, "red", "blue")

###### Need to comment out legend.position="none" on line 209 for legend (lines 223-229) and then remove comment for g1<-ggarrange...(line 232)
que <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=q.site$long, y=q.site$lat, color=q.site$color), size=0.8, alpha=0.4) + 
  theme_classic() +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none", ##### Need to comment this line out to get the legend for the "g_legend" function, then remove comment to rerun the plot for arranging the grid
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95")) +
  annotate("text",label= "Quercus robur", col="#CB1788", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + #sc + 
  scale_color_manual(values=c("red"="red", "blue"="blue"), labels=c("red" = "Less freezes after 1984", 
                                                       "blue" = "More freezes after 1984")) +
  labs(color="Difference in \nday of budburst") + ggtitle("")


g_legend<-function(a.gplot){ ### remember to rerun que with - legend.position = "none" - commented out
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(que)

quartz()
g1<-ggarrange(bet, aes, aln, syl, que, fra, ncol=3, nrow=2) ### before running this, need to remove comment from legend.position = "none" and rerun que
g2<-grid.arrange(mylegend)
mappies<-grid.arrange(g1, g2, ncol=2, widths=c(2.5, 0.75)) ### This places the two plots side by side but puts the legend in the middle

setwd("~/Documents/git/regionalrisk/analyses")
png("figures/budburst_diff.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=8,
    height=5, units="in", res = 350 )
grid.draw(mappies)
dev.off()


