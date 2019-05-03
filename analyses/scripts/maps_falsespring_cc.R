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
fsboth$diff <- fsboth$meanfspost - fsboth$meanfspre
fsboth <- fsboth[!is.na(fsboth$diff),]

#Using GGPLOT, plot the Base World Map
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(40,60),xlim=c(5,25)) # create a layer of borders
myPalette <- colorRampPalette(c("red", "blue"))
site <- fsboth[(fsboth$diff<=0.5 & fsboth$diff>-0.5),]
#sc <- scale_colour_gradientn(colours = myPalette(30), limits=c(-1, 1)) ### this is the range of difference in budburst between the two time periods (using the 10% to 90% range)
#a.site <- site [!(site$meanfspre==0 & site$meanfspost==0),]
site <- subset(site, select=c("lat", "long", "diff", "species"))
#a.site<-filter(site, species=="AESHIP")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
#mid<-mean(a.site$diff)
#pal <- c(-1:0 == "red", 0:1 == "blue")
#a.site$color <- ifelse(a.site$diff < 0, "red", "blue")
site <- site[!is.na(site$diff),]
a.site <- site[(site$species=="AESHIP"),]
#a.site$diff <- (a.site$diff + 1)*100
aes <- ggplot() + # data=a.site, aes(x=a.site$long, y=a.site$lat, fill=a.site$diff)
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray86', fill="gray96", size = .2) +  ### This creates the base map
  #geom_polygon(data=a.site, aes(x=a.site$long, y=a.site$lat, fill=a.site$diff)) +  ### this removes extra background features from ggplot2
  stat_density2d(geom="raster") + geom_point(aes(col=diff,  x = long, y = lat),
                                             data = a.site) +
  #geom_tile(aes(color=a.site$diff)) +
  #geom_jitter(width=3,aes(x=a.site$long, y=a.site$lat, color=a.site$diff), alpha=0.4) + 
  #scale_size_area() + ## add in a.site$diff + 1
  scale_color_gradient2(low = "blue", high = "red", mid="white", na.value=NA, limits=c(-.5,0.5)) + theme_classic() +
  coord_cartesian(ylim=c(40,60),xlim=c(5,18))+  ### zooms in on Europe
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey99")) + ### to make sure the continent doesn't blend in with the ocean
  annotate("text",label= "Aesculus \nhippocastanum", col="#CAB1C4", x=5, y=59,fontface="bold.italic", size=3,
           family="Helvetica", hjust=0) + #sc + 
  #scale_color_manual(values=c("red"="red", "blue"="blue"), labels=c("red" = "More freezes after 1984", 
   #                                                    "blue" = "Fewer freezes after 1984")) +
  labs(color="Difference in \nfalse spring risk") + guides(alpha=FALSE)
aes


b.site<-filter(site, species=="BETPEN")
bet <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray86', fill="gray96", size = .2) +  ### This creates the base map
  geom_point(aes(col=diff,  x = long, y = lat),data = b.site) +
scale_color_gradient2(low = "blue", high = "red", mid="white", na.value=NA, limits=c(-.5,0.5)) + theme_classic() +
  coord_cartesian(ylim=c(40,60),xlim=c(5,18))+  ### zooms in on Europe
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey99")) + ### to make sure the continent doesn't blend in with the ocean
  annotate("text",label= "Betula pendula", col="#7FC97F", x=9, y=60,fontface="bold.italic", size=3,
           family="Helvetica") +
  labs(color="Difference in \nfalse spring risk") + guides(alpha=FALSE)

ag.site<-filter(site, species=="ALNGLU")
aln <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray86', fill="gray96", size = .2) +  ### This creates the base map
  geom_point(aes(col=diff,  x = long, y = lat),data = ag.site) +
  scale_color_gradient2(low = "blue", high = "red", mid="white", na.value=NA, limits=c(-.5,0.5)) + theme_classic() +
  coord_cartesian(ylim=c(40,60),xlim=c(5,18))+  ### zooms in on Europe
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey99")) + ### to make sure the continent doesn't blend in with the ocean
  annotate("text",label= "Alnus glutinosa", col="gold2", x=9, y=60,fontface="bold.italic", size=3,
           family="Helvetica") +
  labs(color="Difference in \nfalse spring risk") + guides(alpha=FALSE)

f.site<-filter(site, species=="FAGSYL")
fsyl <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray86', fill="gray96", size = .2) +  ### This creates the base map
  geom_point(aes(col=diff,  x = long, y = lat),data = f.site) +
  scale_color_gradient2(low = "blue", high = "red", mid="white", na.value=NA, limits=c(-.5,0.5)) + theme_classic() +
  coord_cartesian(ylim=c(40,60),xlim=c(5,18))+  ### zooms in on Europe
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey99")) + ### to make sure the continent doesn't blend in with the ocean
  annotate("text",label= "Fagus sylvatica", col="#87A6A6", x=9, y=60,fontface="bold.italic", size=3,
           family="Helvetica") +
  labs(color="Difference in \nfalse spring risk") + guides(alpha=FALSE)

fe.site<-filter(site, species=="FRAEXC")
fra <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray86', fill="gray96", size = .2) +  ### This creates the base map
  geom_point(aes(col=diff,  x = long, y = lat),data = fe.site) +
  scale_color_gradient2(low = "blue", high = "red", mid="white", na.value=NA, limits=c(-.5,0.5)) + theme_classic() +
  coord_cartesian(ylim=c(40,60),xlim=c(5,18))+  ### zooms in on Europe
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey99")) + ### to make sure the continent doesn't blend in with the ocean
  annotate("text",label= "Fraxinus excelsior", col="#BF5B17", x=9, y=60,fontface="bold.italic", size=3,
           family="Helvetica") +
  labs(color="Difference in \nfalse spring risk") + guides(alpha=FALSE)

q.site<-filter(site, species=="QUEROB")
que <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray86', fill="gray96", size = .2) +  ### This creates the base map
  geom_point(aes(col=diff,  x = long, y = lat),data = q.site) +
  scale_color_gradient2(low = "blue", high = "red", mid="white", na.value=NA, limits=c(-.5,0.5)) + theme_classic() +
  coord_cartesian(ylim=c(40,60),xlim=c(5,18))+  ### zooms in on Europe
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey99")) + ### to make sure the continent doesn't blend in with the ocean
  annotate("text",label= "Quercus robur", col="#CB1788", x=9, y=60,fontface="bold.italic", size=3,
           family="Helvetica") +
  labs(color="Difference in \nfalse spring risk") + guides(alpha=FALSE)

g_legend<-function(a.gplot){ ### remember to rerun que with - legend.position = "none" - commented out
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(que)

quartz()
g1<-ggarrange(bet, aes, aln, fsyl, que, fra, ncol=3, nrow=2) ### before running this, need to remove comment from legend.position = "none" and rerun que
g2<-grid.arrange(mylegend)
mappies<-grid.arrange(g1, g2, ncol=2, widths=c(2.5, 0.75)) ### This places the two plots side by side but puts the legend in the middle

library(grid)
setwd("~/Documents/git/regionalrisk/analyses")
png("figures/falsespring_diff.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=8,
    height=5, units="in", res = 350 )
grid.draw(mappies)
dev.off()


