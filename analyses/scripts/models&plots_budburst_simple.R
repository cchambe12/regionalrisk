## Cat - 1 May 2019
# Working on budburst information for before and after CC
##### Then to make a series of maps showing difference in budburst before and after CC

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
#library(ggplot2)
#library(egg)
#library(RColorBrewer)
library(dplyr)
#library(maptools)


setwd("~/Documents/git/regionalrisk")


bb <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)


leafout <- read.csv("analyses/output/BBdata.csv", header=TRUE)
leafout <- subset(leafout, select=c("lat", "long", "year", "species", "bb"))

bbbycc <- left_join(bb, leafout)
bbbycc$cc <- ifelse(bbbycc$year<=1983, "before", "after")
bbbycc$meanleafout <- ave(bbbycc$bb, bbbycc$species, bbbycc$cc)
meanleafoutdates <- subset(bbbycc, select=c("meanleafout", "species", "cc"))
meanleafoutdates <- meanleafoutdates[!duplicated(meanleafoutdates),]

locc <- lm(bb ~ species*cc, data=bbbycc)

confinlocc <- as.data.frame(confint(locc, levels = c(0.025)))
confinlocc$parameter <- rownames(confinlocc)
coeflocc <- as.data.frame(coef(locc))
coeflocc$parameter <- rownames(coeflocc)

bbchange <- full_join(confinlocc, coeflocc)


bbchange[8, 1] <- bbchange$`2.5 %`[8] + bbchange$`2.5 %`[7] + bbchange$`2.5 %`[1] + + bbchange$`2.5 %`[2]
bbchange[8, 2] <- bbchange$`97.5 %`[8] + bbchange$`97.5 %`[7] + bbchange$`97.5 %`[1] + + bbchange$`97.5 %`[2]
bbchange[8, 4] <- bbchange$`coef(locc)`[8] + bbchange$`coef(locc)`[7] + bbchange$`coef(locc)`[1] + + bbchange$`coef(locc)`[2]
bbchange[9, 1] <- bbchange$`2.5 %`[9] + bbchange$`2.5 %`[7] + bbchange$`2.5 %`[1] + + bbchange$`2.5 %`[3]
bbchange[9, 2] <- bbchange$`97.5 %`[9] + bbchange$`97.5 %`[7] + bbchange$`97.5 %`[1] + + bbchange$`97.5 %`[3]
bbchange[9, 4] <- bbchange$`coef(locc)`[9] + bbchange$`coef(locc)`[7] + bbchange$`coef(locc)`[1] + + bbchange$`coef(locc)`[3]
bbchange[10, 1] <- bbchange$`2.5 %`[10] + bbchange$`2.5 %`[7] + bbchange$`2.5 %`[1] + + bbchange$`2.5 %`[4]
bbchange[10, 2] <- bbchange$`97.5 %`[10] + bbchange$`97.5 %`[7] + bbchange$`97.5 %`[1] + + bbchange$`97.5 %`[4]
bbchange[10, 4] <- bbchange$`coef(locc)`[10] + bbchange$`coef(locc)`[7] + bbchange$`coef(locc)`[1] + + bbchange$`coef(locc)`[4]
bbchange[11, 1] <- bbchange$`2.5 %`[11] + bbchange$`2.5 %`[7] + bbchange$`2.5 %`[1] + + bbchange$`2.5 %`[5]
bbchange[11, 2] <- bbchange$`97.5 %`[11] + bbchange$`97.5 %`[7] + bbchange$`97.5 %`[1] + + bbchange$`97.5 %`[5]
bbchange[11, 4] <- bbchange$`coef(locc)`[11] + bbchange$`coef(locc)`[7] + bbchange$`coef(locc)`[1] + + bbchange$`coef(locc)`[5]
bbchange[12, 1] <- bbchange$`2.5 %`[12] + bbchange$`2.5 %`[7] + bbchange$`2.5 %`[1] + + bbchange$`2.5 %`[6]
bbchange[12, 2] <- bbchange$`97.5 %`[12] + bbchange$`97.5 %`[7] + bbchange$`97.5 %`[1] + + bbchange$`97.5 %`[6]
bbchange[12, 4] <- bbchange$`coef(locc)`[12] + bbchange$`coef(locc)`[7] + bbchange$`coef(locc)`[1] + + bbchange$`coef(locc)`[6]

bbchange[2:7, 1] <- bbchange$`2.5 %`[2:7] + bbchange$`2.5 %`[1]
bbchange[2:7, 2] <- bbchange$`97.5 %`[2:7] + bbchange$`97.5 %`[1]
bbchange[2:7, 4] <- bbchange$`coef(locc)`[2:7] + bbchange$`coef(locc)`[1]


rownames(bbchange) <- c("Aesculus hippocastanum (1984-2016)", "Alnus glutinosa (1984-2016)", "Betula pendula (1984-2016)", 
                        "Fagus sylvatica (1984-2016)", "Fraxinus excelsior (1984-2016)", "Quercus robur (1984-2016)", 
                        "Aesculus hippocastanum (1951-1983)", "Alnus glutinosa (1951-1983)", "Betula pendula (1951-1983)", 
                        "Fagus sylvatica (1951-1983)", "Fraxinus excelsior (1951-1983)", "Quercus robur (1951-1983)")
bbchange$parameter <- NULL
bbchange$leafout <- bbchange$`coef(locc)`
bbchange$`coef(locc)`<-NULL
bbchange <- subset(bbchange, select=c("leafout", "2.5 %", "97.5 %"))

#write.csv(bbchange, file="analyses/output/changeinbb.csv", row.names=TRUE)


if(FALSE){
### Now let's try and make those maps..
bbbefore <- bbbycc[(bbbycc$cc=="before"),]
bbbefore$meanbbpre <- ave(bbbefore$bb, bbbefore$lat.long, bbbefore$species)
bbbefore <- subset(bbbefore, select=c("lat", "long", "meanbbpre", "species"))
bbbefore <- bbbefore[!duplicated(bbbefore),]

bbafter <- bbbycc[(bbbycc$cc=="after"),]
bbafter$meanbbpost <- ave(bbafter$bb, bbafter$lat.long, bbafter$species)
bbafter <- subset(bbafter, select=c("lat", "long", "meanbbpost", "species"))
bbafter <- bbafter[!duplicated(bbafter),]

bbboth <- full_join (bbbefore, bbafter)
checkuk <- bbboth[(bbboth$lat>=50 & bbboth$lat<=58 & bbboth$long>=-10 & bbboth$long<=5),]
bbboth$diff <- bbboth$meanbbpost - bbboth$meanbbpre
bbboth <- bbboth[!is.na(bbboth$diff),]

#Using GGPLOT, plot the Base World Map
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35)) # create a layer of borders
#myPalette <- colorRampPalette(c("red", "yellow"))
site <- bbboth[(bbboth$diff>=-12 & bbboth$diff<=4),]
#sc <- scale_colour_gradientn(colours = myPalette(30), limits=c(-12, 4)) ### this is the range of difference in budburst between the two time periods (using the 10% to 90% range)
a.site<-filter(site, species=="AESHIP")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(a.site$diff)
aes <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) + ### This creates the base map
  geom_jitter(width=3,aes(x=a.site$long, y=a.site$lat, color=a.site$diff), size=0.8, alpha=0.4) + theme_classic() + ### this removes extra background features from ggplot2
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))+  ### zooms in on Europe
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95")) + ### to make sure the continent doesn't blend in with the ocean
  annotate("text",label= "Aesculus \nhippocastanum", col="#CAB1C4", x=-12, y=68,fontface="bold.italic", size=3,
           family="Helvetica", hjust=0) + #sc + 
  scale_color_gradient2(midpoint=mid, low="red", mid="orange",
                        high="blue", space ="Lab" ) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

ag.site<-filter(site, species=="ALNGLU")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(ag.site$diff)
aln <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=ag.site$long, y=ag.site$lat, color=ag.site$diff), size=0.8, alpha=0.4) + theme_classic() +
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
  scale_color_gradient2(midpoint=mid, low="red", mid="orange",
                        high="blue", space ="Lab" ) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

b.site<-filter(site, species=="BETPEN")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(b.site$diff)
bet <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=b.site$long, y=b.site$lat, color=b.site$diff), size=0.8, alpha=0.4) + theme_classic() +
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
  scale_color_gradient2(midpoint=mid, low="red", mid="orange",
                        high="blue", space ="Lab" ) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

f.site<-filter(site, species=="FAGSYL")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(f.site$diff)
syl <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=f.site$long, y=f.site$lat, color=f.site$diff), size=0.8, alpha=0.4) + theme_classic() +
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
  scale_color_gradient2(midpoint=mid, low="red", mid="orange",
                        high="blue", space ="Lab" ) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

fe.site<-filter(site, species=="FRAEXC")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(fe.site$diff)
fra <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=fe.site$long, y=fe.site$lat, color=fe.site$diff), size=0.8, alpha=0.4) + theme_classic() +
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
  scale_color_gradient2(midpoint=mid, low="red", mid="orange",
                        high="blue", space ="Lab" ) +
  labs(color="Difference in \nday of budburst") + ggtitle("")

q.site<-filter(site, species=="QUEROB")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mid<-mean(q.site$diff)

###### Need to comment out legend.position="none" on line 209 for legend (lines 223-229) and then remove comment for g1<-ggarrange...(line 232)
que <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=q.site$long, y=q.site$lat, color=q.site$diff), size=0.8, alpha=0.4) + 
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
  scale_color_gradient2(midpoint=mid, low="red", mid="orange",
                        high="blue", space ="Lab" ) +
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
}

