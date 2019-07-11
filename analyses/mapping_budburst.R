#### Last Modified January 2018 - Cat
## Make a map of all raw data, using help from Nacho
##############################################################################################################
# Script to:
#' * Easy approach to generate maps w/wo ocean background
#'
#'  Ignacio Morales-Castilla
##############################################################################################################

rm(list=ls())
library(ggplot2)
library(dplyr)
library(egg)
library(RColorBrewer)
library(maptools)
library(grid)

if(FALSE){
##Simple approach
## map without background
mapDevice() #create world map shaped window
map("world", fill=TRUE
    ,col="grey65"
    ,boundary=F,interior=F
    ,ylim=c(-60, 65), mar=c(0,0,0,0)
    ,projection='albers',par=c(0,0),wrap=T
    ,resolution=1,border="lightgrey",myborder=0)

## map with blue background
map("world", fill=TRUE
    ,col="grey65"
    ,bg="deepskyblue4"
    ,boundary=F,interior=F
    ,ylim=c(30, 65),xlim=c(-5,35), mar=c(0,0,0,0)
    ,projection='albers',par=c(0,0),wrap=T
    ,resolution=0,border="white",myborder=0)
}

setwd("~/Documents/git/regionalrisk/analyses")
d<-read.csv("output/BBdata.csv", header=TRUE)
d<-d[(d$bb>=0),]
d$bb.space<-ave(d$bb, d$PEP_ID, d$species)
d$cc<-ifelse(d$year<=1983, 0, 1)
d$bb.avg<-ave(d$bb, d$species)

#Using GGPLOT, plot the Base World Map
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35)) # create a layer of borders
site<-d%>%dplyr::select(lat, long, bb.space, species)
site<-site[!duplicated(site),]
myPalette <- colorRampPalette(brewer.pal(9, "YlOrRd")) #### Gives us a heat map look
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(50, 150)) ### this is the range of budburst data we have
a.site<-filter(site, species=="AESHIP")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
aes <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) + ### This creates the base map
  geom_jitter(width=3,aes(x=a.site$long, y=a.site$lat, color=a.site$bb), size=0.8, alpha=0.4) + theme_classic() + ### this removes extra background features from ggplot2
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
  annotate("text",label= "B. Aesculus \nhippocastanum", col="#CAB1C4", x=-12, y=68,fontface="bold.italic", size=3,
           family="Helvetica", hjust=0) + sc + 
  labs(color="Day of Budburst") + ggtitle("")

### These sections were used to download shapefiles in case I wanted to tweak the output in QGIS
#spg<-a.site
#coordinates(spg)<- ~long+lat
#proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
#coords<-spTransform(spg, CRS("+proj=longlat"))
#shapefile(coords, "output/aeship_dvr.shp")

ag.site<-filter(site, species=="ALNGLU")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
aln <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=ag.site$long, y=ag.site$lat, color=ag.site$bb), size=0.8, alpha=0.4) + theme_classic() +
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
  annotate("text",label= "C. Alnus glutinosa", col="gold2", x=1, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
  labs(color="Day of Budburst") + ggtitle("")

## For QGIS
#spg<-ag.site
#coordinates(spg)<- ~long+lat
#proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
#coords<-spTransform(spg, CRS("+proj=longlat"))
#shapefile(coords, "output/alnglu_dvr.shp")

b.site<-filter(site, species=="BETPEN")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
bet <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=b.site$long, y=b.site$lat, color=b.site$bb), size=0.8, alpha=0.4) + theme_classic() +
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
  annotate("text",label= "A. Betula pendula", col="#7FC97F", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
  labs(color="Day of Budburst") + ggtitle("")

# For QGIS
#b.site<-na.omit(b.site)
#spg<-b.site
#coordinates(spg)<- ~long+lat
#proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
#coords<-spTransform(spg, CRS("+proj=longlat"))
#shapefile(coords, "output/betpen_dvr.shp", overwrite=TRUE)

f.site<-filter(site, species=="FAGSYL")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
syl <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=f.site$long, y=f.site$lat, color=f.site$bb), size=0.8, alpha=0.4) + theme_classic() +
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
  annotate("text",label= "D. Fagus sylvatica", col="#87A6A6", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
  labs(color="Day of Budburst") + ggtitle("")

## For QGIS
#spg<-f.site
#coordinates(spg)<- ~long+lat
#proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
#coords<-spTransform(spg, CRS("+proj=longlat"))
#shapefile(coords, "output/fagsyl_dvr.shp")

fe.site<-filter(site, species=="FRAEXC")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
fra <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=fe.site$long, y=fe.site$lat, color=fe.site$bb), size=0.8, alpha=0.4) + theme_classic() +
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
  annotate("text",label= "F. Fraxinus excelsior", col="#BF5B17", x=3, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
  labs(color="Day of Budburst") + ggtitle("")

## For QGIS
#spg<-fe.site
#coordinates(spg)<- ~long+lat
#proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
#coords<-spTransform(spg, CRS("+proj=longlat"))
#shapefile(coords, "output/fraexc_dvr.shp")

q.site<-filter(site, species=="QUEROB")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)

###### Need to comment out legend.position="none" on line 209 for legend (lines 223-229) and then remove comment for g1<-ggarrange...(line 232)
que <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=q.site$long, y=q.site$lat, color=q.site$bb), size=0.8, alpha=0.4) + 
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
  annotate("text",label= "E. Quercus robur", col="#CB1788", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
    labs(color="Day of Budburst") + ggtitle("")

## For QGIS
#spg<-q.site
#coordinates(spg)<- ~long+lat
#proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
#coords<-spTransform(spg, CRS("+proj=longlat"))
#shapefile(coords, "output/querob_dvr.shp")

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

png("figures/BB_base.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=8,
    height=5, units="in", res = 350 )
grid.draw(mappies)
dev.off()

