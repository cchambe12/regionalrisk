
##############################################################################################################
# Script to:
#' * Easy approach to generate maps w/wo ocean background
#'
#'  Ignacio Morales-Castilla
##############################################################################################################

rm(list=ls())
library(ggmap)
library(ggplot2)
library(rworldmap)
library(maps)
library(mapdata)
library(marmap)
library(dplyr)
library(tidyr)
library(egg)
library(RColorBrewer)
library(raster)

library(maptools)
library(ggplotify)


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


#Using GGPLOT, plot the Base World Map
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35)) # create a layer of borders
site<-d%>%dplyr::select(lat, long, space)
site<-site[!duplicated(site),]
mp <- ggplot(site, aes(x=long, y=lat, color=space)) +   mapWorld +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))
mp + theme(panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) + geom_point(aes(color=space)) + geom_jitter()

setwd("~/Documents/git/regionalrisk/analyses")
d<-read.csv("output/BBdata_dvr.csv", header=TRUE)
d<-d[(d$bb>=0),]
d$bb.space<-ave(d$bb, d$PEP_ID, d$species)

#Using GGPLOT, plot the Base World Map
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35)) # create a layer of borders
site<-d%>%dplyr::select(lat, long, bb.space, species)
site<-site[!duplicated(site),]
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(30, 168))
a.site<-filter(site, species=="AESHIP")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
aes <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=3,aes(x=a.site$long, y=a.site$lat, color=a.site$bb), size=0.8, alpha=0.4) + theme_classic() +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        #legend.position = "none",
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95")) +
  annotate("text",label= "Aesculus \nhippocastanum", col="gold2", x=-12, y=68,fontface="bold.italic", size=3,
           family="Helvetica", hjust=0) + sc + 
  labs(color="Day of Budburst") + ggtitle("C.")

spg<-a.site
coordinates(spg)<- ~long+lat
proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
coords<-spTransform(spg, CRS("+proj=longlat"))
shapefile(coords, "output/aeship_dvr.shp")

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
  annotate("text",label= "Alnus glutinosa", col="#CAB1C4", x=1, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
  labs(color="Day of Budburst") + ggtitle("B.")

spg<-ag.site
coordinates(spg)<- ~long+lat
proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
coords<-spTransform(spg, CRS("+proj=longlat"))
shapefile(coords, "output/alnglu_dvr.shp")

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
  annotate("text",label= "Betula pendula", col="#7FC97F", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
  labs(color="Day of Budburst") + ggtitle("A.")

b.site<-na.omit(b.site)
spg<-b.site
coordinates(spg)<- ~long+lat
proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
coords<-spTransform(spg, CRS("+proj=longlat"))
shapefile(coords, "output/betpen_dvr.shp", overwrite=TRUE)

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
  annotate("text",label= "Fagus sylvatica", col="#87A6A6", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
  labs(color="Day of Budburst") + ggtitle("D.")

spg<-f.site
coordinates(spg)<- ~long+lat
proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
coords<-spTransform(spg, CRS("+proj=longlat"))
shapefile(coords, "output/fagsyl_dvr.shp")

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
        panel.background = element_rect(fill="grey95")) +
  annotate("text",label= "Fraxinus excelsior", col="#BF5B17", x=3, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
  labs(color="Day of Budburst") + ggtitle("F.")

spg<-fe.site
coordinates(spg)<- ~long+lat
proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
coords<-spTransform(spg, CRS("+proj=longlat"))
shapefile(coords, "output/fraexc_dvr.shp")

q.site<-filter(site, species=="QUEROB")
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
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
          legend.position = "none",
          axis.title = element_blank(),
          panel.background = element_rect(fill="grey95")) +
  annotate("text",label= "Quercus robur", col="#CB1788", x=0, y=70,fontface="bold.italic", size=3,
           family="Helvetica") + sc + 
    labs(color="Day of Budburst") + ggtitle("E.")

spg<-q.site
coordinates(spg)<- ~long+lat
proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
coords<-spTransform(spg, CRS("+proj=longlat"))
shapefile(coords, "output/querob_dvr.shp")


quartz()
mappies<-ggarrange(bet, aln, aes, syl, que, fra, ncol=3, nrow=2)

png("figures/BB_dvr.png", 
    width=8,
    height=5, units="in", res = 350 )
grid.draw(mappies)
dev.off()

## mapping with rworldmap
mapDevice() #create world map shaped window

trymap<-getMap()
mapBubbles(dF=trymap
           ,colourPalette="rainbow"
           #,oceanCol="white"
           ,landCol="grey62"
           ,borderCol="grey62"
           ,ylim=c(-49.5,70)
           ,lwd=0.1,add)

## adding contour lines
map('worldHires',add=T,col="lightgrey",lwd=1.5)


############ in reality those are suboptimal choices
## it is best to obtain shapefiles (e.g. http://www.naturalearthdata.com/)
## and plotting them
??shapefile

library(rgdal)
library(maptools)
library(rgeos)
setwd("~/Documents/git/regionalrisk/analyses")
land<-readShapeSpatial("input/natural_earth_vector/50m_physical/ne_50m_land.shp") ## 
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
master<-read.csv("output/master_events.csv", header=TRUE)
betula<-read.csv("output/betula_events.csv", header=TRUE)
fagus<-read.csv("output/fagus_events.csv", header=TRUE)
acer<-read.csv("output/acer_events.csv", header=TRUE)
tilia<-read.csv("output/tilia_events.csv", header=TRUE)
master<-read.csv("output/climate_master.csv", header=TRUE)
#bet.bb<-read.csv("output/bbch_region_betula.csv", header=TRUE)
plot(land,col="grey",lty=0,ylim=c(30,60),xlim=c(-5,35))

## master:
master<-prep_cc
plot(boundars,col="grey",border="lightgrey",ylim=c(30,70),xlim=c(-5,35))
colors<-colorRampPalette(c("red", "blue"))
master$events<-as.numeric(as.character(master$fs.num))
master$Col <- colors(10)[as.numeric(cut(as.factor(master$species),breaks = 1))]
points(master$long, master$lat, col=colors(10), cex = master$events)

# betula
plot(boundars,col="grey",border="lightgrey",ylim=c(30,70),xlim=c(-5,35))
colors<-colorRampPalette(c("red", "blue"))
betula$events<-as.numeric(as.character(betula$events))
betula$freq<-as.numeric(as.character(betula$freq))
betula$Col <- colors(10)[as.numeric(cut(betula$events,breaks = 2))]
points(betula$long, betula$lat, col=colors(10), cex = .6)

# fagus
plot(boundars,col="grey",border="lightgrey",ylim=c(30,70),xlim=c(-5,35))
colors<-colorRampPalette(c("red", "blue"))
fagus$events<-as.numeric(as.character(fagus$events))
fagus$freq<-as.numeric(as.character(fagus$freq))
fagus$Col <- colors(10)[as.numeric(cut(fagus$events,breaks = 2))]
points(fagus$long, fagus$lat, col=colors(10), cex = .6)

# acer
plot(boundars,col="grey",border="lightgrey",ylim=c(30,70),xlim=c(-5,35))
colors<-colorRampPalette(c("red", "blue"))
acer$events<-as.numeric(as.character(acer$events))
acer$freq<-as.numeric(as.character(acer$freq))
acer$Col <- colors(10)[as.numeric(cut(acer$events,breaks = 2))]
points(acer$long, acer$lat, col=colors(10), cex = .6)

# tilia
plot(boundars,col="grey",border="lightgrey",ylim=c(30,70),xlim=c(-5,35))
colors<-colorRampPalette(c("red", "blue"))
tilia$events<-as.numeric(as.character(tilia$events))
tilia$freq<-as.numeric(as.character(tilia$freq))
tilia$Col <- colors(10)[as.numeric(cut(tilia$events,breaks = 2))]
points(tilia$long, tilia$lat, col=colors(10), cex = .6)


## adding bathimetry to the plot ## can take a while
BATHYMET<-getNOAA.bathy(lon1=-15,lon2=40,lat1=30,lat2=70, resolution=30)

# Creating a custom palette of blues
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
# Plotting the bathymetry with different colors for land and sea
plot(BATHYMET, image = TRUE, land = TRUE, lwd = 0.05,lty=0,
     bpal = list(c(0, max(BATHYMET), "lightsteelblue1"),
                 c(min(BATHYMET),0,blues)),add=F)
ggplot(BATHYMET)


library(raster)
## cropping shape to add within the extent of bathymetry
extent(boundars)
to.crop<-extent(-15,40,30,70)
land.cropped<-crop(boundars,to.crop)
plot(land.cropped,col="grey",lty=0,add=TRUE, border="lightgrey",lwd=3)
points(bet.bb$long, bet.bb$lat, col = "red", cex = .6, add=TRUE)



plot(boundars, col="grey", border="lightgrey", ylim=c(30, 70), xlim=c(-5, 35))

library(ggmap)
map<-get_map(location="Europe", zoom=4)
mapPoints<-ggmap(map) + geom_point(x=bet.clim$long, y=bet.clim$lat, col="red", data=bet.clim) + geom_point(x=betula$long, y=betula$lat, col="blue", data=betula) +
  geom_point(x=bet.bb$LON, y=bet.bb$LAT, col="green", data=bet.bb)
mapPoints



