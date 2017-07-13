
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
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,60),xlim=c(-5,35)) # create a layer of borders
mp <- ggplot() +   mapWorld
mp + theme(panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())


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
boundars<-readShapeSpatial("input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
betula<-read.csv("output/betula_events.csv", header=TRUE)
fagus<-read.csv("output/fagus_events.csv", header=TRUE)
acer<-read.csv("output/acer_events.csv")
bet.clim<-read.csv("output/climate_betula.csv", header=TRUE)
bet.bb<-read.csv("output/bbch_region_betula.csv", header=TRUE)
plot(land,col="grey",lty=0,ylim=c(30,60),xlim=c(-5,35))

## or:
plot(boundars,col="grey",border="lightgrey",ylim=c(30,70),xlim=c(-5,35))
colors<-colorRampPalette(c("red", "blue"))
betula$events<-as.numeric(as.character(betula$events))
fagus$freq<-as.numeric(as.character(fagus$freq))
fagus$Col <- colors(10)[as.numeric(cut(fagus$freq,breaks = 0.1))]
points(fagus$long, fagus$lat, col = colors(10), cex = .6)

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



