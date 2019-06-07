# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(raster)
library(arm)
library(rgdal)
library(maptools)
library(rgeos)
library(graphics)

setwd("~/Documents/git/regionalrisk/analyses")
bx<-read.csv("output/bbch_region.csv", header=TRUE)
land<-readShapeSpatial("input/natural_earth_vector/50m_physical/ne_50m_land.shp") 
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")




bx<-bx%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(bx$YEAR, bx$DAY)
bx$date<-as.Date(strptime(x, format="%Y %j"))
bx$year<-as.numeric(substr(bx$date, 0,4))
bx$month<-as.numeric(substr(bx$date, 6, 7))
bx$day<-as.numeric(substr(bx$date, 9,10))
bx<-filter(bx, year>=1950)
bx<-filter(bx, BBCH<=19)
bx<-bx%>%dplyr::select(lat, long, date, BBCH, PEP_ID, year)
num.years<-aggregate(year ~ PEP_ID, data = bx, FUN = length)
hist(num.years$year)
table(bx$BBCH)

master<-bx
master<-filter(master, BBCH==13)

plot(boundars,col="grey",border="lightgrey",ylim=c(30,70),xlim=c(-5,35))
colors<-colorRampPalette(c("red", "blue"))
master$bbch<-as.numeric(as.character(master$BBCH))
master$year<-as.numeric(as.character(master$year))
master$Col <- colors(10)[as.numeric(cut(master$year, breaks=2))]
points(master$long, master$lat, col=master$bbch, cex = .6)
points(master$long, master$lat, col=colors(10), cex = 0.6)
