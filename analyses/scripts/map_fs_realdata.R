## Map showing False Springs vs strictly climate map
# Cat - 31 Aug 2018

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/regionalrisk/analyses")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(grid)
library(gridExtra)
library(maptools)
library(ggplotify)

## a bunch of this code is taken from cleaning/cleanup_checksmaps.R
# Get packages
d<-read.csv("output/fs_newdvr_space.csv", header=TRUE)

d<-subset(d, select=c("fs.count", "lat", "long", "lat.long", "cc"))
d<-d[!duplicated(d),]

d$numfs<-ave(d$fs.count, d$lat.long, d$cc, FUN=sum)
df<-d%>%spread(cc, numfs)

df<-na.omit(df)
df$diff<-df$`1`-df$`0`


#mapWorld <- borders("world", colour="gray94", fill="gray92") # create a layer of borders
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mp <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
            color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=1.5,aes(x=df$long, y=df$lat, col=as.integer(df$diff))) + theme_classic() +
  theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.position=c(0.075, 0.163),
        #legend.position = "none",
        legend.key = element_rect(fill="white", color="white"),
        legend.box.background = element_rect(fill="white"),legend.text = element_text(size=7), legend.key.size = unit(0.3,"cm"),
        legend.title = element_text(size=8))+
  guides(shape=FALSE)  +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-13,40), ylim=c(34,72))



quartz()
mp


