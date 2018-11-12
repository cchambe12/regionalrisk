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

df$total<-ave(df$fs.count, df$lat.long, FUN=sum)

df<-na.omit(df)
df$diff<-df$`1`-df$`0`

df<-df[!(df$total<2),]

#mapWorld <- borders("world", colour="gray94", fill="gray92") # create a layer of borders
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
myPalette <- colorRampPalette(brewer.pal(6, "Spectral"))
sc <- scale_colour_gradientn(colours = myPalette(50), limits=c(1, 38))
mp <- ggplot() + 
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
            color = 'gray', fill="lightgrey", size = .2) + 
  geom_jitter(width=3,aes(x=df$long, y=df$lat, color=df$total, alpha=df$total), size=0.8) + theme_classic() +
  theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.position=c(0.075, 0.163),
        #legend.position = "none",
        legend.key = element_rect(fill="white", color="white"),
        legend.box.background = element_rect(fill="white"),legend.text = element_text(size=7), legend.key.size = unit(0.3,"cm"),
        legend.title = element_text(size=8))+ sc +
  xlab("") + ylab("") + coord_cartesian(ylim=c(30,70),xlim=c(-10,35)) + guides(alpha=FALSE)

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
  labs(color="Day of Budburst")

quartz()
mp


