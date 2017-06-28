## 20 June 2017 - Cat
## Cleaning up PEP data for Regional Risk paper


# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(arm)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(grid)
library(rworldmap)
library(gridExtra)


# Set Working Directory
setwd("~/Documents/git/regionalrisk/data/acer")
austria<-read.csv("PEP725_AT/PEP725_AT_Acer.csv", header=TRUE)
aust.station<-read.csv("PEP725_AT/PEP725_AT_stations.csv", header=TRUE)
switz<-read.csv("PEP725_CH/PEP725_CH_Acer.csv", header=TRUE)
switz.station<-read.csv("PEP725_CH/PEP725_CH_stations.csv", header=TRUE)
czech<-read.csv("PEP725_CZ/PEP725_CZ_Acer.csv", header=TRUE)
czech.station<-read.csv("PEP725_CZ/PEP725_CZ_stations.csv", header=TRUE)
spain<-read.csv("PEP725_ES/PEP725_ES_Acer.csv", header=TRUE)
spain.station<-read.csv("PEP725_ES/PEP725_ES_stations.csv", header=TRUE)
latv<-read.csv("PEP725_LV/PEP725_LV_Acer.csv", header=TRUE)
latv.station<-read.csv("PEP725_LV/PEP725_LV_stations.csv", header=TRUE)
neth<-read.csv("PEP725_NL/PEP725_NL_Acer.csv", header=TRUE)
neth.station<-read.csv("PEP725_NL/PEP725_NL_stations.csv", header=TRUE)
engl<-read.csv("PEP725_UK/PEP725_UK_Acer.csv", header=TRUE)
engl.station<-read.csv("PEP725_UK/PEP725_UK_stations.csv", header=TRUE)


at<-austria%>%filter(BBCH<=19)
at<-full_join(at, aust.station)
at<-dplyr::select(at, -NAME)
at<-na.omit(at)

ch<-switz%>%filter(BBCH<=19)
ch<-full_join(ch, switz.station)
ch<-dplyr::select(ch, -NAME)
ch<-na.omit(ch)

d<-bind_rows(at,ch)

cz<-czech%>%filter(BBCH<=19)
cz<-full_join(cz, czech.station)
cz<-dplyr::select(cz, -NAME)
cz<-na.omit(cz)

d<-bind_rows(d, cz)

lv<-latv%>%filter(BBCH<=19)
lv<-full_join(lv, latv.station)
lv<-dplyr::select(lv, -NAME)
lv<-na.omit(lv)

d<-bind_rows(d, lv)

nl<-neth%>%filter(BBCH<=19)
nl<-full_join(nl, neth.station)
nl<-dplyr::select(nl, -NAME)
nl<-na.omit(nl)

d<-bind_rows(d, nl)

uk<-engl%>%filter(BBCH<=19)
uk<-full_join(uk, engl.station)
uk<-dplyr::select(uk, -NAME)
uk<-na.omit(uk)

d<-bind_rows(d, uk)
d<-na.omit(d)
d$species<-"ACEPSU"
#write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/bbch_region.csv", row.names = FALSE)

at$mean<-ave(at$DAY, at$PEP_ID)
at.high<-at%>%group_by(PEP_ID)%>%summarise(high=max(DAY))%>%ungroup(at)
at.low<-at%>%group_by(PEP_ID)%>%summarise(low=min(DAY))%>%ungroup(at)


########## Look at data a bit - qplots and glm #########################
qplot(as.factor(PEP_ID), DAY, data = at, 
      geom = "boxplot", color=PEP_ID) + 
  xlab("Site")+ylab("Budburst to Leafout")

rounded<-d
rounded$LAT<-round(rounded$LAT)
qplot(as.factor(LAT), DAY, data = rounded, 
      geom = "boxplot", color=PEP_ID) + 
  xlab("Site")+ylab("Budburst to Leafout")

rounded$LON<-round(rounded$LON)
qplot(as.factor(LON), DAY, data = rounded, 
      geom = "boxplot", color=PEP_ID) + 
  xlab("Site")+ylab("Budburst to Leafout")

mod<-glm(DAY~YEAR + LAT*LON + ALT, data=thirty)
display(mod)

mod1<-glm(DAY~., data=thirty)
summary(mod1)
hist(thirty$LAT)
ggplot((thirty), aes(x=LAT, y=DAY)) + geom_point(aes(color=LON))

at$average<-ave(at$DAY, at$LON)
at$maybe<-ave(at$average, at$LAT)

################ Attempt to make a map #############################
# Europe Map
# Get the world map
worldMap <- getMap()

# European Countries
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Norway","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","Switzerland", "United Kingdom")
indEU <- which(worldMap$NAME%in%europeanUnion)
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

eur <- ggplot(europeCoords) + geom_polygon(data = europeCoords, aes(x = long, y = lat, group=region), 
                                           color="grey", fill="white") + coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

thirty<-d%>%filter(YEAR>=1986)
eur.map <- eur + 
  geom_point(aes(LON, LAT, color=DAY),position="jitter", data=d) + scale_color_gradient(low = "blue", high="red", breaks=c(30,60,90,120,150,180,210))
plot(eur.map)






