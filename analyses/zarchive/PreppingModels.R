### 12 January 2018 - Cat
### Starting Analysis - looking at different questions
## 1) FS (y/n) ~ year + (1|species) + site
## 2) FS # ~ species + site + MAT

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
#library(rstanarm)
#library(rstan)
#library(bayesplot)
#library(shinystan)


library(lubridate)
library(ncdf4)
library(Interpol.T)
library(chillR)
library(raster)
library(reshape2)
library(data.table)
library(velox)

### Load data
setwd("~/Documents/git/regionalrisk/analyses/output")
d<-read.csv("fs_yearsitespp.csv", header=TRUE)
dx<-read.csv("mat.csv", header=TRUE)

### Clean the weather data
setwd("~/Desktop/")
r<-brick("~/Desktop/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")

d$lat.long<-paste(d$lat, d$long, sep=",")
d<-d[!duplicated(d$lat.long),]
lats<-d$lat
lons<-d$long
coords<-data.frame(x=lons, y=lats)
points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

dclim <- cbind.data.frame(coordinates(points),values)

dx<-melt(dclim, id.vars=c("x","y"))

dx<-dx%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tavg=value)

dx$date<-substr(dx$date, 2,11)
dx$Date<-gsub("[.]","-", dx$date)

dx<-dplyr::select(dx, -date)

#write.csv(dx, file="~/Documents/git/regionalrisk/analyses/output/mat.csv", row.names=FALSE)

dx$year<- substr(dx$Date, 0, 4)
dx$lat.long<-paste(dx$lat, dx$long)

dx$mat<-ave(dx$Tavg, dx$year, dx$lat, dx$long)
dx<-dplyr::select(dx, -Tavg)
dxx<-dx[!duplicated(dx),]

one<-dx%>%filter(year<=1969)
one$mat<-ave(one$Tavg, one$year, one$lat.long)
one<-dplyr::select(one, -Tavg)
done<-one[!duplicated(one),]

two<-dx%>%filter(year<=1989) %>% filter(year>=1970)
two$mat<-ave(two$Tavg, two$year, two$lat.long)
two<-dplyr::select(two, -Tavg)
dtwo<-two[!duplicated(two),]

d<-full_join(done, dtwo)

th<-dx%>%filter(year<=2008) %>% filter(year>=1990)
th$mat<-ave(th$Tavg, th$year, th$lat.long)
th<-dplyr::select(th, -Tavg)
dth<-th[!duplicated(th),]

d<-full_join(d, dth)

## Start running models...
d$PEP_ID<-as.numeric(as.factor(d$PEP_ID))
d$year<-as.numeric(d$year)
d$species<-as.numeric(as.factor(d$species))
df<-d[sample(nrow(d), 50000), ]

mod<-stan_glmer(fs~year+(1|species)+lat*long, data=df, family=gaussian)


