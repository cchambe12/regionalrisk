### 11 July 2019 - Cat
## To check out species differences, start with differences in data

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)
library(ggplot2)


## Set Working Directory
setwd("~/Documents/git/regionalrisk")

fs <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)
fs$spp <- as.numeric(as.factor(fs$species))
species <- unique(fs$species)

### Plot of histograms

cols <- colorRampPalette(brewer.pal(7,"Accent"))(6)


quartz()
par(mfrow=c(2,2))
ggplot(fs, aes(x=mst,col=species, fill=species)) + geom_histogram() + coord_cartesian(xlim=c(-7,17)) + 
  xlab("Mean Spring Temperature (°C)") + 
  scale_color_manual(values=cols,
                     labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                              "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                              "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                              "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                              "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                              "QUEROB"=expression(paste(italic("Quercus robur"))))) + 
  scale_fill_manual(values=cols,
                    labels=c("AESHIP"=expression(paste(italic("Aesculus hippocastanum"))),
                             "ALNGLU"=expression(paste(italic("Alnus glutinosa"))),
                             "aaBETPEN"=expression(paste(italic("Betula pendula"))),
                             "FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "zFRAEXC"=expression(paste(italic("Fraxinus excelsior"))),
                             "QUEROB"=expression(paste(italic("Quercus robur")))))
hist(fs$distkm, xlim=c(0,720), main="", xlab="Distance from Coast (km)")
hist(fs$elev, xlim=c(0, 1900), main="", xlab="Elevation (m)")
hist(fs$nao, xlim=c(-0.6, 0.7), main="NAO Index")




### alright now let's go through the predictors...
quartz() ## MST
par(mfrow=c(2,3))
for(i in 1:length(species)){
  hist(fs$mst[(fs$spp==i)], xlim=c(-7, 17))
}

quartz() ## Distance
par(mfrow=c(2,3))
for(i in 1:length(species)){
hist(fs$distkm[(fs$spp==i)], xlim=c(0, 720))
}

quartz() ## Elev
par(mfrow=c(2,3))
for(i in 1:length(species)){
  hist(fs$elev[(fs$spp==i)], xlim=c(0, 1900))
}

quartz() ## NAO
par(mfrow=c(2,3))
for(i in 1:length(species)){
  hist(fs$nao[(fs$spp==i)], xlim=c(-0.6, 0.7))
}


### No glaring differences... 