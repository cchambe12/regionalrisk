### Started 15 January 2016 ###
### By Lizzie ###

## Looking at PEP725 species ###

# safety feature(s)
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# set up
setwd("~/Documents/git/projects/misc/pep725/")
library(plyr)
library(ggplot2)
library(rgdal)

# which species and phasecodes show up?
phendata <- read.csv2("PEP725_records/PEP725_data.csv", skip=1, header=FALSE,col.names=c("PEP_ID", "PLANT_ID", "CULT_ID", "BBCH", "YEAR", "DAY"))

goo <- ddply(phendata, c("PLANT_ID", "CULT_ID", "BBCH"), summarise,
             N=length(PLANT_ID),
             meanyr=mean(YEAR)
             )

goo <- goo[order(-goo$N),] # order by most to least N
hist(goo$N, breaks=200)
hist(goo$N, xlim=c(0,200), breaks=20000)

# station data 
statz <- read.csv2("PEP725_records/PEP725_stations.csv", header=TRUE)

# merge in cultivar and BBCH
plantz <- read.csv2("PEP725_records/PEP725_plant.csv", header=TRUE)
bbch <- read.csv2("PEP725_records/PEP725_BBCH.csv", header=TRUE)
cult <- read.csv2("PEP725_records/PEP725_cultivar.csv", header=TRUE)

dater <- merge(goo, bbch, by.x="BBCH", by.y="bbch", all.x=TRUE)
dater <- merge(dater, plantz, by.x="PLANT_ID", by.y="plant_id", all.x=TRUE)
dater <- merge(dater, cult, by.x="CULT_ID", by.y="cult_id", all.x=TRUE)

write.csv(dater, "pep725commonspp.csv", row.names=FALSE)

dater$plantstage <- paste(dater$sci_name, dater$description)
dater <- dater[order(-dater$N),] # order again by most to least N

# okay so let's look at those with N>50

dater50 <- subset(dater, N>50) # that halves things, from 690 to 367
dater100 <- subset(dater, N>100)
dim(dater100) # hmm, only takes us down a further couple dozen: to 322

length(unique(dater$sci_name)) # 122
length(unique(dater50$sci_name)) # 79

# which BBCH stages show up across multiple species and commonly?
commonbbchsp <- aggregate(dater["N"], dater["description"], FUN=length)
commonbbch <- aggregate(dater["N"], dater["description"], FUN=sum)

commonbbchsp[order(-commonbbchsp$N),]
hist(commonbbchsp$N, breaks=40)

keepbbch <- subset(commonbbchsp, N>5)

datercommon <- dater50[which(dater50$description %in% keepbbch$description),] 
length(unique(datercommon$sci_name)) # 77, ha! Saved one species

datercommonspp <- aggregate(datercommon["N"], datercommon[c("sci_name","cult_name")],
    FUN=length)

write.csv(datercommonspp, "pep725topspp.csv", row.names=FALSE)

# make a few maps of the Fu spp.

# get the map and set the theme
wmap <- readOGR("../maps/ne_110m_land", layer="ne_110m_land")
wmap.df <- fortify(wmap)

theme.tanmap <- list(theme(panel.grid.minor = element_blank(),
                        # panel.grid.major = element_blank(),
                        panel.background = element_rect(fill = "grey90",colour = NA),
                        # plot.background = element_rect(fill=NA),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22),
                        legend.position = "left"))

# Fu et al. 2015 species:
# Alnus glutinosa, Betula pendula, Aesculus hippocastanum, Fagus sylvatica, Tilia cordata, Quercus robur, Fraxinus excelsior


subb <- subset(phendata, PLANT_ID==108) # Fagus sylvatica
# subb <- subset(phendata, PLANT_ID==102 & CULT_ID==40) # Alnus glutinosa
# subb <- subset(phendata, PLANT_ID==101) # Aesculus hippocastanum
# subb <- subset(phendata, PLANT_ID==106 & CULT_ID==20) # Betula pendula
# subb <- subset(phendata, PLANT_ID==129 & CULT_ID==70) # Tilia cordata
# subb <- subset(phendata, PLANT_ID==111) # Quercus robur
# subb <- subset(phendata, PLANT_ID==120) # Fraxinus excelsior


subbstat <- merge(subb, statz, by="PEP_ID")
subbstatagg <- aggregate(subbstat[c("PEP_ID")], subbstat[c("BBCH", "LON", "LAT", "ALT")], FUN=length)
names(subbstatagg)[names(subbstatagg)=="PEP_ID"] <- "obs.n"
# make the lat, longs numeric (note: should look into why they are not already)
subbstatagg$lat <- as.numeric(subbstatagg$LAT)
subbstatagg$lon <- as.numeric(subbstatagg$LON)

ggplot() + 
  geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
  coord_cartesian(ylim=c(30, 75), xlim=c(-15, 40)) +
  geom_point(data=subbstatagg, 
             aes(x=lon, y=lat, size=obs.n, fill=BBCH), 
             colour="dodgerblue4", pch=21) +
  theme.tanmap
