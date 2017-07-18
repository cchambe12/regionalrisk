### Started 25 February 2016 ###
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

# get the data
phendata <- read.csv2("PEP725_records/PEP725_data.csv", skip=1, header=FALSE,col.names=c("PEP_ID", "PLANT_ID", "CULT_ID", "BBCH", "YEAR", "DAY"))

# BBCH, cultivar, plant ID, station data 
statz <- read.csv2("PEP725_records/PEP725_stations.csv", header=TRUE)
plantz <- read.csv2("PEP725_records/PEP725_plant.csv", header=TRUE)
bbch <- read.csv2("PEP725_records/PEP725_BBCH.csv", header=TRUE)
cult <- read.csv2("PEP725_records/PEP725_cultivar.csv", header=TRUE)

# get the species we're possibly interested in
projspp <- read.csv("pepsens/input/project_species.csv", header=TRUE)


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

for (i in c(1:nrow(projspp))){
    subb <- subset(phendata, PLANT_ID==projspp$plant_id[i] & CULT_ID==projspp$cult_id[i])
    bbchhere <- unique(subb$BBCH)
    for (j in c(1:length(bbchhere))){
        subbch <- subset(subb, BBCH==bbchhere[j])
        subbstat <- merge(subbch, statz, by="PEP_ID")
        subbstatagg <- aggregate(subbstat[c("PEP_ID")], subbstat[c("LON", "LAT", "ALT")], FUN=length)
        names(subbstatagg)[names(subbstatagg)=="PEP_ID"] <- "obs.n"
        # make the lat, longs numeric (note: should look into why they are not already)
        subbstatagg$lat <- as.numeric(subbstatagg$LAT)
        subbstatagg$lon <- as.numeric(subbstatagg$LON)
        pdf(paste("pepsens/graphs/", subbch$PLANT_ID[1], subbch$CULT_ID[1], "bbch", subbch$BBCH[1], ".pdf", sep=""))
        print(ggplot() + 
            geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
            coord_cartesian(ylim=c(30, 75), xlim=c(-15, 40)) +
            geom_point(data=subbstatagg, 
            aes(x=lon, y=lat, size=obs.n), 
            colour="dodgerblue4", pch=21) +
            theme.tanmap)
        dev.off()
    }
}
