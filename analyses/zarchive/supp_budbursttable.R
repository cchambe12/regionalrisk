## Cat - 11 June 2019
# Working on budburst information for before and after CC

rm(list=ls()) 
options(stringsAsFactors = FALSE)


setwd("~/Documents/git/regionalrisk")

bb <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)

leafout <- read.csv("analyses/output/BBdata.csv", header=TRUE)
leafout <- subset(leafout, select=c("lat", "long", "year", "species", "bb"))

bbbycc <- left_join(bb, leafout)
bbbycc$cc <- ifelse(bbbycc$year<=1983, "before", "after")
bbbycc$mean <- ave(bbbycc$bb, bbbycc$species, bbbycc$cc)
bbbycc$sd <- ave(bbbycc$bb, bbbycc$species, bbbycc$cc, FUN=sd)
meanleafoutdates <- subset(bbbycc, select=c("species", "cc", "mean", "sd"))
meanleafoutdates <- meanleafoutdates[!duplicated(meanleafoutdates),]

write.csv(meanleafoutdates, file="analyses/output/changeinbb.csv", row.names=TRUE)
