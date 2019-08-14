## 15 July 2019 - Cat ##
## Making a hinge model for Yann Vitasse and Fredi

### housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(dplyr)


## Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses")

fs <- read.csv("output/fs_newspace_fullleaf.csv", header=TRUE)

## Clean up dataframe
fs <- subset(fs, select=c("fs", "species", "lat", "long", "elev", "lat.long", "year", 
                          "cc"))

fs$fs.count <- ave(fs$fs, fs$species, fs$lat.long, fs$year, FUN=sum)
fs$fs <- ifelse(fs$fs.count>0, 1, 0)

fs <- fs[!duplicated(fs),]

## Now let's see which sites are represented for 20 years on either side of break point
fs$numsites.cc<-ave(fs$year, fs$lat.long, fs$cc, fs$species, FUN=length)
#fs$numsites<-ave(fs$year, fs$lat.long, fs$species, FUN=length)
#fs.cleansites <- fs[(fs$numsites>=65),]

if(TRUE){
fs$spsite <- paste(fs$species, fs$lat.long)
findsites <- subset(fs, select=c("lat.long", "cc", "numsites.cc", "species"))
findsites <- findsites[!duplicated(findsites),]
findsites <- findsites[(findsites$numsites.cc>=10),]

findsites <- tidyr::spread(findsites, cc, numsites.cc)

findsites <- findsites[(findsites$`0`==findsites$`1`),]
findsites <- na.omit(findsites)
findsites$spsite <- paste(findsites$species, findsites$lat.long)

sites <- unique(findsites$spsite)

fs.cleansites<-fs[(fs$spsite%in%sites),]
}

fs.cleansites$spsite <- paste(fs.cleansites$species, fs.cleansites$lat.long)
fs.cleansites$hinge <- fs.cleansites$year - 1983
fs.cleansites$hinge <- ifelse(fs.cleansites$hinge<0, 0, fs.cleansites$hinge)

sppxsite <- length(unique(fs.cleansites$spsite))

#spp <- length(unique(fs.cleansites$species))

fs.cleansites$spsitenum <- as.numeric(as.factor(fs.cleansites$spsite))
#fs.cleansites$spnum <- as.numeric(as.factor(fs.cleansites$species))

outcome <- data.frame( matrix( ncol=5, nrow=sppxsite))
x <- c("spsitenum", "intercept", "confint", "slope", "confslope")
colnames(outcome) <- x
outcome$spsitenum <- seq(1, to=sppxsite, by=1)

#fs.glm <-subset(fs.cleansites, select=c("fs", "spsitenum", "hinge"))
#fs.glm <- fs.glm[!duplicated(fs.glm),]

for(i in 1:sppxsite){ #i=1
  mod <- glm(fs ~ hinge, data=fs.cleansites[(fs.cleansites$spsitenum==i),], family=binomial(link="logit"))
  #mod <- lm(fs ~ hinge, data=fs.cleansites[(fs.cleansites$spsitenum==i),])

  #outcome[which(outcome$spsite==i),1] <- i
  outcome[which(outcome$spsitenum==i),2] <- coef(mod)[1]
  outcome[which(outcome$spsitenum==i),3] <- confint.default(mod)[1]
  outcome[which(outcome$spsitenum==i),4] <- coef(mod)[2]
  outcome[which(outcome$spsitenum==i),5] <- confint.default(mod)[2]

}

sppsites <- subset(fs.cleansites, select=c("spsitenum", "species", "lat", "long",
                                           "elev"))

outcome <- full_join(outcome, sppsites)
outcome <- outcome[!duplicated(outcome),]

hingeoutput <- subset(outcome, select=c("species", "lat", "long", "elev",
                                        "intercept", "confint", "slope", "confslope"))

write.csv(hingeoutput, file="output/hingemodel.csv", row.names = FALSE)
