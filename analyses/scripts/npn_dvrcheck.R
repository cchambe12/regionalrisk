### Add more data on how we selected 12 days for Budburst to Leafout
## 1 May 2019 - Cat

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(dplyr)

setwd("~/Documents/git/regionalrisk/analyses/input")

npn <- read.csv("npndata_dvrtime2.csv", header=TRUE)

phenophases <- c("Breaking leaf buds", ">=75% of full leaf size (deciduous)", "Leaves")
npn <- npn[(npn$Phenophase_Description%in%phenophases),]

npn <- npn[(npn$Multiple_FirstY>=1 | npn$Multiple_Observers>0),]
npn <- npn[(npn$NumYs_in_Series>=7),]
npn <- npn[(npn$NumDays_Since_Prior_No<=7),]

bb <- npn[(npn$Phenophase_Description=="Breaking leaf buds"),]
bb$budburst <- ave(bb$First_Yes_DOY, bb$Individual_ID, bb$First_Yes_Year, FUN=first)
bb <- subset(bb, select=c("Individual_ID", "Genus", "Species", "First_Yes_Year", 
                          "First_Yes_DOY", "NumDays_Since_Prior_No", "NumDays_Until_Next_No", "budburst"))

lo <- npn[(npn$Phenophase_Description=="Leaves"),]
lo$leafout <- ave(lo$First_Yes_DOY,lo$Individual_ID, lo$First_Yes_Year, FUN=first)
lo <- subset(lo, select=c("Individual_ID", "Genus", "Species", "First_Yes_Year", 
                          "First_Yes_DOY", "NumDays_Since_Prior_No", "NumDays_Until_Next_No", "leafout"))


dvr <- left_join(lo, bb, by=c("Individual_ID", "First_Yes_Year"))
dvr$dvr <- dvr$leafout - dvr$budburst
dvr <- dvr[!is.na(dvr$dvr),]
mean(dvr$dvr) # 12.53 days!  


