# 11 Nov 2018 - Cat
## Checking out DVR

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("~/Documents/git/regionalrisk/analyses")
d<-read.csv("input/aeship_dvrcheck.csv", header=TRUE)
d<-read.csv("input/acepse_dvrcheck.csv", header=TRUE)

### Let's just start with the PEP data and do some cleaning
bbchs<-c(7, 11)
df<-subset(d, d$BBCH %in% bbchs)
df<-subset(df, df$YEAR>=2008)  

dx<-df%>%spread(BBCH, DAY)  
dx<-na.omit(dx)  

dx$dvr<-dx$`11`-dx$`7`  
mean(dx$dvr) ## 9.43 - range of years is 2008-2014; 62 sites; most years at one site is 4 (AESHIP)
mean(dx$dvr) ## 12.42 - range of years is 2011-2014; 52 sites; most is 3 out of 4 years (ACEPSE)
ave(dx$dvr, dx$PEP_ID)


