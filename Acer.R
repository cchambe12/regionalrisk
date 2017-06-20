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

# Set Working Directory
setwd("~/Documents/git/regionalrisk/PEP/PEP725_AT")
austria<-read.csv("PEP725_AT_Acer.csv", header=TRUE)
aust.station<-read.csv("PEP725_AT_stations.csv", header=TRUE)

at<-austria%>%filter(BBCH<=19)
at<-full_join(at, aust.station)
