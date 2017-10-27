### Let the processing begin!!
## ISSUES: Why are there 12 days but why are there days missing???
## Cat - 27 October 2017

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)
library(rstanarm)
library(lme4)

setwd("~/Documents/git/regionalrisk/analyses")
d<-read.csv("output/acepsu_data.csv", header=TRUE)

dx<-d[!is.na(d$Tmin),]
dx$frz<-ifelse(dx$Tmin<=-2.2, 1, 0)
dx<-arrange(dx, PEP_ID, Date)
dx$fs <- ave(
  dx$frz, dx$PEP_ID, dx$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

mod<-lm(fs~year+lat*long, data=dx)
display(mod)
