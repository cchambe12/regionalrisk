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

######################### Betula pendula #################################

d<-read.csv("output/betpen_data.csv", header=TRUE)

dx<-d[!is.na(d$Tmin),]
dx<-dx[!duplicated(dx),]
dx$frz<-ifelse(dx$Tmin<=-2.2, 1, 0)
dx<-arrange(dx, PEP_ID, Date)
dx$fs <- ave(
  dx$frz, dx$PEP_ID, dx$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
dx$pep.year<-paste(dx$PEP_ID, dx$year)
test<-dx[ave(dx$doy, dx$pep.year, FUN=length) <13, ]
testing<- test %>%
  group_by(pep.year) %>%
  arrange(Date) %>%
  filter(row_number()==n())
#testing$fs<-ifelse(testing$fs>=1, 1, 0)

mod<-lm(fs~year+lat, data=testing)
display(mod)

######################### Quercus robar #################################

d<-read.csv("output/querob_data.csv", header=TRUE)

dx<-d[!is.na(d$Tmin),]
dx<-dx[!duplicated(dx),]
dx$frz<-ifelse(dx$Tmin<=-2.2, 1, 0)
dx<-arrange(dx, PEP_ID, Date)
dx$fs <- ave(
  dx$frz, dx$PEP_ID, dx$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
dx$pep.year<-paste(dx$PEP_ID, dx$year)
test<-dx[ave(dx$doy, dx$pep.year, FUN=length) <13, ]
testing<- test %>%
  group_by(pep.year) %>%
  arrange(Date) %>%
  filter(row_number()==n())
#testing$fs<-ifelse(testing$fs>=1, 1, 0)

mod<-lm(fs~year+lat*long, data=testing)
display(mod)

######################### Fagus sylvatica #################################

d<-read.csv("output/fagsyl_data.csv", header=TRUE)

dx<-d[!is.na(d$Tmin),]
dx<-dx[!duplicated(dx),]
dx$frz<-ifelse(dx$Tmin<=-2.2, 1, 0)
dx<-arrange(dx, PEP_ID, Date)
dx$fs <- ave(
  dx$frz, dx$PEP_ID, dx$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
dx$pep.year<-paste(dx$PEP_ID, dx$year)
test<-dx[ave(dx$doy, dx$pep.year, FUN=length) <13, ]
testing<- test %>%
  group_by(pep.year) %>%
  arrange(Date) %>%
  filter(row_number()==n())
#testing$fs<-ifelse(testing$fs>=1, 1, 0)

mod<-lm(fs~year+lat*long, data=testing)
display(mod)