### Compare some results...


# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(arm)
library(data.table)


setwd("~/Documents/git/regionalrisk/analyses/output")
last<-read.csv("lastobs.csv", header=TRUE)
not<-read.csv("notlastobs.csv", header=TRUE)


tot<-full_join(last, not)
dupes<-tot[duplicated(tot),]


list<-dupes%>%dplyr::select(PEP_ID, year)
list<-list[!duplicated(list),]



write.csv(list, "~/Documents/git/regionalrisk/analyses/output/ListOfDupes.csv", row.names = FALSE)
