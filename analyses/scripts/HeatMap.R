## 28 July 2017 - Cat with assistance from Nacho!
## Cleaning up Climate data for Regional Risk paper for Heat Map


# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(Interpol.T)
library(chillR)
library(raster)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/input")

eur.temp <- nc_open(file.path("tn_0.25deg_reg_v15.0.nc"))
raster1<-brick("tn_0.25deg_reg_v15.0.nc", varname="tn", sep="")
## 17 leap years - need to come up with a loop that sequences through those years separately...


#raster1<-brick()
#names(name.min)

#dir.temps<- “your directory”
#name.min<- “the file’s name”


# open as brick
#raster1<- brick(paste(dir.temps,name.min,sep=''))
#raster1<-brick(name.min, sep="")

#######################################################
# Function to rename bias-corrected GCMs:
#'
#' Started 20th January 2017
# Ignacio Morales-Castilla, Benjamin Cook & Elizabeth M. Wolkovich
#######################################################
rename.GCM<-function(x,list.years){
  years.i<-paste(min(list.years),max(list.years),sep="-")
  nyears<-length(seq(min(list.years),max(list.years),1))
  doys.names<-sort(rep(seq(1,365,1),nyears))
  years.names<-rep(seq(min(list.years),max(list.years),1),365)
  
  if(length(names(x))==length(years.names)){
    names(x)<-paste(rep('X',length(years.names)),years.names,doys.names,sep=".")
  }
  
}

## re-naming (see attached the script with a function to rename GCMs, you may need to change it according to your data, or may not even need this)
temp.years<-1:30
names(raster1)<-rename.GCM(raster1,temp.years[[30]]) ## temp.years[[time.years]] – this is your period of interest (start to end of the period in the file)


empty.raster<-raster1[[1]]
values(empty.raster)<-NA
non.nas.ids<-which(!is.na(values(raster1[[1]])))


#plot(raster1)

start.date<-45
end.date<-200
period<-start.date:end.date

nlayers1<-nlayers(raster1)
seq1<-seq(1,nlayers1,365)
length(1950:2016)

num.false.spring.year<-list()
for(i in 1:30){
  print(i)
  sequence.years<-seq(seq1[i]+45,seq1[i]+200,1)
  raster.sub<-subset(raster1,sequence.years)
  numnonas<-sum(!is.na(values(raster.sub[[1]]))
                
                rast.array<-array(NA,dim=c(numnonas,155))
                for(j in 1:155){
                  rast.array[,j]<-values(raster.sub[[j]]))[which(!is.na(values(raster.sub[[j]]))))]
  
  
                }
                
                num.false.spring<-apply(rast.array,1,function(x){sum(ifelse(x<-2.2,1,0))})
                values(empty.raster)<-NA
                values(empty.raster)[non.nas.ids]<-num.false.spring
                
                num.false.spring.year[[i]]<-empty.raster
                
}

final.raster<-stack(unlist(num.false.spring.year))

extract(final.raster,xylonlat)
summed.false.springs<-calc(final.raster,sum)   
?writeRaster