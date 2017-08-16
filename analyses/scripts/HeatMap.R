## 16 August 2017
## Heat map with Nacho
# Investigating the number of False spring events (-2.2 degC) that occur before climate change
# here defined as before 1984, and after climate change between Feb 15 to June 30.
# Next step is to find how many years each location has a false spring event and what the frequency of
# FSs are. 

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

raster1<-brick("tn_0.25deg_reg_v15.0.nc", varname="tn", sep="")
eur.temp<-nc_open("tn_0.25deg_reg_v15.0.nc")
raster1<-brick("//WeldShare/Wolkovich Lab/Budburst Review - Ospree/Climate Data/tn_0.25deg_reg_v15.0.nc", varname="tn", sep="")
eur.temp <- nc_open("//128.103.155.31/WeldShare/Wolkovich Lab/Budburst Review - Ospree/Climate Data/tn_0.25deg_reg_v15.0.nc")
plot(raster1[[45]])

#raster1 <- setMinMax(raster1)

#length(doy)/365
doy<-ncvar_get(eur.temp, "time")
doy<-as.Date(doy, origin="1950-01-01")
day<-substr(doy, 9,10)
year<-as.numeric(substr(doy, 1, 4))
month<-as.numeric(substr(doy, 6, 7))
timevec<-paste(year, month, day, sep="-")
years.vec<-as.character(timevec, format="Y-%m-%d")
year<-as.numeric(substr(years.vec, 1, 4))

#doy.vec<-as.POSIXlt(names(raster1), format="X%j")

dates<-as.Date(years.vec)

names(raster1)<-dates

empty.raster<-raster1[[1]]
values(empty.raster)<-NA

years<-1950:2016
leaps <- function(x) {
  m <- c()
  for(i in 1:50) {
    year.i <- years[which(((years %% 4 == 0) & (years %% 100 !=0) | (years %% 400 == 0)))]
    m <- c(m, year.i)
  }
  return(m)
}
leap.years<-as.data.frame(leaps(1))
leap.years<-leap.years[!duplicated(leaps(1)),]

#year<-1950:2016
num.false.spring.year<-list()
for(i in 1951:1983){#i=1952
  print(i)
  year.i<-i
  is.leap<-ifelse(year.i%in%leap.years,TRUE,FALSE)
  
  sequence.years<-which(year==year.i)
  #length(sequence.years)
  raster.sub<-subset(raster1,sequence.years)
  #numnonas<-sum(!is.na(values(raster.sub[[1]])))
  
  rast.array<-array(0,dim=c(ncell(raster.sub),181))
  
  if(is.leap){
    for(j in 45:181){ ## you need to change
      print(paste(year.i,j))
      rast.array[,j]<-values(raster.sub[[j]])
      
    }
  }
  
  if(!is.leap){
    for(j in 45:180){ ## you need to change
      print(paste(year.i,j))
      rast.array[,j]<-values(raster.sub[[j]])
      
    }
  }
  
  num.false.spring<-apply(rast.array,1,function(x){sum(ifelse(x<=-2.2,1,0))})
  non.nas.ids<-which(!is.na(num.false.spring))
  values(empty.raster)<- NA
  #plot(raster1[[1]])
  values(empty.raster)[non.nas.ids]<- num.false.spring[!is.na(num.false.spring)]
  #values(empty.raster)[num.false.spring]<- num.false.spring[!is.na(num.false.spring)]
  #plot(empty.raster)
  
  
  num.false.spring.year[[i]]<-empty.raster
  
}

final.raster.preCC<-stack(unlist(num.false.spring.year))
summed.false.springs.preCC<-calc(final.raster.preCC,sum) 
plot(summed.false.springs.preCC)

#write.csv(num.false.spring.year, file=("~Documents/git/regionalrisk/output/falsespring.preCC.csv"), row.names=FALSE)

num.false.spring.year.post<-list()
for(i in 1984:2016){#i=1952
  print(i)
  year.i<-i
  is.leap<-ifelse(year.i%in%leap.years,TRUE,FALSE)
  
  sequence.years.post<-which(year==year.i)
  #length(sequence.years)
  raster.sub.post<-subset(raster1,sequence.years.post)
  #numnonas<-sum(!is.na(values(raster.sub[[1]])))
  
  rast.array.post<-array(0,dim=c(ncell(raster.sub.post),181))
  
  if(is.leap){
    for(j in 45:181){ ## you need to change
      print(paste(year.i,j))
      rast.array.post[,j]<-values(raster.sub.post[[j]])
      
    }
  }
  
  if(!is.leap){
    for(j in 45:180){ ## you need to change
      print(paste(year.i,j))
      rast.array.post[,j]<-values(raster.sub.post[[j]])
      
    }
  }
  
  num.false.spring.post<-apply(rast.array.post,1,function(x){sum(ifelse(x<=-2.2,1,0))})
  non.nas.ids.post<-which(!is.na(num.false.spring.post))
  values(empty.raster)<- NA
  #plot(raster1[[1]])
  values(empty.raster)[non.nas.ids.post]<- num.false.spring.post[!is.na(num.false.spring.post)]
  #values(empty.raster)[num.false.spring]<- num.false.spring[!is.na(num.false.spring)]
  #plot(empty.raster)
  
  
  num.false.spring.year.post[[i]]<-empty.raster
  
}

final.raster.postCC<-stack(unlist(num.false.spring.year.post))
summed.false.springs.postCC<-calc(final.raster.postCC,sum) 
plot(summed.false.springs.postCC)
