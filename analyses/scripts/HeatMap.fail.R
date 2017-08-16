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

#raster1<-brick("//128.103.155.31/WeldShare/Wolkovich Lab/Budburst Review - Ospree/Climate Data/tn_0.25deg_reg_v15.0.nc", varname="tn", sep="")
eur.temp <- nc_open(file.path("tn_0.25deg_reg_v15.0.nc"))
raster1<-brick("tn_0.25deg_reg_v15.0.nc", varname="tn", sep="")
plot(raster1[[1]])
plot(raster.sub)
plot(raster1[[45]])

#length(doy)/365
#doy<-ncvar_get(eur.temp, "time")
#doy<-as.Date(doy, origin="1950-01-01")
#day<-substr(doy, 9,10)
#year<-as.numeric(substr(doy, 1, 4))
#month<-substr(doy, 6, 7)
#timevec<-paste(year, month, day, sep="-")
#years.vec<-as.character(timevec, format="Y-%m-%d")
#doy.vec<-as.POSIXlt(names(raster1), format="X%j")

#dates<-as.Date(years.vec)

#names(raster1)<-dates


#feb29s<-dates[format.Date(dates,"%m")=="02" & format.Date(dates,"%d")=="29"]
#feb29s<-paste("X",dates, sep="")
#names(raster1)<-names(raster1)[-which(names(raster1)%in%feb29s)]
#raster1<-raster1[-which(raster1%in%feb29s)]


#raster1<-is.leapyear(raster1)
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
#rename.GCM<-function(x,list.years){
 # years.i<-paste(min(list.years),max(list.years),sep="-")
  #nyears<-length(seq(min(list.years),max(list.years),1))
  #doys.names<-sort(rep(seq(1,365,1),nyears))
#years.names<-rep(seq(min(list.years),max(list.years),1),365)
  
  #if(length(names(x))==length(years.names)){
   # names(x)<-paste(rep('X',length(years.names)),years.names,doys.names,sep=".")
  #}
  
#}

## re-naming (see attached the script with a function to rename GCMs, you may need to change it according to your data, or may not even need this)
#temp.years<-1950:2016
#names(raster1)<-rename.GCM(raster1,temp.years) ## temp.years[[time.years]] – this is your period of interest (start to end of the period in the file)




#plot(raster1)

#start.date<-45
#end.date<-200
#period<-start.date:end.date


#for(i in 1950:2016){
 # print(i)
  #year.i<-i
  #if (leap_year(year.i) == TRUE){
  #  nlayers1=366
  #}else{
   # nlayers1=365
  #}
#}


#seq1<-ifelse(year%in%is.leapyear, seq(1, nlayers1, 366), seq(1, nlayers1, 365))
#seq2 <- seq(731, nlayers1, 1460)
#leaps<-c("X1952-02-29", "X1956-02-29", "X1960-02-29", "X1964-02-29", "X1968-02-29", "X1972-02-29", "X1976-02-29", "X1980-02-29",
#         "X1984-02-29", "X1988-02-29", "X1992-02-29", "X1994-02-29", "X1998-02-29", "X2002-02-29", "X2006-02-29", "X2010-02-29", 
#         "2014-02-29")

empty.raster<-raster1[[1]]
values(empty.raster)<-NA
non.nas.ids<-which(!is.na(values(raster1[[1]])))


nlayers1<-nlayers(raster1)
seq1<-seq(1,nlayers1,365)
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
years<-years[!grep(leap.years),]
yrs<-as.data.frame(years)
yrs$leapies<-yrs$years %in% leap.years
yrs$year<-ifelse(yrs$leapies==FALSE, yrs$years, NA)
non.leaps<-dplyr::select(yrs, year)
non.leaps<-na.omit(non.leaps)
non<-non.leaps$year
num.false.spring.year<-list()
for(i in non){
  print(i)
  year.i<-i
  sequence.years<-which(non==year.i)
  #raster.subbiest<-subset(raster1,sequencies)
  #nlayers1<-nlayers(raster.subbiest)
  #seq1<-seq(1, nlayers1, 365)
  #sequence.years<-seq(nlayers1[i]+45,nlayers1[i]+200,365)
  raster.sub<-subset(raster1, sequence.years)
  #numnonas<-sum(!is.na(values(raster.sub[[1]])))
                rast.array<-array(NA,dim=c(ncell(raster.sub),nlayers1)) #not sure what to make this either..
                for(j in nlayers1){#not sure what to make this...
                  print(j)
                  rast.array[,j]<-values(raster.sub[j])
                }
                
                num.false.spring<-apply(rast.array,1,function(x){sum(ifelse(x<=-2.2,1,0))})
                non.nas.ids<-which(!is.na(num.false.spring))
                values(empty.raster)<- NA
                values(empty.raster)[non.nas.ids]<- num.false.spring
                
                num.false.spring.year[[i]]<-empty.raster
                
}

final.raster<-stack(unlist(num.false.spring.year))
plot(final.raster)

year<-1950:2016
num.false.spring.year<-list()
for(i in 1950:2016){#i=1952
  print(i)
  year.i<-i
  
  sequence.years<-which(year==year.i)
  #length(sequence.years)
  raster.sub<-subset(raster1,sequence.years)
  #numnonas<-sum(!is.na(values(raster.sub[[1]])))
  
  rast.array<-array(NA,dim=c(ncell(raster.sub),365))
  for(j in 1:365){ ## you need to change
    print(j)
    rast.array[,j]<-values(raster.sub[[j]])
    
  }
  
  num.false.spring<-apply(rast.array,1,function(x){sum(ifelse(x<2.2,1,0))})
  non.nas.ids<-which(!is.na(num.false.spring))
  values(empty.raster)<- NA
  #plot(raster1[[1]])
  values(empty.raster)[non.nas.ids]<- num.false.spring[!is.na(num.false.spring)]
  #plot(empty.raster)
  
  
  num.false.spring.year[[i]]<-empty.raster
  
}



extract(final.raster,xylonlat)
summed.false.springs<-calc(final.raster,sum)   
?writeRaster
plot(summed.false.springs)
plot(final.raster)






############### Poor attempt #45 ########################
empty.raster<-raster1[[1]]
values(empty.raster)<-NA
nlayers1<-nlayers(raster1)

years<-1950:1952
num.false.spring.year<-list()
for(i in years){#i=1952
  print(i)
  year.i<-i
  is.leap<-ifelse(year.i%in%leap.years,TRUE,FALSE)
  sequence.years<-array()
  if(is.leap){
    for(j in 45:180){ ## you need to change
      print(paste(year.i,j))
      sequence.years<-seq(nlayers1[j],nlayers1[j],366)
      raster.sub[,j]<-values(sequence.years[[j]])
      
    }
  }
  
  if(!is.leap){
    for(j in 45:180){ ## you need to change
      print(paste(year.i,j))
      sequence.years<-seq(nlayers1[j],nlayers1[j],365)
      raster.sub[,j]<-values(sequence.years[[j]])
      
    }
  }
  
  #raster.sub<-subset(raster1, sequence.years)
  #sequence.years<-which(year==year.i)
  
  #length(sequence.years)
  #raster.sub<-subset(raster1,sequence.years)
  #numnonas<-sum(!is.na(values(raster.sub[[1]])))
  raster.sub<-array(NA,dim=c(ncell(raster.sub),181))
  
  num.false.spring<-apply(rast.array,1,function(x){sum(ifelse(min<=-2.2,1,0))})
  non.nas.ids<-which(!is.na(num.false.spring))
  #values(empty.raster)<- NA
  #plot(raster1[[1]])
  #values(empty.raster)[non.nas.ids]<- num.false.spring[!is.na(num.false.spring)]
  values(empty.raster)[non.nas.ids]<- num.false.spring[which(!is.na(num.false.spring))]
  #plot(empty.raster)
  
  
  num.false.spring.year[[i]]<-empty.raster
  
}


plot(stack(unlist(num.false.spring.year)))
rast.array<-array(NA,dim=c(ncell(raster.sub),181))




if(is.leap){
  for(j in 45:180){ ## you need to change
    print(paste(year.i,j))
    sequence.years<-seq(nlayers1[45],nlayers1[180],366)
    rast.array[,j]<-values(raster.sub[[j]])
    
  }
}

if(!is.leap){
  for(j in 45:180){ ## you need to change
    print(paste(year.i,j))
    sequence.years<-seq(nlayers1[45],nlayers1[180],365)
    rast.array[,j]<-values(raster.sub[[j]])
    
  }
}
