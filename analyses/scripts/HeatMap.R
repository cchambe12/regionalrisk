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
library(maptools)
library(rgeos)
library(rgdal)

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/input")

raster1<-brick("~/Desktop/tn_0.25deg_reg_v16.0.nc", varname="tn", sep="")
eur.temp<-nc_open("~/Desktop/tn_0.25deg_reg_v16.0.nc")
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
empty.raster<-raster1[[1]]
num.false.spring.year<-list()
#dates.false.spring<-list()
for(i in 1950:1983){#i=1952
  print(i)
  year.i<-i
  is.leap<-ifelse(year.i%in%leap.years,TRUE,FALSE)
  
  sequence.years<-which(year==year.i)
  #length(sequence.years)
  raster.sub<-subset(raster1,sequence.years)
  #numnonas<-sum(!is.na(values(raster.sub[[1]])))
  
  rast.array<-array(75,dim=c(ncell(raster.sub),181))
  
  if(is.leap){
    for(j in 75:181){ ## you need to change
      print(paste(year.i,j))
      rast.array[,j]<-values(raster.sub[[j]])
      
    }
  }
  
  if(!is.leap){
    for(j in 75:181){ ## you need to change
      print(paste(year.i,j))
      rast.array[,j]<-values(raster.sub[[j]])
      
    }
  }
  
  #dates.fs<-apply(rast.array, 1, function(x){ifelse(x<=-2.2, x, 0)})
  num.false.spring<-apply(rast.array,1,function(x){sum(ifelse(x<=-2.2,1,0))})
  
  non.nas.ids<-which(!is.na(num.false.spring))
  #values(emp.rast)<-NA
  values(empty.raster)<- NA
  #non.nas.dates<-which(!is.na(dates.fs))
  #plot(raster1[[1]])
  values(empty.raster)[non.nas.ids]<- num.false.spring[!is.na(num.false.spring)]
  #values(empty.raster)[non.nas.dates]<- dates.fs[!is.na(dates.fs)]
  #plot(empty.raster)
  #dates.false.spring[[i]]<- empty.raster
  num.false.spring.year[[i]]<-empty.raster
  
}

length(num.false.spring.year)
final.raster.preCC<-stack(unlist(num.false.spring.year))
#final.dates<-unlist(dates.false.spring)
names(final.raster.preCC)<-as.character(seq(1951,1983,1))
freezedays_pre<-rasterToPoints(final.raster.preCC)
freezies <- as.data.frame(freezedays_pre)
try<-melt(freezies, id.vars=c("x","y"))

try<-try%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(freezes=value)
try$date<-seq(1951,1983,1)
try$year<-as.numeric(substr(try$date, 0,4))
mod<-lm(freezes~year+lat+long, data=try)
display(mod)
#lm(formula = freezes ~ year + lat + long, data = try)
#             coef.est coef.se
#(Intercept) -61.51     2.43 
#year          0.00     0.00 
#lat           1.42     0.00 
#long          0.22     0.00 
#---
#  n = 996274, k = 4
#residual sd = 11.75, R-Squared = 0.67

summed.false.springs.preCC<-calc(final.raster.preCC,sum) 
mean.false.springs.preCC<-calc(final.raster.preCC,mean)
plot(summed.false.springs.preCC)
plot(mean.false.springs.preCC)
plot(final.raster.preCC)

fs.years.pre<-calc(final.raster.preCC, function(x) {sum(ifelse(x>=1,1,0))})
plot(fs.years.pre, xlim=c(-10,45), ylim=c(20,70))

writeRaster(fs.years.pre,"~/Documents/git/regionalrisk/analyses/output/fs.30.pre", bylayer=TRUE,format="GTiff", overwrite=TRUE)
writeRaster(summed.false.springs.preCC,"~/Documents/git/regionalrisk/analyses/output/total_pre", bylayer=TRUE,format="GTiff")
writeRaster(mean.false.springs.preCC,"~/Documents/git/regionalrisk/analyses/output/meanperyear_pre", bylayer=TRUE,format="GTiff", overwrite=TRUE)

#### Post Climate Change #####
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
    for(j in 75:181){ ## you need to change
      print(paste(year.i,j))
      rast.array.post[,j]<-values(raster.sub.post[[j]])
      
    }
  }
  
  if(!is.leap){
    for(j in 75:181){ ## you need to change
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

names(final.raster.postCC)<-seq(1984,2016,1)

summed.false.springs.postCC<-calc(final.raster.postCC,sum)
mean.false.springs.postCC<-calc(final.raster.postCC,mean)
plot(summed.false.springs.postCC)
plot(mean.false.springs.postCC)

fs.years.post<-calc(final.raster.postCC, function(x) {sum(ifelse(x>=1,1,0))})
fs.years.post<-calc(final.raster.postCC, function(x) {sum(ifelse(x>=1,1,0))})
plot(fs.years.post)

plot(fs.years.post, xlim=c(-10,45), ylim=c(20,70))

writeRaster(fs.years.post,"~/Documents/git/regionalrisk/analyses/output/fs.postCC", bylayer=TRUE,format="GTiff", overwrite=TRUE)
writeRaster(summed.false.springs.postCC,"~/Documents/git/regionalrisk/analyses/output/total_post", bylayer=TRUE,format="GTiff")
writeRaster(mean.false.springs.postCC,"~/Documents/git/regionalrisk/analyses/output/meanperyear_post", bylayer=TRUE,format="GTiff", overwrite=TRUE)

names(final.raster.postCC)<-as.character(seq(1951,1983,1))
freezedays_post<-rasterToPoints(final.raster.postCC)
fre.pos <- as.data.frame(freezedays_post)
try.post<-melt(fre.pos, id.vars=c("x","y"))

try.post<-try.post%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(freezes=value)
try.post$date<-seq(1951,1983,1)
try.post$year<-as.numeric(substr(try.post$date, 0,4))
mod.post<-lm(freezes~year+lat+long, data=try.post)
display(mod.post);display(mod)











############################# Bad code, just to save for now #######################

#### Attempt to look at total number of years that had a false spring...####
pre<-1951:1983
fs.comb.pre<- list()
emp.rast.pre<-final.raster.preCC[[1]]
for(i in 1951:1983){
  print(i)
  fs.array<-array(NA, dim=c(ncell(final.raster.preCC),33))
  fs.combined<-apply(fs.array, 1, function(x) {sum(ifelse(x>0, 1, 0))})
  no.nas<-which(!is.na(fs.combined))
  values(emp.rast.pre)<-NA
  values(emp.rast.pre)[no.nas]<-fs.combined[!is.na(fs.combined)]
  fs.comb.pre[[i]]<-emp.rast.pre
}

final.fs.pre<-stack(unlist(fs.comb.pre))
summed.final.fs.pre<-calc(final.fs.pre, sum)
plot(summed.final.fs.pre)

######## For some reason, my new raster names are "X1950.01.01.1.1" - which shouldn't the first loop
# parsed down the names to "X1950-02-15" through "X1950-06-30"? And not sure how on earth I managed to add
# the other characters in the name!

# Another failed attempt... #
for(i in 1951:1983){
  print(i)
  fs.attempt<-as.array(as.numeric(final.raster.preCC[i]))
  final.attempt<-apply(fs.attempt,1,function(x){sum(ifelse(x>=1,1,0))})
  no.nas<-which(!is.na(final.attempt))
  values(emp.rast.pre)<-NA
  values(emp.rast.pre)[no.nas]<-final.attempt[!is.na(final.attempt)]
  fs.comb.pre[[i]]<-emp.rast.pre
}

final.fs.pre<-stack(unlist(fs.comb.pre))
summed.final.fs.pre<-calc(final.fs.pre, sum)
plot(summed.final.fs.pre)


############################## MAKE BETTER MAPS!! #######################################
setwd("~/Documents/git/regionalrisk/analyses")
land<-readShapeSpatial("input/natural_earth_vector/50m_physical/ne_50m_land.shp") ## 
boundars<-readShapeSpatial("input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")


plot(fs.years.pre,ylim=c(30,70),xlim=c(-15,35))
plot(fs.years.post,ylim=c(30,70),xlim=c(-15,35))









