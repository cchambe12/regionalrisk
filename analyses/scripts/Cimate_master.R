## 13 July 2017 - Cat
## Cleaning up Climate data for Regional Risk paper


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

# Set Working Directory
setwd("~/Documents/git/regionalrisk/analyses/output")
#d<-read.csv("bbch_region_betula.csv", header=TRUE)
da<-read.csv("bbch_region.csv", header=TRUE)
df<-read.csv("bbch_region_fagus.csv", header=TRUE)
dt<-read.csv("bbch_region_tilia.csv", header=TRUE)

eur.tempmn <- nc_open(file.path("~/Documents/git/regionalrisk/analyses/input/tn_0.25deg_reg_v15.0.nc"))
d<-full_join(df, da)
d<-full_join(d, dt)
d<-dplyr::select(d, -X, -X.1)

all<-bb%>%filter(YEAR>=1950)
x<-paste(all$YEAR, all$DAY)
all$date<-as.Date(strptime(x, format="%Y %j"))

################## CLIMATE DATA?! ##############################
tempval <- list() 
for(i in 1:nrow(all)){ # i = 1
  # find this location
  lo <- all[i,"LON"]
  la <- all[i,"LAT"]
  
  ndiff.long.cell <- abs(eur.tempmn$dim$longitude$vals-as.numeric(lo))
  ndiff.lat.cell <- abs(eur.tempmn$dim$latitude$vals-as.numeric(la))
  nlong.cell <- which(ndiff.long.cell==min(ndiff.long.cell))[1] 
  nlat.cell <- which(ndiff.lat.cell==min(ndiff.lat.cell))[1]
  
  yr <- as.numeric(all[i,"YEAR"])#
  
  # start and end days of the climate data we need for the lat/long
  stday <- strptime(paste(yr, "01-02", sep="-"),"%Y-%m-%d", tz="GMT")#start day 
  
  # using fieldsample.date2, which is the same as fieldsampledate, but formatted as  "%Y-%m-%d"
  #field sample date2 is the end day for chilling calculations
  endday <- strptime(all[i,"date"],"%Y-%m-%d", tz = "GMT")
  
  st <- as.numeric(as.character(stday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  en <- as.numeric(as.character(endday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  if(en<st){en=st}
  if(endday<stday){endday=stday}
  # get temperature values for this date range.
  # check the dim of the netcdf file, str(netcdf), and see what the order of the different dimensions are. In this case, it goes long, lat, time. So when we are moving through the file, we give it the long and lat and date of start, then move through the files by going 'up' the cube of data to the end date
  mins <- ncvar_get(eur.tempmn, 'tn',
                    start=c(nlong.cell,nlat.cell,st),
                    count=c(1, 1,en-st+1) )# this is where we move through the 'cube' to get the one vector of Temp mins
  
  tempval[[as.character(all[i,"date"])]] <- data.frame(Lat = la,Long = lo, Date = seq(stday, endday, by="day"),
                                                       Tmin = mins)
}

###################### NEXT STEPS! ########################################

freezes <- vector()

for(i in names(tempval)){ 
  
  xx <- tempval[[i]]
  xx$Date<-strptime(xx$Date,"%Y-%m-%d", tz="GMT")
  
  year = as.numeric(format(xx$Date, "%Y"))
  month = as.numeric(format(xx$Date, "%m"))
  day = as.numeric(format(xx$Date, "%d"))
  
  lat = xx$Lat
  long = xx$Long
  
  acer = data.frame(year, month, day, Tmin = xx$Tmin, lat = xx$Lat, long = xx$Long)
  
  freezes <- rbind(freezes, data.frame(acer))
}

ggplot(freezes, aes(x=long, y=lat)) + geom_point
#save the field chilling calculations in a separate file
##### WAIT TO SAVE ONCE I HAVE ALL PARTS SETTLED! ######################
write.csv(freezes, "~/Documents/git/regionalrisk/analyses/output/climate_all.csv", row.names=FALSE, eol="\r\n")
