## Started 8 March 2018 ##
## By Cat ##

## Working on extra plots for raw data ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)
library(brms)


# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


#bb<-read.csv("output/fs_matspspace.csv", header=TRUE)
bb<-read.csv("output/fs_matspring.csv", header=TRUE)

prep_cc<-bb
prep_cc$fs.num<-ave(prep_cc$fs, prep_cc$lat.long, prep_cc$cc,prep_cc$species, FUN=sum)
prep_cc$mat.cc<-ave(prep_cc$mat, prep_cc$lat.long, prep_cc$species)
fs.cc<-dplyr::select(prep_cc, fs.num, mat.cc, cc, space, species)
#fs.cc$species<-as.numeric(as.factor(fs.cc$species))
fs.cc<-fs.cc[!duplicated(fs.cc),]

ggplot(fs.cc, aes(x=mat.cc, y=fs.num, col=species)) + geom_smooth(aes(x=mat.cc, y=fs.num, col=species), 
                                                                  method="lm", se=FALSE) + ylim(0,8) +
  xlim(0,12)

fs.cc$cc<-ifelse(fs.cc$cc==0, "pre 1983", "post 1983")
ggplot(fs.cc, aes(x=mat.cc, y=fs.num, col=cc)) + geom_smooth(aes(x=mat.cc, y=fs.num, col=cc), 
                                                                  method="lm", se=FALSE)

ggplot(prep_cc, aes(x=year, y=fs.num))+ geom_point(aes(col=species))


dxx<-bb
dxx$fs.yr<-ave(dxx$fs, dxx$year, FUN=sum)
dxx$fs.yrspp<-ave(dxx$fs, dxx$species, dxx$year, FUN=sum)
### Sites per species -
#length(unique(dxx$lat.long[dxx$species=="AESHIP"])) # 10191
#length(unique(dxx$lat.long[dxx$species=="ALNGLU"])) # 6775
#length(unique(dxx$lat.long[dxx$species=="BETPEN"])) # 10168
#length(unique(dxx$lat.long[dxx$species=="FAGSYL"])) # 9127
#length(unique(dxx$lat.long[dxx$species=="FRAEXC"])) # 7327
#length(unique(dxx$lat.long[dxx$species=="QUEROB"])) # 8831
dxx$spp.prop<-NA
dxx$spp.sites<-as.numeric(ave(dxx$lat.long, dxx$year, dxx$species, FUN=length))
dxx$spp.prop<-ifelse(dxx$species=="AESHIP", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="ALNGLU", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="BETPEN", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="FAGSYL", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="FRAEXC", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.prop<-ifelse(dxx$species=="QUEROB", dxx$fs.yrspp/dxx$spp.sites, dxx$spp.prop)
dxx$spp.ave<-ave(dxx$spp.prop, dxx$species, FUN=median)

dxx$num.sites<-as.numeric(ave(dxx$lat.long, dxx$year, FUN=length))
dxx$fs.prop<-dxx$fs.yr/dxx$num.sites

dxx$fs.ave<-ave(dxx$fs.prop)


#ggplot(dxx, aes(x=year, y=fs.yr)) + geom_point() + xlab("Year") + ylab("Number of False Springs")
all<-ggplot(dxx, aes(x=year, y=fs.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites with False Springs") + 
  geom_hline(yintercept=dxx$fs.ave, linetype="dashed", color="blue")

############### Look at each individual species now... ###################
aeship<-subset(dxx, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=year, y=spp.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=aeship$spp.ave, linetype="dashed", color="firebrick3") + coord_cartesian(ylim=c(0,0.65)) +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + annotate("text",label= "Avg. Day of Budburst = 99.24", col="firebrick3", x=1980, y=0.65, size=2) #+
  #geom_line(aes(y=bb.yr/500), col="blue") + scale_y_continuous(sec.axis = sec_axis(~.*500, name="Avg Day of Budburst"))
alnglu<-subset(dxx, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=year, y=spp.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=alnglu$spp.ave, linetype="dashed", color="orangered1") + coord_cartesian(ylim=c(0, 0.65))+
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + annotate("text",label= "Avg. Day of Budburst = 98.91", col="orangered1", x=1980, y=0.65, size=2)
#geom_line(aes(y=bb.yr/500), col="blue") + scale_y_continuous(sec.axis = sec_axis(~.*500, name="Avg Day of Budburst"))
betpen<-subset(dxx, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=year, y=spp.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=betpen$spp.ave, linetype="dashed", color="orange3") + coord_cartesian(ylim=c(0, 0.65)) +
  ggtitle(expression(paste(italic("Betula pendula")))) + annotate("text",label= "Avg. Day of Budburst = 98.77", col="orange3", x=1980, y=0.65, size=2)
  #geom_line(aes(y=bb.yr/500), col="blue") + scale_y_continuous(sec.axis = sec_axis(~.*500, name="Avg Day of Budburst"))
fagsyl<-subset(dxx, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=year, y=spp.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=fagsyl$spp.ave, linetype="dashed", color="sienna2") + coord_cartesian(ylim=c(0, 0.65)) +
  ggtitle(expression(paste(italic("Fagus sylvatica")))) + annotate("text",label= "Avg. Day of Budburst = 106.74", col="sienna2", x=1980, y=0.65, size=2)
  #geom_line(aes(y=bb.yr/500), col="blue") + scale_y_continuous(sec.axis = sec_axis(~.*500, name="Avg Day of Budburst"))
fraexc<-subset(dxx, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=year, y=spp.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=fraexc$spp.ave, linetype="dashed", color="green4") + coord_cartesian(ylim=c(0, 0.65)) +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + annotate("text",label= "Avg. Day of Budburst = 116.34", col="green4", x=1980, y=0.65, size=2)
  #geom_line(aes(y=bb.yr/500), col="blue") + scale_y_continuous(sec.axis = sec_axis(~.*500, name="Avg Day of Budburst"))
querob<-subset(dxx, species=="QUEROB")
qrob<-ggplot(querob, aes(x=year, y=spp.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=querob$spp.ave, linetype="dashed", color="purple2") + coord_cartesian(ylim=c(0, 0.65)) +
  ggtitle(expression(paste(italic("Quercus robur")))) + annotate("text",label= "Avg. Day of Budburst = 113.04", col="purple2", x=1980, y=0.65, size=2)
  #geom_line(aes(y=bb.yr/500), col="blue") + scale_y_continuous(sec.axis = sec_axis(~.*500, name="Avg Day of Budburst"))

quartz()
ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)

############# Add in Budburst data? ###########################
bb.aes<-read.csv("output/bbch_region_aesculus.csv", header=TRUE)
bb.ag<-read.csv("output/bbch_region_alnus.csv", header=TRUE)
bb.bp<-read.csv("output/bbch_region_betula.csv", header=TRUE)
bb.fsyl<-read.csv("output/bbch_region_fagus.csv", header=TRUE)
bb.fex<-read.csv("output/bbch_region_fraxinus.csv", header=TRUE)
bb.qr<-read.csv("output/bbch_region_quercus.csv", header=TRUE)

## Using BBCH 11 for analysis
# AESHIP
bb.aes<-bb.aes%>%filter(BBCH==11)%>%filter(YEAR>=1950)
bb.aes$leafout<-ave(bb.aes$DAY) ## 111.24
bb.aes$bb<-bb.aes$DAY-12
bb.aes$cc<-ifelse(bb.aes$YEAR<=1983, 0, 1)
bb.aes$bb.cc<-ave(bb.aes$bb, bb.aes$cc) ## 0 = 114.27 & 1 = 107.28
bb.aes$decade<-substr(bb.aes$YEAR,3,3)
bb.aes$bb.decade<-NA
bb.aes$bb.decade<-ifelse(bb.aes$decade==5, 1, bb.aes$bb.dec)
bb.aes$bb.decade<-ifelse(bb.aes$decade==6, 2, bb.aes$bb.dec)
bb.aes$bb.decade<-ifelse(bb.aes$decade==7, 3, bb.aes$bb.dec)
bb.aes$bb.decade<-ifelse(bb.aes$decade==8, 4, bb.aes$bb.dec)
bb.aes$bb.decade<-ifelse(bb.aes$decade==9, 5, bb.aes$bb.dec)
bb.aes$bb.decade<-ifelse(bb.aes$decade==0, 6, bb.aes$bb.dec)
bb.aes$bb.decade<-ifelse(bb.aes$decade==1, 7, bb.aes$bb.dec)
bb.aes$bb.dec<-ave(bb.aes$bb, bb.aes$bb.decade)
## 50s=114.76; 60s=114.28; 70s=115.11; 80s=112.93; 90s=107.01; 00s=103.41; 10s=103.98
bb.aes$bb.yr<-ave(bb.aes$bb, bb.aes$YEAR)
bb.aes$bb.space<-ave(bb.aes$bb, bb.aes$PEP_ID)
bb.aes<-rename(bb.aes, year=YEAR)
bb.aes<-dplyr::select(bb.aes, species, year, bb, cc, bb.cc, decade, bb.dec, bb.yr, bb.space, LAT, LON, ALT)
bb.aes<-bb.aes[!duplicated(bb.aes),]

# ALNGLU
bb.ag<-bb.ag%>%filter(BBCH==11)%>%filter(YEAR>=1950)
bb.ag$leafout<-ave(bb.ag$DAY) 
bb.ag$bb<-bb.ag$DAY-12
bb.ag$cc<-ifelse(bb.ag$YEAR<=1983, 0, 1)
bb.ag$bb.cc<-ave(bb.ag$bb, bb.ag$cc) ## 0 = 114.88 & 1 = 106.93
bb.ag$decade<-substr(bb.ag$YEAR,3,3)
bb.ag$bb.decade<-NA
bb.ag$bb.decade<-ifelse(bb.ag$decade==5, 1, bb.ag$bb.dec)
bb.ag$bb.decade<-ifelse(bb.ag$decade==6, 2, bb.ag$bb.dec)
bb.ag$bb.decade<-ifelse(bb.ag$decade==7, 3, bb.ag$bb.dec)
bb.ag$bb.decade<-ifelse(bb.ag$decade==8, 4, bb.ag$bb.dec)
bb.ag$bb.decade<-ifelse(bb.ag$decade==9, 5, bb.ag$bb.dec)
bb.ag$bb.decade<-ifelse(bb.ag$decade==0, 6, bb.ag$bb.dec)
bb.ag$bb.decade<-ifelse(bb.ag$decade==1, 7, bb.ag$bb.dec)
bb.ag$bb.dec<-ave(bb.ag$bb, bb.ag$bb.decade)
## 50s=118.65; 60s=116.06; 70s=114.20; 80s=112.30; 90s=104.96; 00s=105.98; 10s=104.74
bb.ag$bb.yr<-ave(bb.ag$bb, bb.ag$YEAR)
bb.ag$bb.space<-ave(bb.ag$bb, bb.ag$PEP_ID)
bb.ag<-rename(bb.ag, year=YEAR)
bb.ag<-dplyr::select(bb.ag, species, year, bb, cc, bb.cc, decade, bb.dec, bb.yr, bb.space, LAT, LON, ALT)
bb.ag<-bb.ag[!duplicated(bb.ag),]

# BETPEN
bb.bp<-bb.bp%>%filter(BBCH==11)%>%filter(YEAR>=1950)
bb.bp$leafout<-ave(bb.bp$DAY) 
bb.bp$bb<-bb.bp$DAY-12
bb.bp$cc<-ifelse(bb.bp$YEAR<=1983, 0, 1)
bb.bp$bb.cc<-ave(bb.bp$bb, bb.bp$cc) ## 0 = 113.38 & 1 = 107.46
bb.bp$decade<-substr(bb.bp$YEAR,3,3)
bb.bp$bb.decade<-NA
bb.bp$bb.decade<-ifelse(bb.bp$decade==5, 1, bb.bp$bb.dec)
bb.bp$bb.decade<-ifelse(bb.bp$decade==6, 2, bb.bp$bb.dec)
bb.bp$bb.decade<-ifelse(bb.bp$decade==7, 3, bb.bp$bb.dec)
bb.bp$bb.decade<-ifelse(bb.bp$decade==8, 4, bb.bp$bb.dec)
bb.bp$bb.decade<-ifelse(bb.bp$decade==9, 5, bb.bp$bb.dec)
bb.bp$bb.decade<-ifelse(bb.bp$decade==0, 6, bb.bp$bb.dec)
bb.bp$bb.decade<-ifelse(bb.bp$decade==1, 7, bb.bp$bb.dec)
bb.bp$bb.dec<-ave(bb.bp$bb, bb.bp$bb.decade)
## 50s=114.31; 60s=113.13; 70s=113.73; 80s=112.75; 90s=105.77; 00s=105.59; 10s=103.90
bb.bp$bb.yr<-ave(bb.bp$bb, bb.bp$YEAR)
bb.bp$bb.space<-ave(bb.bp$bb, bb.bp$PEP_ID)
bb.bp<-rename(bb.bp, year=YEAR)
bb.bp<-dplyr::select(bb.bp, species, year, bb, cc, bb.cc, decade, bb.dec, bb.yr, bb.space, LAT, LON, ALT)
bb.bp<-bb.bp[!duplicated(bb.bp),]

# FAGSYL
bb.fsyl<-bb.fsyl%>%filter(BBCH==11)%>%filter(YEAR>=1950)
bb.fsyl$leafout<-ave(bb.fsyl$DAY)
bb.fsyl$bb<-bb.fsyl$DAY-12
bb.fsyl$cc<-ifelse(bb.fsyl$YEAR<=1983, 0, 1)
bb.fsyl$bb.cc<-ave(bb.fsyl$bb, bb.fsyl$cc) ## 0 = 121.11 & 1 = 115.74
bb.fsyl$decade<-substr(bb.fsyl$YEAR,3,3)
bb.fsyl$bb.decade<-NA
bb.fsyl$bb.decade<-ifelse(bb.fsyl$decade==5, 1, bb.fsyl$bb.dec)
bb.fsyl$bb.decade<-ifelse(bb.fsyl$decade==6, 2, bb.fsyl$bb.dec)
bb.fsyl$bb.decade<-ifelse(bb.fsyl$decade==7, 3, bb.fsyl$bb.dec)
bb.fsyl$bb.decade<-ifelse(bb.fsyl$decade==8, 4, bb.fsyl$bb.dec)
bb.fsyl$bb.decade<-ifelse(bb.fsyl$decade==9, 5, bb.fsyl$bb.dec)
bb.fsyl$bb.decade<-ifelse(bb.fsyl$decade==0, 6, bb.fsyl$bb.dec)
bb.fsyl$bb.decade<-ifelse(bb.fsyl$decade==1, 7, bb.fsyl$bb.dec)
bb.fsyl$bb.dec<-ave(bb.fsyl$bb, bb.fsyl$bb.decade)
## 50s=120.11; 60s=120.27; 70s=122.27; 80s=120.94; 90s=115.74; 00s=114.00; 10s=111.63
bb.fsyl$bb.yr<-ave(bb.fsyl$bb, bb.fsyl$YEAR)
bb.fsyl$bb.space<-ave(bb.fsyl$bb, bb.fsyl$PEP_ID)
bb.fsyl<-rename(bb.fsyl, year=YEAR)
bb.fsyl<-dplyr::select(bb.fsyl, species, year, bb, cc, bb.cc, decade, bb.dec, bb.yr, bb.space, LAT, LON, ALT)
bb.fsyl<-bb.fsyl[!duplicated(bb.fsyl),]

# FRAEXC
bb.fex<-bb.fex%>%filter(BBCH==11)%>%filter(YEAR>=1950)
bb.fex$leafout<-ave(bb.fex$DAY)
bb.fex$bb<-bb.fex$DAY-12
bb.fex$cc<-ifelse(bb.fex$YEAR<=1983, 0, 1)
bb.fex$bb.cc<-ave(bb.fex$bb, bb.fex$cc) ## 0 = 131.43 & 1 = 125.52
bb.fex$decade<-substr(bb.fex$YEAR,3,3)
bb.fex$bb.decade<-NA
bb.fex$bb.decade<-ifelse(bb.fex$decade==5, 1, bb.fex$bb.dec)
bb.fex$bb.decade<-ifelse(bb.fex$decade==6, 2, bb.fex$bb.dec)
bb.fex$bb.decade<-ifelse(bb.fex$decade==7, 3, bb.fex$bb.dec)
bb.fex$bb.decade<-ifelse(bb.fex$decade==8, 4, bb.fex$bb.dec)
bb.fex$bb.decade<-ifelse(bb.fex$decade==9, 5, bb.fex$bb.dec)
bb.fex$bb.decade<-ifelse(bb.fex$decade==0, 6, bb.fex$bb.dec)
bb.fex$bb.decade<-ifelse(bb.fex$decade==1, 7, bb.fex$bb.dec)
bb.fex$bb.dec<-ave(bb.fex$bb, bb.fex$bb.decade)
## 50s=131.69; 60s=129.84; 70s=132.31; 80s=132.16; 90s=125.35; 00s=123.19; 10s=121.47
bb.fex$bb.yr<-ave(bb.fex$bb, bb.fex$YEAR)
bb.fex$bb.space<-ave(bb.fex$bb, bb.fex$PEP_ID)
bb.fex<-rename(bb.fex, year=YEAR)
bb.fex<-dplyr::select(bb.fex, species, year, bb, cc, bb.cc, decade, bb.dec, bb.yr, bb.space, LAT, LON, ALT)
bb.fex<-bb.fex[!duplicated(bb.fex),]

# QUEROB
bb.qr<-bb.qr%>%filter(BBCH==11)%>%filter(YEAR>=1950)
bb.qr$leafout<-ave(bb.qr$DAY)
bb.qr$bb<-bb.qr$DAY-12
bb.qr$cc<-ifelse(bb.qr$YEAR<=1983, 0, 1)
bb.qr$bb.cc<-ave(bb.qr$bb, bb.qr$cc) ## 0 = 127.93.59 & 1 = 121.62.31
bb.qr$decade<-substr(bb.qr$YEAR,3,3)
bb.qr$bb.decade<-NA
bb.qr$bb.decade<-ifelse(bb.qr$decade==5, 1, bb.qr$bb.dec)
bb.qr$bb.decade<-ifelse(bb.qr$decade==6, 2, bb.qr$bb.dec)
bb.qr$bb.decade<-ifelse(bb.qr$decade==7, 3, bb.qr$bb.dec)
bb.qr$bb.decade<-ifelse(bb.qr$decade==8, 4, bb.qr$bb.dec)
bb.qr$bb.decade<-ifelse(bb.qr$decade==9, 5, bb.qr$bb.dec)
bb.qr$bb.decade<-ifelse(bb.qr$decade==0, 6, bb.qr$bb.dec)
bb.qr$bb.decade<-ifelse(bb.qr$decade==1, 7, bb.qr$bb.dec)
bb.qr$bb.dec<-ave(bb.qr$bb, bb.qr$bb.decade)
## 50s=127.14; 60s=126.64; 70s=129.52; 80s=127.58; 90s=122.1; 00s=118.37; 10s=116.33
bb.qr$bb.yr<-ave(bb.qr$bb, bb.qr$YEAR)
bb.qr$bb.space<-ave(bb.qr$bb, bb.qr$PEP_ID)
bb.qr<-rename(bb.qr, year=YEAR)
bb.qr<-dplyr::select(bb.qr, species, year, bb, cc, bb.cc, decade, bb.dec, bb.yr, bb.space, LAT, LON, ALT)
bb.qr<-bb.qr[!duplicated(bb.qr),]

d<-full_join(bb.aes, bb.ag)
d<-full_join(d, bb.bp)
d<-full_join(d, bb.fsyl)
d<-full_join(d, bb.fex)
d<-full_join(d, bb.qr)

#dxx<-inner_join(dxx, d)
write.csv(d, file="~/Documents/git/regionalrisk/analyses/output/fs_bb_sitedata.csv", row.names = FALSE)
###############################################################
# Relationship between budburst date and false spring incidence?
dxx<-read.csv("output/fs_bb_sitedata.csv", header=TRUE)
prep_bb<-dxx
#prep_bb$lat<-round(prep_bb$lat, digits=0)
#prep_bb$long<-round(prep_bb$long, digits=0)
bb.mod<-brm(fs~year+bb.yr+(1|species), data=prep_bb, family=bernoulli)

prep_dec<-dxx
prep_dec$bb.decade<-NA
prep_dec$bb.decade<-ifelse(prep_dec$decade==5, 1, prep_dec$bb.decade)
prep_dec$bb.decade<-ifelse(prep_dec$decade==6, 2, prep_dec$bb.decade)
prep_dec$bb.decade<-ifelse(prep_dec$decade==7, 3, prep_dec$bb.decade)
prep_dec$bb.decade<-ifelse(prep_dec$decade==8, 4, prep_dec$bb.decade)
prep_dec$bb.decade<-ifelse(prep_dec$decade==9, 5, prep_dec$bb.decade)
prep_dec$bb.decade<-ifelse(prep_dec$decade==0, 6, prep_dec$bb.decade)
prep_dec$bb.decade<-ifelse(prep_dec$decade==1, 7, prep_dec$bb.decade)
prep_dec$fs.num<-ave(prep_dec$fs, prep_dec$lat.long, prep_dec$bb.decade, prep_dec$species, FUN=sum)
prep_dec$lat<-round(prep_dec$lat, digits=0)
prep_dec$long<-round(prep_dec$long, digits=0)
prep_dec<-dplyr::select(prep_dec, fs.num, bb.dec, bb.decade, lat, long, species)
prep_dec<-prep_dec[!duplicated(prep_dec),]
dec.mod<-brm(fs.num~bb.dec+bb.decade+lat+long+(1|species), data=prep_dec, family=poisson)
## Not very interesting... maybe try decade as a factor?





###############################################################
lats<-bb
lats<- lats[with(lats, order(lat)), ]
lats$lat.range<-NA
lats$lat.range[1:366182]<-"low" ### lats = 41.25 to 50.10
lats$lat.range[366183:732365]<-"med" ### from 50.1 to 51.83
lats$lat.range[732366:1098546]<-"high" ### from 51.833 to 69.75

lats$lat.range<-ifelse(lats$lat>=40 & lats$lat<50, "low", lats$lat.range) 
lats$lat.range<-ifelse(lats$lat>=50 & lats$lat<52, "med", lats$lat.range) 
lats$lat.range<-ifelse(lats$lat>=52 & lats$lat<70, "high", lats$lat.range) 

lats$fs.num<-ave(lats$fs, lats$year, lats$lat.range,  FUN=sum)

lats$lat.prop<-NA
length(unique(lats$lat.long[lats$lat.range=="low"])) # 2959 ## should I split so even number of sites?
length(unique(lats$lat.long[lats$lat.range=="med"])) # 4450
length(unique(lats$lat.long[lats$lat.range=="high"])) # 4275
lats$lat.prop<-ifelse(lats$lat.range=="low", lats$fs.num/2959, lats$lat.prop)
lats$lat.prop<-ifelse(lats$lat.range=="med", lats$fs.num/4450, lats$lat.prop)
lats$lat.prop<-ifelse(lats$lat.range=="high", lats$fs.num/4275, lats$lat.prop)
lats$lat.ave<-ave(lats$lat.prop, lats$lat.range)

low<-subset(lats, lat.range=="low")
plow<-ggplot(low, aes(x=year, y=lat.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=low$lat.ave, linetype="dashed", color="blue") +
  ggtitle("Latitude: 40-49") + coord_cartesian(ylim=c(0,0.7))

med<-subset(lats, lat.range=="med")
pmed<-ggplot(med, aes(x=year, y=lat.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=med$lat.ave, linetype="dashed", color="blue") +
  ggtitle("Latitude: 50-55") + coord_cartesian(ylim=c(0,0.7))

high<-subset(lats, lat.range=="high")
phigh<-ggplot(high, aes(x=year, y=lat.prop)) + geom_line() + xlab("Year") + ylab("Proportion of Sites \n with False Springs") + 
  geom_hline(yintercept=high$lat.ave, linetype="dashed", color="blue") +
  ggtitle("Latitude: 55-70") + coord_cartesian(ylim=c(0,0.7))

ggarrange(plow, pmed, phigh, nrow=1)



