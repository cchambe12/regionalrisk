## Started 28 March 2018 ##
## By Cat ##

## Using elevation and NAO index and spring mean annual temperature ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


#library(rstan)
library(ggplot2)
#library(shinystan)
#library(bayesplot)
library(rstanarm)
library(dplyr)
library(tidyr)
library(brms)
library(ggstance)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data
#bb<-read.csv("output/fs_matspspace.csv", header=TRUE)
bb<-read.csv("output/fs_matspring.csv", header=TRUE)
mat<-read.csv("output/fs_bb_sitedata.csv", header=TRUE)

## fix KNMI Climate Explorer data for NAO - https://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/cpc_nao&STATION=CPC_NAO&TYPE=i&id=someone@somewhere
#setwd("~/Documents/git/regionalrisk")
#nao<-read.delim("data/icpc_nao.dat.txt", header=TRUE)
#write.csv(nao, "~/Documents/git/regionalrisk/analyses/output/icpc_nao.csv", row.names=FALSE)
nao<-read.csv("output/icpc_nao.csv", header=TRUE)
nf<-nao%>%gather(month, index, -year)
nf$m.index<-ave(nf$index, nf$year)
nx<-dplyr::select(nf, year, m.index)
nx<-nx[!duplicated(nx),]
nx<-filter(nx, year<=2016)

#### Get elevation information
bb<-bb%>%rename(sp.temp=pre.bb)
bb<-dplyr::select(bb, -fs.count, -PEP_ID)
bb<-bb[!duplicated(bb),]
mat<-mat%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
mat<-dplyr::select(mat, species, lat, long, elev)
mat<-mat[!duplicated(mat),]
d<-inner_join(bb, mat)
d<-inner_join(d, nx)

#d$cc<-NA
#d$cc<-ifelse(d$year<=1970&d$year>=1950, 0, d$cc)
#d$cc<-ifelse(d$year>=1996&d$year<=2016, 1, d$cc)
#d<-d[!is.na(d$cc),]
d$cc<-ifelse(d$year>=1950&d$year<=1983, 0, 1)
d$elev<-d$elev+1
d$fs.num<-ave(d$fs, d$lat.long, d$species, d$cc, FUN=sum)
d$sp.temp<-ave(d$sp.temp, d$cc, d$lat.long)
fs.cc<-dplyr::select(d, fs.num, sp.temp, elev, cc, species)
fs.cc$species<-as.numeric(as.factor(fs.cc$species))
fs.cc<-fs.cc[!duplicated(fs.cc),]
fs.cc<-fs.cc[!is.na(fs.cc$sp.temp),]
fs.cc<-fs.cc[!is.na(fs.cc$elev),]
fs.cc$elev<-ifelse(fs.cc$elev<=500, 0, 1)
fs.cc<-fs.cc[!duplicated(fs.cc),]

fit<-stan_glmer(fs.num~sp.temp+elev+cc+(1|species), data=fs.cc, family=poisson, chains=2)
## nothing with elevation... spatial autocorrelation issues?

ele.brm<-brm(fs.num~sp.temp+elev+cc+(1|species)+(sp.temp-1|species) + (elev-1|species) +
               (cc-1|species), data=fs.cc, family=poisson)

m<-ele.brm
m.int<-posterior_interval(m)
sum.m<-summary(m)
cri.f<-as.data.frame(sum.m$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f<-cri.f[-1,] #removing the intercept 
fdf1<-as.data.frame(rbind(as.vector(cri.f[,1]), as.vector(cri.f[,2]), as.vector(cri.f[,3])))
fdf2<-cbind(fdf1, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2)<-c(rownames(cri.f), "species", "perc")

cri.r<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$species
cri.r2<-cri.r[, ,-1]
cri.r2<-cri.r2[,-2,]
dims<-dim(cri.r2)
twoDimMat <- matrix(cri.r2, prod(dims[1:2]), dims[3])
mat2<-cbind(twoDimMat, c(rep(1:6, length.out=18)), rep(c("Estimate", "2.5%", "95%"), each=6))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "species", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, sp.temp:cc, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=21)) {
  for (j in seq(from=3, to=20, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$species>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$species<-as.factor(dfwide$species)
## plotting

pd <- position_dodgev(height = -0.5)

estimates<-c("Mean Spring Temperature", "Elevation", "CC")
dfwide$legend<-factor(dfwide$species,
                      labels=c("Overall Effects","Aesculus hippocastanum","Alnus glutinosa",
                               "Betula pendula","Fagus sylvatica","Fraxinus excelsior",
                               "Quercus robur"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue", "firebrick3", "orangered1","orange3", "sienna2", "green4", "purple2"),
                      labels=c("Overall Effects",
                               "Aesculus hippocastanum" = expression(paste(italic("Aesculus hippocastanum"))),
                               "Alnus glutinosa" = expression(paste(italic("Alnus glutinosa"))),
                               "Betula pendula" = expression(paste(italic("Betula pendula"))),
                               "Fagus sylvatica" = expression(paste(italic("Fagus sylvatica"))),
                               "Fraxinus excelsior" = expression(paste(italic("Fraxinus excelsior"))),
                               "Quercus robur" = expression(paste(italic("Quercus robur")))))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) +  
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.2, "cm"),
                              legend.text=element_text(size=8), legend.position= c(0.9,0.9)) #+
#xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
fig1



####### Try NAO model... ########
nao.mod<-dplyr::select(d, fs, sp.temp, elev, species, m.index)
nao.mod<-nao.mod[!duplicated(nao.mod),]
nao.mod<-nao.mod[!is.na(nao.mod$sp.temp),]
nao.mod<-nao.mod[!is.na(nao.mod$elev),]
nao.mod<-nao.mod[!is.na(nao.mod$m.index),]
nao.mod$species<-as.numeric(as.factor(nao.mod$species))
nao.mod$elev<-ifelse(nao.mod$elev<=500, 0, 1)

n.stan<-stan_glm(fs~sp.temp+elev+m.index+species, data=nao.mod, family=binomial(link="logit"))
n.brm<-brm(fs~sp.temp+elev+m.index+(1|species)+(sp.temp-1|species)+(elev-1|species)+(m.index-1|species), data=nao.mod, family=bernoulli)


m<-n.brm
m.int<-posterior_interval(m)
sum.m<-summary(m)
cri.f<-as.data.frame(sum.m$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f<-cri.f[-1,] #removing the intercept 
fdf1<-as.data.frame(rbind(as.vector(cri.f[,1]), as.vector(cri.f[,2]), as.vector(cri.f[,3])))
fdf2<-cbind(fdf1, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2)<-c(rownames(cri.f), "species", "perc")

cri.r<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$species
cri.r2<-cri.r[, ,-1]
cri.r2<-cri.r2[,-2,]
dims<-dim(cri.r2)
twoDimMat <- matrix(cri.r2, prod(dims[1:2]), dims[3])
mat2<-cbind(twoDimMat, c(rep(1:6, length.out=18)), rep(c("Estimate", "2.5%", "95%"), each=6))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "species", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, sp.temp:m.index, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=21)) {
  for (j in seq(from=3, to=20, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$species>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$species<-as.factor(dfwide$species)
## plotting

pd <- position_dodgev(height = -0.5)

estimates<-c("Mean Spring Temperature", "Elevation", "NAO Index")
dfwide$legend<-factor(dfwide$species,
                      labels=c("Overall Effects","Aesculus hippocastanum","Alnus glutinosa",
                               "Betula pendula","Fagus sylvatica","Fraxinus excelsior",
                               "Quercus robur"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue", "firebrick3", "orangered1","orange3", "sienna2", "green4", "purple2"),
                      labels=c("Overall Effects",
                               "Aesculus hippocastanum" = expression(paste(italic("Aesculus hippocastanum"))),
                               "Alnus glutinosa" = expression(paste(italic("Alnus glutinosa"))),
                               "Betula pendula" = expression(paste(italic("Betula pendula"))),
                               "Fagus sylvatica" = expression(paste(italic("Fagus sylvatica"))),
                               "Fraxinus excelsior" = expression(paste(italic("Fraxinus excelsior"))),
                               "Quercus robur" = expression(paste(italic("Quercus robur")))))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) +  
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.2, "cm"),
                              legend.text=element_text(size=8), legend.position= c(0.9,0.9)) #+
#xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
fig1

## Maybe try the proportion of sites rather than each site...?
dxx<-d
dxx$fs.yr<-ave(dxx$fs, dxx$year, FUN=sum)
dxx$fs.yrspp<-ave(dxx$fs, dxx$species, dxx$year, FUN=sum)
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


#dxx$fs.ave<-ave(dxx$fs.prop)
prop<-dxx%>%dplyr::select(spp.prop, species, m.index, year, fs.prop, sp.temp)
prop$cc<-ifelse(prop$year<=1983, 0, 1)
prop$cc.20<-NA
prop$cc.20<-ifelse(prop$year<=1970 & prop$year>=1950, 0, prop$cc.20)
prop$cc.20<-ifelse(prop$year<=2016 & prop$year>=1996, 1, prop$cc.20)
prop<-prop[!is.na(prop$sp.temp),]
prop$sp.temp<-ave(prop$sp.temp, prop$year)
prop<-prop[!duplicated(prop),]
prop$fs.tot<-prop$fs.prop*100

prop$spp.tot<-prop$spp.prop*100
prop.mod<-brm(fs.prop~m.index+cc+(m.index-1|species)+(cc-1|species), data=prop)



m<-sp20.mod
m.int<-posterior_interval(m)
sum.m<-summary(m)
cri.f<-as.data.frame(sum.m$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f<-cri.f[-1,] #removing the intercept 
fdf1<-as.data.frame(rbind(as.vector(cri.f[,1]), as.vector(cri.f[,2]), as.vector(cri.f[,3])))
fdf2<-cbind(fdf1, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2)<-c(rownames(cri.f), "species", "perc")

cri.r<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$species
cri.r2<-cri.r[, ,-1]
cri.r2<-cri.r2[,-2,]
dims<-dim(cri.r2)
twoDimMat <- matrix(cri.r2, prod(dims[1:2]), dims[3])
mat2<-cbind(twoDimMat, c(rep(1:6, length.out=18)), rep(c("Estimate", "2.5%", "95%"), each=6))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "species", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, nao:`nao:cc`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=21)) {
  for (j in seq(from=3, to=20, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$species>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$species<-as.factor(dfwide$species)
## plotting

pd <- position_dodgev(height = -0.5)

estimates<-c("NAO Index", "CC", "Nao Index x CC")
dfwide$legend<-factor(dfwide$species,
                      labels=c("Overall Effects","Aesculus hippocastanum","Alnus glutinosa",
                               "Betula pendula","Fagus sylvatica","Fraxinus excelsior",
                               "Quercus robur"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue", "firebrick3", "orangered1","orange3", "sienna2", "green4", "purple2"),
                      labels=c("Overall Effects",
                               "Aesculus hippocastanum" = expression(paste(italic("Aesculus hippocastanum"))),
                               "Alnus glutinosa" = expression(paste(italic("Alnus glutinosa"))),
                               "Betula pendula" = expression(paste(italic("Betula pendula"))),
                               "Fagus sylvatica" = expression(paste(italic("Fagus sylvatica"))),
                               "Fraxinus excelsior" = expression(paste(italic("Fraxinus excelsior"))),
                               "Quercus robur" = expression(paste(italic("Quercus robur")))))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) +  
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.2, "cm"),
                              legend.text=element_text(size=8), legend.position= c(0.9,0.9)) #+
#xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
fig1

prop.test<-ggplot(prop, aes(x=year, y=fs.prop)) + geom_line(aes(color=species)) 
#+ 
 # geom_line(aes(y=m.index/2), col="blue") + scale_y_continuous(~.*2, sec.axis = sec_axis(name="NAO Index"))

  
## should I take the log of m.index so it's a positive value?
prop$nao<-prop$m.index+1.5
#sp20.mod<-brm(fs.tot | trunc(lb = 0)~m.index+cc.20+sp.temp+m.index:cc +(1|species)+(m.index-1|species)+(cc.20-1|species)+
 #               (sp.temp-1|species) + (m.index:cc-1|species), data=prop) ## 3986 divergent tranistions!!
sp20.mod<-brm(fs.tot~nao+cc+nao:cc +(1|species)+(nao-1|species)+(cc-1|species) + (nao:cc-1|species), data=prop) ## 5 divergent tranistions!!
sp20.stan<-stan_glmer(spp.tot~m.index+sp.temp+(1|species) , data=prop)



prop_prev<-prop
prop_prev$year<-prop_prev$year - 1
prop_prev$nao_prev<-NA
for(i in c(1:nrow(prop))) {
  for(j in c(1:nrow(prop_prev))) 
    if(prop$year[i]==prop_prev$year[j] )
      prop_prev$nao_prev[j]<-prop$nao[i]
    
}
## still not working but see if the year previous had a strong NAO then does that influence false springs?
## better with previous year... see if Ben thinks this is reasonable... keep exploring
nao<-stan_glm(fs.tot~nao_prev+sp.temp, data=prop_prev)
