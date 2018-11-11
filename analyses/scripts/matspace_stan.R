## Started 5 March 2018 ##
## By Cat ##

## Using space parameter from residuals of eigenvectors ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(ggplot2)
library(rstanarm)
library(dplyr)
library(tidyr)
library(brms)
library(jtools)
#library(ggmap)
#library(rworldmap)
#library(maps)
#library(mapdata)
#library(marmap)
#library(RColorBrewer)
#library(raster)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data
#dx<-read.csv("output/fs_matspspace_old.csv", header=TRUE)
#dx<-read.csv("output/fs_matspspace_times2.csv", header=TRUE)
#dx<-subset(dx, year>1950)
#xx<-read.csv("output/fs_matspspace.csv", header=TRUE)
#bb.stan<-read.csv("output/bb.brm.nointer.csv", header=TRUE)

#dx<-dx%>%dplyr::select(lat, long, space)
#dx<-dx[!duplicated(dx),]
#dxx<-dx[which(dx$space<=-100 | dx$space>=300),]

#mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35)) # create a layer of borders
#myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
#bb.map.mini<-bb.map
#bb.map.mini$space<-round(bb.map$space, digits=0)
#bb.map.mini<-bb.map.mini[!duplicated(bb.map.mini),]
#sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-51, 87))
#site<- ggplot(bb.map.mini, aes(x=long, y=lat, col=space), alpha=0.2) +   mapWorld +
#  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))
#site <- site + theme(panel.border = element_blank(),
 #                 panel.grid.major = element_blank(),
  #                panel.grid.minor = element_blank()) + geom_point() + geom_jitter(alpha=0.3)+
  # sc + labs(color="Space Parameter")+
  # xlab("Longitude") + ylab("Latitude")
#quartz()
#site

#spg<-bb.map
#coordinates(spg)<- ~long+lat
#proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
#coords<-spTransform(spg, CRS("+proj=longlat"))
#shapefile(coords, "output/bbspace.shp")

dx<-read.csv("output/fs_space_new.csv", header=TRUE)
#xx<-read.csv("output/fs_yearsitespp.csv", header=TRUE)
#xx<-subset(xx, year>1950)
dx<-subset(dx, select=c("lat", "long", "lat.long", "eigen", "distkm", "elev", "mst", "nao", "year", "species", "cc"))
#xx<-read.csv("output/fs_yearsitespp_5.csv", header=TRUE)
xx<-read.csv("output/fs_dvr_cleaned.csv", header=TRUE)
xx<-subset(xx, select=c("lat", "long", "fs.count", "year", "species", "fs"))
df<-read.csv("output/mat_MAM.csv", header=TRUE)
df<-subset(df, year>1950)
mat<-read.csv("output/fs_bb_sitedata.csv", header=TRUE)
mat<-subset(mat, year>1950)
nao<-read.csv("output/nao_NovApr.csv", header=TRUE)
nao<-subset(nao, year>1950)

### Clean up dataframes a bit
dx<-dx%>%dplyr::select(lat, long, space)
dx<-dx[!duplicated(dx),]
df<-df[!duplicated(df),]
dx<-full_join(df, dx)
dx<-dx[!duplicated(dx),]

mat<-dplyr::select(mat, species, LAT, LON, ALT)
mat<-mat%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
mat$lat.long<-paste(mat$lat, mat$long)
mat<-mat[!duplicated(mat),]
bb<-full_join(mat, dx)


#### Get elevation information
#bb<-bb%>%rename(sp.temp=pre.bb)
bb$cc<-ifelse(bb$year<=1983&bb$year>1950, 0, 1)

xx<-dplyr::select(xx, lat, long, species, fs.count, year)
xx<-xx[!duplicated(xx),]
bb<-full_join(bb, xx)
bb<-na.omit(bb)
bb<-bb[!duplicated(bb),]

#bb$elev<-ave(bb$elev, bb$lat.long)
#bb<-bb[!duplicated(bb),]


nao<-dplyr::select(nao, year, nao)
nao<-nao[!duplicated(nao),]


bb<-full_join(bb, nao)

#dist<-read.csv("output/dist_utm.csv", header=TRUE)
dist<-read.csv("output/dist_wgs.csv", header=TRUE)
dist<-dist%>%rename(long=LONG)%>%rename(lat=LAT)

bb<-full_join(dx, dist)

bb<-full_join(dx, xx)
bb<-bb[!duplicated(bb),]
bb<-na.omit(bb)
#### Space parameter? ####
# summary(lm(space~elev+lat+long, data=bb))

tar.var<-c("species", "year", "lat.long")
resp.var<-c("fs.count")

bb.sub<-bb[,c(tar.var, resp.var)]
bb.sub.nodup<-bb[!duplicated(bb.sub),]
dd<-bb.sub.nodup
#dd$space<-round(dd$space, digits=3)
#dd<-dd[!duplicated(dd),]

write.csv(dd, file="~/Documents/git/regionalrisk/analyses/output/regrisk.nov_5.csv", row.names = FALSE)
write.csv(bb, file="~/Documents/git/regionalrisk/analyses/output/fs_newdvr_space.csv", row.names = FALSE)
#mat<-mat%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
#mat<-dplyr::select(mat, species, lat, long, elev)
#mat<-mat[!duplicated(mat),]
#d<-inner_join(bb, mat)

#d$nao<-NA
#for(i in c(1:nrow(d))){
#  for(j in c(1:nrow(nao)))
#    if(d$species[i]==nao$species[j] & d$sp.temp[i]==nao$sp.temp[j])
#      d$nao[i]<-nao$m.index[j]
#}

#fs.cc<-dplyr::select(d, fs.num, sp.temp, elev, cc, species, nao)
#fs.cc$species<-as.numeric(as.factor(fs.cc$species))

#fs.cc<-fs.cc[!duplicated(fs.cc),]

##### Another starting point!!! #####
#bb<-read.csv("output/regrisk.cleaned.csv", header=TRUE)

dd$sm.elev<-dd$elev/100
#bb$nao<-bb$m.index*10

columnstokeep <- c("species", "nao", "mst", "cc", "lat", "elev", "fs.count", "distkm", "space")
#columnstokeep.map <- c("space","lat", "long")
#bb.map<-subset(bb, select=columnstokeep.map)
bb.stan <- subset(dd, select=columnstokeep)

#bb.stan$sp.temp<-round(bb.stan$sp.temp, digits=3)
#bb.stan$space<-round(bb.stan$space, digits=3)

bb.stan<-bb.stan[!duplicated(bb.stan),]
#bb.map<-bb.map[!duplicated(bb.map),]
bb.stan<-na.omit(bb.stan)
#bb.map<-na.omit(bb.map)

#bb.stan<-bb
write.csv(bb.stan, file="~/Documents/git/regionalrisk/analyses/output/bb_latprep_nov_5.csv", row.names = FALSE)
bb.stan<-read.csv("output/bb.brm.nointer.csv", header=TRUE)

#bb<-bb.stan[sample(nrow(bb.stan), 500), ]

#fit<-brm(fs.count~m.index+sm.elev+cc+m.index:cc+sm.elev:cc + 
 #          (m.index+sm.elev+cc+m.index:cc+sm.elev:cc|species), data=bb)

nao<-interact_plot(model = fit2, pred = m.index, modx = cc)
elev<-interact_plot(model = fit2, pred = sm.elev, modx = cc)

#check<-bb[duplicated(bb[1:4]) | duplicated(bb[1:4], fromLast= TRUE),]




############# Probably can delete below later #################


spp <- unique(bb.stan$complex)
for (sp in c(1:length(spp))){
  par(mfrow=c(1,3))
  subby <- subset(bb.stan, complex==spp[sp])
  # chilling
  plot(resp~chill, data=subby, main=subby$complex.wname[1]) 
  intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
  slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
  abline(intercepthere, slopehere)
  intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
  slopehere <- othermodel[grep("b_chill", rownames(othermodel)),1][spp[sp]+2]
  abline(intercepthere, slopehere, col="blue")
  # forcing 
  plot(resp~force, data=subby) # should add color coding by datasetID
  intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
  slopehere <- whichmodel[grep("b_force", rownames(whichmodel)),1][spp[sp]+2]
  abline(intercepthere, slopehere)
  intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
  slopehere <- othermodel[grep("b_force", rownames(othermodel)),1][spp[sp]+2]
  abline(intercepthere, slopehere, col="blue")
  # photo
  plot(resp~photo, data=subby) # should add color coding by datasetID
  intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
  slopehere <- whichmodel[grep("b_photo", rownames(whichmodel)),1][spp[sp]+2]
  abline(intercepthere, slopehere)
  intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
  slopehere <- othermodel[grep("b_photo", rownames(othermodel)),1][spp[sp]+2]
  abline(intercepthere, slopehere, col="blue")
}

m<-brm.full.nointer
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
dflong<- tidyr::gather(dftot, var, value, nao:sm.elev, factor_key=TRUE)

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

write.csv(dfwide, file="/n/wolkovich_lab/Lab/Cat/brm_output.csv", row.names=FALSE)
## plotting

pd <- position_dodgev(height = -0.5)

estimates<-c("NAO", "Mean Spring Temperature", "Before or After 1983", "Space", "Elevation")
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
                              legend.title=element_blank(), legend.key=element_blank(),legend.key.size = unit(0.15, "cm"),
                              legend.text=element_text(size=8), legend.position= c(0.78,0.88), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black")) #+
#xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
quartz()
fig1


bb.pre<-bb%>%filter(year<=1983)
bb.pre<-dplyr::select(bb.pre, sp.temp, sm.elev, species, nao, fs.count, space)
bb.pre<-bb.pre[!duplicated(bb.pre),]

brm.first<-brm(fs.count~sp.temp+sm.elev+space+nao+(1|species), data=bb.pre, family=poisson, chains =2)



fit.first<-stan_glmer(fs~sp.temp+sm.elev+space+nao+(1|species), data=bb.pre, family=binomial, chains=1)

bb.post<-bb%>%filter(year>1983)
bb.post<-dplyr::select(bb.post, sp.temp, sm.elev, species, year, fs)
bb.post<-bb.post[!duplicated(bb.post),]

fit.last<-stan_glmer(fs~sp.temp+sm.elev)

fit1.nointer<-stan_glmer(fs~sp.temp+nao+space+cc+(1|species), data=bb)


fit<-stan_glmer(fs.num~sp.temp+elev+cc+(1|species), data=fs.cc, family=neg_binomial_2, chains=2)
fit<-stan_glm(fs.num~sp.temp+elev+cc, data=fs.cc, family=neg_binomial_2, chains=2)
fit.inter<-stan_glmer(fs.num~sp.temp+elev+cc+sp.temp:elev+(1|species), data=fs.cc, family=poisson)

fit.brm<-brm(fs.num~sp.temp+elev+cc+elev:sp.temp+(1|species), data=fs.cc,family=poisson, chains=2)

stan.ele<-stan_glmer(fs.num~sp.temp+elev+cc+sp.temp:elev+(sp.temp-1|species)+
                       (elev-1|species)+(cc-1|species)+(sp.temp:elev-1|species)+(1|species), data=fs.cc, family=poisson)

fs.cc<-fs.cc[!is.na(fs.cc$fs.num),]
fs.cc<-fs.cc[!is.na(fs.cc$sp.temp),]
fs.cc<-fs.cc[!is.na(fs.cc$elev),]
fs.cc<-fs.cc[!is.na(fs.cc$cc),]
fs.cc<-fs.cc[!is.na(fs.cc$nao),]

fs.cc$fs.num<-as.integer(fs.cc$fs.num+1)

ele.brm<-brm(fs.num~ sp.temp + cc + elev + elev:sp.temp + (1|species) + (sp.temp-1|species) + (cc-1|species)
            + (elev-1|species) + (elev:sp.temp-1|species), data=fs.cc, family=poisson, 
            prior = set_prior("normal(0,10)", class="b", lb = 0))



ele.brm<-brm(fs.num~ sp.temp + cc + sm.elev + sp.temp:cc + (1|species) + (sp.temp-1|species) + (cc-1|species)
             + (sm.elev-1|species) + (sp.temp:cc-1|species), data=fs.cc)

### Try to fix elevation autocorrelation issues? Or maybe just scale it...
fs.cc$sm.elev<-fs.cc$elev/100

elev.stan<-stan_glmer(fs.num~sp.temp+cc+sm.elev+(1|species), data=fs.cc)

ele2.brm<-brm(fs.num~ sp.temp + cc + sm.elev + sm.elev:sp.temp + (1|species) + (sp.temp-1|species) + (cc-1|species)
             + (sm.elev-1|species) + (sm.elev:sp.temp-1|species), data=fs.cc, family=negbinomial)

ele3.brm<-brm(fs.num~ sp.temp + cc + sm.elev + nao + sm.elev:sp.temp + (1|species) + (sp.temp-1|species) + (cc-1|species)
              + (sm.elev-1|species) + (cc:sp.temp-1|species), data=fs.cc, family=negbinomial)

## Using 1983 as split point
prep_cc<-bb
prep_cc$fs.num<-ave(prep_cc$fs, prep_cc$lat.long, prep_cc$cc,prep_cc$species, FUN=sum)
prep_cc$mat.cc<-ave(prep_cc$mat, prep_cc$lat.long)
fs.cc<-dplyr::select(prep_cc, fs.num, mat.cc, cc, space, species)
fs.cc$species<-as.numeric(as.factor(fs.cc$species))
fs.cc<-fs.cc[!duplicated(fs.cc),]

fit<-stan_glm(fs.num~mat.cc+cc+space+species, data=fs.cc, family=poisson)
## maybe cool...
cc.brm<-brm(fs.num~ mat.cc + cc + space + mat.cc:cc + (1|species) + (mat.cc-1|species) + (cc-1|species)
             + (space-1|species) + (mat.cc:cc-1|species), data=fs.cc, family=poisson)
m<-ele3.brm
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
dflong<- tidyr::gather(dftot, var, value, sp.temp:`sp.temp:sm.elev`, factor_key=TRUE)

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

estimates<-c("Mean Spring Temperature", "Before or After 1983", "Elevation", "Mean Spring Temperature x Elevation")
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
                              legend.title=element_blank(), legend.key=element_blank(),legend.key.size = unit(0.15, "cm"),
                              legend.text=element_text(size=8), legend.position= c(0.78,0.88), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black")) #+
  #xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
quartz()
fig1

write.csv(dfwide, file="~/Documents/git/regionalrisk/analyses/output/mat_cc_output.csv", row.names=FALSE)

### Looking at each year
prep_yr<-bb
fs.yr<-dplyr::select(prep_yr, fs, mat, year, space, species)
fs.yr<-fs.yr[!duplicated(fs.yr),]


fit1<-stan_glm(fs~mat+year+space+species, data=fs.yr, family=binomial)
## not so cool...


### Looking at decade
bb$decade<-NA
bb$decade<-ifelse(bb$year>=1950 & bb$year<1960, 1, bb$decade)
bb$decade<-ifelse(bb$year>=1960 & bb$year<1970, 2, bb$decade)
bb$decade<-ifelse(bb$year>=1970 & bb$year<1980, 3, bb$decade)
bb$decade<-ifelse(bb$year>=1980 & bb$year<1990, 4, bb$decade)
bb$decade<-ifelse(bb$year>=1990 & bb$year<2000, 5, bb$decade)
bb$decade<-ifelse(bb$year>=2000 & bb$year<2010, 6, bb$decade)
bb<-bb[!is.na(bb$decade),]
prep_dec<-bb
prep_dec$fs.num<-ave(prep_dec$fs, prep_dec$lat.long, prep_dec$decade, prep_dec$species, FUN=sum)
prep_dec$mat.dec<-ave(prep_dec$mat, prep_dec$lat.long, prep_dec$decade)
fs.dec<-dplyr::select(prep_dec, fs.num, mat.dec, decade, space, species)
fs.dec<-fs.dec[!duplicated(fs.dec),]

fit2<-stan_glm(fs.num~mat.dec+decade+space+species, data=fs.dec, family=poisson)

#stan_glm
#family:       poisson [log]
#formula:      fs.num ~ mat.dec + decade + space
#observations: 6125
#predictors:   4
#------
#  Median MAD_SD
#(Intercept)  0.6    0.0  
#mat.dec     -0.1    0.0  
#decade       0.1    0.0  
#space        0.1    0.0  

#Sample avg. posterior predictive distribution of y:
#  Median MAD_SD
#mean_PPD 1.6    0.0   

#------
#  For info on the priors used see help('prior_summary.stanreg').







bb<-bb%>%rename(sp.temp=pre.bb)
bb$nao<-NA
pb <- txtProgressBar(min = 1, max = nrow(bb), style = 3)
for(i in c(1:nrow(bb))){
  for(j in c(1:nrow(nao)))
    if(bb$species[i]==nao$species[j] & bb$year[i]==nao$year[j])
      bb$nao[i]<-nao$m.index[j]
    setTxtProgressBar(pb, i)
}
bb$fs.num<-ave(bb$fs, bb$lat.long, bb$species, bb$nao, FUN=sum)
bb$sp.temp<-ave(bb$sp.temp, bb$lat.long, bb$year)
bb<-dplyr::select(bb, -fs.count, -PEP_ID, -year, -fs, -lat.long)
bb<-bb[!duplicated(bb),]
mat<-mat%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
mat<-dplyr::select(mat, species, lat, long, elev)
mat<-mat[!duplicated(mat),]
d<-inner_join(bb, mat)



fs.cc<-dplyr::select(d, fs.num, sp.temp, elev, species, nao)
fs.cc$species<-as.numeric(as.factor(fs.cc$species))

fs.cc<-fs.cc[!duplicated(fs.cc),]

fs.cc<-fs.cc[!is.na(fs.cc$fs.num),]
fs.cc<-fs.cc[!is.na(fs.cc$sp.temp),]
fs.cc<-fs.cc[!is.na(fs.cc$elev),]
fs.cc<-fs.cc[!is.na(fs.cc$nao),]

fs.cc$fs.num<-as.integer(fs.cc$fs.num+1)
fs.cc$sm.elev<-fs.cc$elev/100

nao.stan<-stan_glm(fs.num~sp.temp+nao+sm.elev, data=fs.cc)

write.csv(fs.cc, "~/Documents/git/regionalrisk/analyses/output/mat_mod_yr.csv", row.names=FALSE)

nao.brm<-brm(fs.num~ sp.temp + nao + sm.elev + nao:sp.temp + (1|species) + (sp.temp-1|species) + (nao-1|species)
              + (sm.elev-1|species) + (nao:sp.temp-1|species), data=fs.cc, chains=2)

m<-nao.brm
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
dflong<- tidyr::gather(dftot, var, value, sp.temp:`sp.temp:nao`, factor_key=TRUE)

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

estimates<-c("Mean Spring Temperature", "NAO Index", "Elevation", "Mean Spring Temperature x NAO Index")
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
                              legend.title=element_blank(), legend.key=element_blank(),legend.key.size = unit(0.15, "cm"),
                              legend.text=element_text(size=8), legend.position= c(0.78,0.88), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black")) #+
#xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
quartz()
fig1

#### Should probaly try adding in year and yearxnao  as predictors but probably is my final model!
## Maybe do the exact same model for predicting budburst date? And then do a quick one and see if FS ~ BB is significant
# Finally, need to determine how to quantify anamolies in temperature...

