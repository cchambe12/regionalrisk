## Started 5 March 2018 ##
## By Cat ##

## Using space parameter from residuals of eigenvectors ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(dplyr)
library(tidyr)
library(brms)
library(ggstance)

# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")


########################
#### get the data
bb<-read.csv("output/fs_matspspace.csv", header=TRUE)


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
m<-cc.brm
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
dflong<- tidyr::gather(dftot, var, value, mat.cc:`mat.cc:cc`, factor_key=TRUE)

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

estimates<-c("Mean Annual Temperature", "CC", "Space", "MAT*CC")
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
                              legend.text=element_text(size=8), legend.position= c(0.2,0.1)) #+
  #xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
fig1



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
