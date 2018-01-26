## Started 25 January 2018 ##
## By Cat ##

## Try to run REAL data ##
## With Stan! ##
## Rstanarm poisson data - talk to Lizzie
# Num FS years ~ MAT + Sp + Site -- comparing before and after 1980

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#dostan = TRUE

library(rstan)
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(dplyr)
library(ape)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/regionalrisk/analyses/")
#source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
#bb<-read.csv("output/smfake_mat.csv", header=TRUE)
bb<-read.csv("output/fs_matspsite.csv", header=TRUE)

## # yrs FS ~ MAT + SP + SITE prep data

#pre<-bb%>%filter(year>1950)%>%filter(year<=1983)
pre<-bb
pre$fs<-ave(pre$fs, pre$PEP_ID, pre$species, FUN=sum)
pre$sp<-as.numeric(as.factor(pre$species))
pre$mat<-ave(pre$mat, pre$PEP_ID)
pre$lat<-as.numeric(pre$lat)
pre$long<-as.numeric(pre$long)

pre<-pre%>%dplyr::select(lat.long, lat, long, mat, sp, fs)
pre.bx<-pre[!duplicated(pre),]

## subsetting data, preparing genus variable, removing NAs
pre.mat.prepdata <- subset(pre.bx, select=c("fs", "mat", "sp", "lat", "long")) 
pre.mat.stan <- pre.mat.prepdata[complete.cases(pre.mat.prepdata),]

pre.mat<-stan_glm(fs~mat+sp+lat+long, data=pre.mat.stan, family=poisson, prior=normal(0,1))
pre.mat
pre.plot<-plot(pre.mat, pars="beta")
pp_check(pre.mat)
#launch_shinystan(pre.mat)
launch_shinystan(mat)

####################################

fs = pre.mat.stan$fs
mat = pre.mat.stan$mat
sp = pre.mat.stan$sp
lat = pre.mat.stan$lat
lon = pre.mat.stan$long
N = length(fs)


# making a list out of the processed data. It will be input for the model
datalist.td <- list(fs=fs,mat=mat,sp=sp,lat=lat,lon=lon,N=N)

mat.td4 = stan('scripts/fs_matsimple.stan', data = datalist.td,
               iter = 2000, warmup=1500, control=list(adapt_delta=0.99)) 


######### Plotting time! ##########
df<-pre.mat.stan
df$fs.num<-ave(df$fs, df$sp)
df$fs.sd<-ave(df$fs, df$sp, FUN=sd)
df$sp<-ifelse(df$sp==1, "A.hippocastanum", df$sp)
df$sp<-ifelse(df$sp==2, "A.glutinosa", df$sp)
df$sp<-ifelse(df$sp==3, "B.pendula", df$sp)
df$sp<-ifelse(df$sp==4, "F.sylvatica", df$sp)
df$sp<-ifelse(df$sp==5, "F.excelsior", df$sp)
df$sp<-ifelse(df$sp==6, "Q.robar", df$sp)
dx<-df%>%dplyr::select(-fs, -mat, -lat, -long)
dx<-dx[!duplicated(dx),]
diff<-ggplot(dx, aes(x=as.factor(sp), y=fs.num)) + geom_point() + 
  geom_linerange(aes(ymin=fs.num-fs.sd, ymax=fs.num+fs.sd), alpha=0.3) + 
  ylab(expression("Years with False Springs")) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, vjust=0.5), axis.text=element_text(size=10))
plot(diff)

df$matr<-round(df$mat, digits=1)
df$fs.mat<-ave(df$fs, df$mat)
dm<-df%>%dplyr::select(fs.mat, mat, sp )
dm<-dm[!duplicated(dm),]
dm$matr<-as.numeric(dm$matr)
mat.spp<-ggplot(dm, aes(mat, fs.mat)) + xlab("Mean Annual Temperature") +
  ylab("Number of False Springs")  + geom_smooth(aes(col=as.factor(sp)),method="lm", se=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key=element_blank())
plot(mat.spp)

df$latr<-round(df$lat, digits=2)
df$fs.lat<-ave(df$fs, df$latr)
dl<-df%>%dplyr::select(fs.lat, latr, sp )
dl<-dl[!duplicated(dl),]
dl$latr<-as.numeric(dl$latr)
lat.spp<-ggplot(dl, aes(latr, fs.lat)) + xlab("Latitude") +
  ylab("Number of False Springs")  + geom_smooth(aes(col=as.factor(sp)),method="loess", se=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key=element_blank())
plot(lat.spp)

df$longr<-round(df$long, digits=2)
df$fs.long<-ave(df$fs, df$longr)
dlon<-df%>%dplyr::select(fs.long, longr, sp )
dlon<-dlon[!duplicated(dlon),]
dlon$longr<-as.numeric(dlon$longr)
long.spp<-ggplot(dlon, aes(longr, fs.long)) + xlab("Longitude") +
  ylab("Number of False Springs")  + geom_smooth(aes(col=as.factor(sp)),method="loess", se=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key=element_blank())
plot(long.spp)


### Moran's I ####
pre.spa<-pre.mat.stan%>%dplyr::select(-sp)
pre.spa$lat.long<-paste(pre.spa$lat, pre.spa$long)
pre.spa$fs<-ave(pre.spa$fs, pre.spa$lat.long)
pre.spa<-pre.spa[!duplicated(pre.spa),]
pre.spa$lat<-jitter(pre.spa$lat, factor=1, amount=NULL)
pre.spa$long<-jitter(pre.spa$long, factor=1, amount=NULL)
pre.dist<-as.matrix(dist(cbind(pre.spa$lat, pre.spa$long)))
pre.dist.inv<-1/pre.dist
diag(pre.dist.inv) <- 0
pre.dist.inv[1:5,1:5]
Moran.I(pre.spa$fs, pre.dist)



################ Not separating right now... asking separate questions to answer this question ### 
############## Post now! ###############
post<-bb%>%filter(year>1983)
#post<-bb
post$sp<-as.numeric(as.factor(post$species))
#post$sp<-post$species
post$lat<-as.numeric(post$lat)
post$long<-as.numeric(post$long)
post$lat.long<-paste(post$lat, post$long)
post$fs<-ave(post$fs, post$lat.long, post$species, FUN=sum)
post$mat<-ave(post$mat, post$lat.long)

post<-post%>%dplyr::select(lat, long, mat, sp, fs)
post.bx<-post[!duplicated(post),]

## subsetting data, postparing genus variable, removing NAs
post.mat.prepdata <- subset(post.bx, select=c("fs", "mat", "sp", "lat", "long")) 
post.mat.stan <- post.mat.prepdata[complete.cases(post.mat.prepdata),]

#post<-post.mat.stan[order(post.mat.stan$lat, post.mat.stan$long), ]
#post$lat.long<-paste(post$lat, post$long)
#post$site<-as.numeric(as.factor(post$lat.long))

post.mat<-stan_glm(fs~mat+sp+lat+long, data=post.mat.stan, family=poisson, prior=normal(0,1))
post.mat
post.plot<-plot(post.mat, pars="beta") + scale_x_continuous(limits = c(-0.35, 0.15))+ 
  annotate("text", x = -0.3, y = 5, label = "1984-2016", fontface = "bold")
pp_check(post.mat)
#launch_shinystan(post.mat)

grid.arrange(pre.plot, post.plot, ncol=2)

post.dist<-as.matrix(dist(cbind(post.mat.stan$lat, post.mat.stan$long)))
post.dist.inv<-1/post.dist
diag(post.dist.inv) <- 0
post.dist.inv[1:5,1:5]
Moran.I(post.mat.stan$fs, post.dist.inv)


