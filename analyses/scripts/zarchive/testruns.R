## Cat - 12 August 2018
# Ben Goodrich suggested I try the stan_biglm function

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)
library(rstan)
library(rstanarm)
library(sjPlot)
library(sjmisc)
library(egg)
library(RColorBrewer)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


bb<-read.csv("~/Documents/git/regionalrisk/analyses/output/bb.brm.nointer.csv", header=TRUE)
bbs<-bb[sample(nrow(bb), 60000),]
ols<-stan_glmer(fs.count~m.index+sp.temp+cc+sm.elev+space+m.index:cc + sp.temp:cc +sm.elev:cc +
                  space:cc + (m.index+sp.temp+cc|species), data=bb)
br<-brm(fs.count~m.index+sp.temp+cc+sm.elev+space+m.index:cc + sp.temp:cc +sm.elev:cc +
          space:cc + (m.index+sp.temp+cc|species), data=bb)

bb$nao.c <- (bb$m.index-mean(bb$m.index,na.rm=TRUE))
bb$mat.c <- (bb$sp.temp-mean(bb$sp.temp,na.rm=TRUE))
bb$cc.c <- (bb$cc-mean(bb$cc,na.rm=TRUE))
bb$elev.c <- (bb$sm.elev-mean(bb$sm.elev,na.rm=TRUE))
bb$space.c <- (bb$space-mean(bb$space,na.rm=TRUE))

bb$nao.z <- (bb$m.index-mean(bb$m.index,na.rm=TRUE))/sd(bb$m.index,na.rm=TRUE)
bb$mat.z <- (bb$sp.temp-mean(bb$sp.temp,na.rm=TRUE))/sd(bb$sp.temp,na.rm=TRUE)
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/sd(bb$cc,na.rm=TRUE)
bb$elev.z <- (bb$sm.elev-mean(bb$sm.elev,na.rm=TRUE))/sd(bb$sm.elev,na.rm=TRUE)
bb$space.z <- (bb$space-mean(bb$space,na.rm=TRUE))/sd(bb$space,na.rm=TRUE)

testp<-brm(fs.count~nao.z+mat.z+cc.z+elev.z, data=bbs, cores=3, family=poisson)

test2<-brm(fs.count~nao.z+mat.z+cc.z+elev.z, data=bbs, future=TRUE)

cen<-brm(fs.count~nao.c+mat.c+cc.c+elev.c+space.c+nao.c:cc.c + mat.c:cc.c +elev.c:cc.c +
           space.c:cc.c + (nao.c+mat.c+cc.c|species), data=bb)


z<-brm(fs.count~nao.z+mat.z+cc.z+elev.z+space.z+nao.z:cc.z + mat.z:cc.z +elev.z:cc.z +
         space.z:cc.z + (nao.z+mat.z+cc.z|species), data=bb)

z2<-brm(fs.count~nao.z+mat.z+cc.z+elev.z+space.z+nao.z:cc.z + mat.z:cc.z +elev.z:cc.z +
          space.z:cc.z + (0+nao.z||species) + (0+mat.z||species) + (0+cc.z||species), data=bb, cores=4)

bprior1 <- prior(normal(0,1), class="b") + prior(student_t(1,0,2), group="species", class="sd")

z3<-brm(fs.count~nao.z+mat.z+cc.z+elev.z+space.z+nao.z:cc.z + mat.z:cc.z +elev.z:cc.z +
          space.z:cc.z + (0+nao.z||species) + (0+mat.z||species) + (0+cc.z||species), data=bb, 
        cores=4, prior=bprior1)
z4<-brm(fs.count~nao.z+mat.z+cc.z+elev.z+space.z+nao.z:cc.z + mat.z:cc.z +elev.z:cc.z +
          space.z:cc.z + (0+nao.z||species) + (0+mat.z||species) + (0+cc.z||species), data=bb, 
        prior=bprior1, future=TRUE)
z5<-brm(fs.count~nao.z+mat.z+cc.z+elev.z+space.z+nao.z:cc.z + mat.z:cc.z +elev.z:cc.z +
          space.z:cc.z + (0+nao.z||species) + (0+mat.z||species) + (0+cc.z||species), data=bb, 
        prior=bprior1, future=TRUE)

get_prior(fs.count~nao.z+mat.z+cc.z+elev.z+space.z+nao.z:cc.z + mat.z:cc.z +elev.z:cc.z +
            space.z:cc.z + (0+nao.z||species) + (0+mat.z||species) + (0+cc.z||species), data=bb)

d<-read.csv("~/Documents/git/regionalrisk/analyses/output/regrisk.cleaned.csv", header=TRUE)
d<-read.csv("~/Documents/git/regionalrisk/analyses/output/regrisk.cleaned.2.csv", header=TRUE)
bb<-d

#library(ggmap)
#map<-get_map(location="Europe", zoom=4)
#mapPoints<-ggmap(map) + geom_point(x=d$long, y=d$lat, aes(col=d$space), data=d) + facet_wrap(~species) +
#  theme(legend.position = "none")
#mapPoints

bb$sm.elev<-bb$elev/100
spp.nao<-unique(ave(bb$m.index, bb$species))
spp.mat<-unique(ave(bb$sp.temp, bb$species))
spp.cc<-as.matrix(unique(ave(bb$cc, bb$species)))
spp.elev<-as.matrix(unique(ave(bb$sm.elev, bb$species)))
spp.space<-as.matrix(unique(ave(bb$space, bb$species)))
bb$nao.cc<-bb$m.index*bb$cc
bb$mat.cc<-bb$sp.temp*bb$cc
bb$elev.cc<-bb$sm.elev*bb$cc
bb$space.cc<-bb$space*bb$cc
bb$space.elev<-bb$space*bb$sm.elev

quartz()
bbs<-bb[sample(nrow(bb), 60000),]
d<-bb
dist<-ggplot(d, aes(x=distkm, y=space)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")
lat<-ggplot(d, aes(x=distkm, y=lat)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")
elev<-ggplot(d, aes(x=lat, y=elev)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")
mat<-ggplot(d, aes(x=lat, y=mst)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")

sp<-subset(d, select=c("space", "long", "lat"))
sp<-sp[!duplicated(sp),]
space<-ggplot(sp, aes(x=long, y=lat)) + geom_point(aes(col=space))

library(raster)
library(rgdal)
spg<-sp
coordinates(spg)<- ~long+lat
proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
coords<-spTransform(spg, CRS("+proj=longlat"))
shapefile(coords, "~/Documents/git/regionalrisk/analyses/output/spaceparam.shp", overwrite=TRUE)


space2<-ggplot(d[(d$space<=10 & d$space>=-10),], aes(x=long, y=lat)) + geom_point(aes(col=space), alpha=0.3) + 
  facet_wrap(~species) 

box<-ggplot(d, aes(x=species, y=space)) + geom_boxplot()

bb<-na.omit(bb)
#bb$species<-as.numeric(as.factor(bb$species))
fit<-lm(fs.count~ m.index + ( m.index:species) + sp.temp + (sp.temp:species) + sm.elev + (sm.elev:species) + 
          space + (space:species) + cc  + (cc:species) + species + m.index:cc + sp.temp:cc +sm.elev:cc +
          space:cc, data=bb)             # not necessary in this case

b <- coef(fit)[-1]
R <- qr.R(fit$qr)[-1,-1]
SSR <- crossprod(fit$residuals)[1]
not_NA <- !is.na(fitted(fit))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$m.index)),  as.numeric(mean(bb$m.index[bb$species=="ALNGLU"])), as.numeric(mean(bb$m.index[bb$species=="BETPEN"])),
          as.numeric(mean(bb$m.index[bb$species=="FAGSYL"])), as.numeric(mean(bb$m.index[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$m.index[bb$species=="QUEROB"])), as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$sp.temp[bb$species=="ALNGLU"])), as.numeric(mean(bb$sp.temp[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sp.temp[bb$species=="FAGSYL"])), as.numeric(mean(bb$sp.temp[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="QUEROB"])),
          as.numeric(mean(bb$sm.elev)),  
          as.numeric(mean(bb$sm.elev[bb$species=="ALNGLU"])), as.numeric(mean(bb$sm.elev[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sm.elev[bb$species=="FAGSYL"])), as.numeric(mean(bb$sm.elev[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$sm.elev[bb$species=="QUEROB"])), 
          as.numeric(mean(bb$space)), 
          as.numeric(mean(bb$space[bb$species=="ALNGLU"])), as.numeric(mean(bb$space[bb$species=="BETPEN"])),
          as.numeric(mean(bb$space[bb$species=="FAGSYL"])), as.numeric(mean(bb$space[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$space[bb$species=="QUEROB"])),
          as.numeric(mean(bb$cc)),  
          as.numeric(mean(bb$cc[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc[bb$species=="BETPEN"])), as.numeric(mean(bb$cc[bb$species=="FAGSYL"])), 
          as.numeric(mean(bb$cc[bb$species=="FRAEXC"])), as.numeric(mean(bb$cc[bb$species=="QUEROB"])),
          as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FAGSYL")), as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")), 
          as.numeric(mean(bb$m.index*bb$cc)), as.numeric(mean(bb$sp.temp*bb$cc)), as.numeric(mean(bb$sm.elev*bb$cc)), 
          as.numeric(mean(bb$space*bb$cc)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
post.inter <- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                       # the next line is only to make the example go fast
                       chains = 4, iter = 2000)
cbind(lm = b, stan_lm = rstan::get_posterior_mean(post.inter)[1:26,]) # shrunk
# }


plotting <- as.data.frame(summary(post.inter)$summary)
simple<-plotting
simple$var<- rownames(simple)
rownames(simple)<-1:45
simple<-simple[2:40,]
simple<-simple[!(simple$var=="speciesALNGLU"|simple$var=="speciesBETPEN"|simple$var=="speciesFAGSYL"|
                   simple$var=="speciesFRAEXC"|simple$var=="speciesQUEROB"),]
simple<-subset(simple, select=c("var", "mean", "2.5%", "97.5%"))
simple$species<-c(1,1,1,1,1,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,0,0,0,0)
simple$Jvar<-NA
simple$Jvar<-ifelse(simple$var=="m.index", 9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesALNGLU", 8.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesBETPEN", 8.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesFAGSYL", 8.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesFRAEXC", 8.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="m.index:speciesQUEROB", 8.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="sp.temp", 8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesALNGLU:sp.temp", 7.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesBETPEN:sp.temp", 7.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFAGSYL:sp.temp", 7.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFRAEXC:sp.temp", 7.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesQUEROB:sp.temp", 7.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="sm.elev", 7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesALNGLU:sm.elev", 6.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesBETPEN:sm.elev", 6.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFAGSYL:sm.elev", 6.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFRAEXC:sm.elev", 6.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesQUEROB:sm.elev", 6.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="space", 6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesALNGLU:space", 5.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesBETPEN:space", 5.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFAGSYL:space", 5.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFRAEXC:space", 5.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesQUEROB:space", 5.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="cc", 5, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesALNGLU:cc", 4.9, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesBETPEN:cc", 4.8, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFAGSYL:cc", 4.7, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesFRAEXC:cc", 4.6, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="speciesQUEROB:cc", 4.5, simple$Jvar)

simple$Jvar<-ifelse(simple$var=="m.index:cc", 4, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="sp.temp:cc", 3, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="sm.elev:cc", 2, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="space:cc", 1, simple$Jvar)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(7)
estimates<-c("NAO Index", "Mean Spring Temperature", "Elevation", "Space Parameter",
             "NAO Index x \nClimate Change", "Mean Spring Temperature \nx Climate Change",
             "Elevation x \nClimate Chnage", "Space Parameter \nx Climate Change")
estimates<-rev(estimates)
expB<-ggplot(simple, aes(x=`2.5%`, xend=`97.5%`, y=Jvar, yend=Jvar, col=as.factor(species))) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=mean, y=Jvar, col=as.factor(species))) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("1"=expression(paste(italic("Aesculus hippocastanum"))),
                               "2"=expression(paste(italic("Alnus glutinosa"))),
                               "3"=expression(paste(italic("Betula lenta"))),
                               "4"=expression(paste(italic("Fagus sylvatica"))),
                               "5"=expression(paste(italic("Fraxinus excelsior"))),
                               "6"=expression(paste(italic("Quercus robur"))),
                               "0"="Interactive Effect"))+
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  scale_y_discrete(limits = sort(unique(simple$var)), labels=estimates) +
  xlab("Change in Number of False Springs") + ylab("") +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = c(0.85,0.15),
        legend.text.align = 0) #+ coord_cartesian(ylim=c(1,5), xlim=c(-20, 10))
quartz()
expB




sumer.ni[grep("mu_", rownames(sumer.ni)),]


allspp.cues <- data.frame(
  nao=sumer.ni[grep("m.index", rownames(sumer.ni)),],
  mat=sumer.ni[grep("sp.temp", rownames(sumer.ni)),],
  elev=sumer.ni[grep("sm.elev", rownames(sumer.ni)),],
  space=sumer.ni[grep("space", rownames(sumer.ni)),])

getintxns <- sumer.ni[c("m.index:cc","sp.temp:cc","sm.elev:cc", "space:cc"),]
intxnhere <- getintxns[1,1] # cf is 0.12
forcenums <- seq(-1.3, 0.7, by=0.001)
colhere <- "deepskyblue"
if(FALSE){
plot(forcenums*2*intxnhere~forcenums, ylab="estimated effect", 
     xlab="forcing temp", col=alpha(colhere, 0.2), type="l")
lines(forcenums*4*intxnhere~forcenums, ylab="estimated effect", xlab="nao index", col=alpha(colhere, 0.8))
lines(abs(sumer.ni[c("m.index", "m.index:speciesALNGLU", "m.index:speciesBETPEN",
                     "m.index:speciesFAGGRA", "m.index:speciesFRAEXC", "m.index:QUEROB"),][1]*forcenums)~forcenums)
}

plot(nao~cc, data=allspp.cues)


nao<- plot_model(ben, type = "pred", terms = c("m.index", "cc")) + xlab("NAO") + ylab("Number of False Springs") + ggtitle("") + guides(name="Before or After 1983")
elev<- plot_model(ben, type = "pred", terms = c("sm.elev", "cc")) + xlab("Elevation") + ylab("Number of False Springs") + ggtitle("") + guides(name="Before or After 1983")
mat<- plot_model(ben, type = "pred", terms = c("sp.temp", "cc")) + xlab("Mean Spring Temperature") + ylab("Number of False Springs") + ggtitle("") + guides(name="Before or After 1983")
space<- plot_model(ben, type = "pred", terms = c("space", "cc")) + xlab("Space Parameter") + ylab("Number of False Springs") + ggtitle("") + guides(name="Before or After 1983")

quartz()
ggarrange(nao, elev, mat, space, ncol=2, nrow=2)


fit2<-lmer(fs.count~m.index+sp.temp+cc+sm.elev+space+m.index:cc+sp.temp:cc+
             sm.elev:cc+space:cc+(m.index+sp.temp+cc|species), data=bb)

nao<-interact_plot(model = fit2, pred = m.index, modx = cc)
mat<-interact_plot(model = fit2, pred=sp.temp, modx=cc)
elev<-interact_plot(model = fit2, pred = sm.elev, modx = cc)
space<-interact_plot(model=fit2, pred=space, modx=cc)

quartz()
ggarrange(nao, mat, elev, space, ncol=2, nrow=2)

loo.ben<-loo(post)
