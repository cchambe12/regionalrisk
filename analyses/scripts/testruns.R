## Cat - 12 August 2018
# Ben Goodrich suggested I try the stan_biglm function

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)
library(rstan)
library(rstanarm)
library(future)
library(arm)
library(lme4)
library(jtools)
library(egg)
plan(multiprocess)

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

library(ggmap)
map<-get_map(location="Europe", zoom=4)
mapPoints<-ggmap(map) + geom_point(x=d$long, y=d$lat, aes(col=d$space), data=d) + facet_wrap(~species) +
  theme(legend.position = "none")
mapPoints

spp.nao<-unique(ave(bb$m.index, bb$species))
spp.mat<-unique(ave(bb$sp.temp, bb$species))
spp.cc<-as.matrix(unique(ave(bb$cc, bb$species)))
bb$nao.cc<-bb$m.index*bb$cc
bb$mat.cc<-bb$sp.temp*bb$cc
bb$elev.cc<-bb$sm.elev*bb$cc
bb$space.cc<-bb$space*bb$cc
bb$space.elev<-bb$space*bb$sm.elev

quartz()
bbs<-bb[sample(nrow(bb), 60000),]
lat<-ggplot(d, aes(x=lat, y=space)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")
long<-ggplot(d, aes(x=long, y=space)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")
elev<-ggplot(d, aes(x=elev, y=space)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")
mat<-ggplot(d, aes(x=sp.temp, y=space)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")

space<-ggplot(d[(d$space>=30 | d$space<=-30),], aes(x=long, y=lat)) + geom_point(aes(col=space), alpha=0.3) + 
  facet_wrap(~species) 


space2<-ggplot(d[(d$space<=10 & d$space>=-10),], aes(x=long, y=lat)) + geom_point(aes(col=space), alpha=0.3) + 
  facet_wrap(~species) 

box<-ggplot(d, aes(x=species, y=space)) + geom_boxplot()

ben<-lm(fs.count~m.index*species+sp.temp*species+cc*species+sm.elev+space+m.index:cc + sp.temp:cc +sm.elev:cc +
          space:cc + space:sm.elev, data=bb)             # not necessary in this case

b <- coef(ben)[-1]
R <- qr.R(ben$qr)[-1,-1]
SSR <- crossprod(ben$residuals)[1]
not_NA <- !is.na(fitted(ben))
N <- sum(not_NA)
xbar <- c(as.numeric(mean(bb$m.index)), as.numeric(as.factor("ALNGLU")), as.numeric(as.factor("BETPEN")), 
          as.numeric(as.factor("FAGSYL")), as.numeric(as.factor("FRAEXC")), as.numeric(as.factor("QUEROB")), 
          as.numeric(mean(bb$sp.temp)), as.numeric(mean(bb$cc)), 
          as.numeric(mean(bb$sm.elev)), as.numeric(mean(bb$space)),
          as.numeric(mean(bb$m.index[bb$species=="ALNGLU"])), as.numeric(mean(bb$m.index[bb$species=="BETPEN"])),
          as.numeric(mean(bb$m.index[bb$species=="FAGSYL"])), as.numeric(mean(bb$m.index[bb$species=="FRAEXC"])),
          as.numeric(mean(bb$m.index[bb$species=="QUEROB"])),
          as.numeric(mean(bb$sp.temp[bb$species=="ALNGLU"])), as.numeric(mean(bb$sp.temp[bb$species=="BETPEN"])),
          as.numeric(mean(bb$sp.temp[bb$species=="FAGSYL"])), as.numeric(mean(bb$sp.temp[bb$species=="FRAEXC"])), 
          as.numeric(mean(bb$sp.temp[bb$species=="QUEROB"])), as.numeric(mean(bb$cc[bb$species=="ALNGLU"])), 
          as.numeric(mean(bb$cc[bb$species=="BETPEN"])), as.numeric(mean(bb$cc[bb$species=="FAGSYL"])), 
          as.numeric(mean(bb$cc[bb$species=="FRAEXC"])), as.numeric(mean(bb$cc[bb$species=="QUEROB"])), 
          as.numeric(mean(bb$m.index*bb$cc)), as.numeric(mean(bb$sp.temp*bb$cc)), as.numeric(mean(bb$sm.elev*bb$cc)), 
          as.numeric(mean(bb$space*bb$cc)), as.numeric(mean(bb$space*bb$sm.elev)))
xbarnames<-colnames(R)
names(xbar)<-xbarnames

y <- bb$fs.count[not_NA]
ybar <- mean(y)
s_y <- sd(y)
post.inter <- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y, prior = R2(.75),
                       # the next line is only to make the example go fast
                       chains = 4, iter = 2000)
cbind(lm = b, stan_lm = rstan::get_posterior_mean(post)[27:29,]) # shrunk
# }


fit2<-lmer(fs.count~m.index+sp.temp+cc+sm.elev+space+m.index:cc+sp.temp:cc+
             sm.elev:cc+space:cc+(m.index+sp.temp+cc|species), data=bb)

nao<-interact_plot(model = fit2, pred = m.index, modx = cc)
mat<-interact_plot(model = fit2, pred=sp.temp, modx=cc)
elev<-interact_plot(model = fit2, pred = sm.elev, modx = cc)
space<-interact_plot(model=fit2, pred=space, modx=cc)

quartz()
ggarrange(nao, mat, elev, space, ncol=2, nrow=2)

loo.ben<-loo(post)
