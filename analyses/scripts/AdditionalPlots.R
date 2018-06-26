### Additional Plots for Regional Risk
## Looking at MAT and NAO plus others
## 8 June 2018 - Cat

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
#library(egg)
#library(purrr)
#library(broom)
library(brms)


# Setting working directory
setwd("~/Documents/git/regionalrisk/analyses/")

bb<-read.csv("output/regrisk.cleaned.csv", header=TRUE)

#### Let's try Hadley's Methods!
# Maybe subset by each type of model I want to run...?
bb.mat<-subset(bb, select=c(year, species, cc, sp.temp, fs.count))
bb.mat<-bb.mat[!duplicated(bb.mat),]

mat<-bb.mat%>%
  group_by(cc, species)%>%
  nest

sp_mod<-function(df){
  glm(fs.count~sp.temp, data=df, family=poisson)
}

models<-mat%>%
  mutate(mod = map(data, sp_mod))

models<-models%>%
  mutate(tidy=map(mod, broom::tidy))

mat.tidy<-unnest(models, tidy)
#mat.df<-unnest(models, data)
#mat.df<-inner_join(mat.df, mat.tidy)
mat.df<-mat.tidy%>%
  select(cc, species, term, estimate)%>%
  spread(term, estimate)


test<-ggplot(mat.df, aes(y=`(Intercept)`, x=sp.temp)) + geom_point(aes(col=as.factor(species), size=as.factor(cc))) +
  geom_smooth(se=FALSE) + xlab("Mean Spring Temperature") + ylab("Number of False Springs")


mat<-ggplot(mat.df, aes(x=`(Intercept)`, y=sp.temp)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("Mean Spring Temperature") + xlab("Number of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  #coord_cartesian(xlim=c(-14, 15), ylim=c(0, 9)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))



dxx<-read.csv("output/fs_matspring.csv", header=TRUE)
x<-read.csv("output/fs_bb_sitedata.csv", header = TRUE)

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

dxx$fs.ave<-ave(dxx$fs.prop, FUN=median)

x<-x%>%rename(lat=LAT)%>%rename(long=LON)%>%rename(elev=ALT)
x<-dplyr::select(x, species, year, bb, bb.yr, lat, long, elev)
x<-x[!duplicated(x),]

dxx<-subset(dxx, select=c("species", "year", "lat", "long", "fs.prop", "spp.prop", "num.sites"))

df<-inner_join(bb, dxx)
df<-inner_join(df, x)
df<-df[!duplicated(df),]

### Let's look at NAO and bb
cols<-c("bb", "cc", "m.index", "species")
bb.nao<-subset(df, select=cols)
bb.nao<-bb.nao[!duplicated(bb.nao),]

#bb.nao$species<-as.numeric(as.factor(bb.nao$species))
bbnao.mod<-brm(bb~cc+m.index+cc:m.index+(cc+m.index+cc:m.index|species), data=bb.nao, chains=2, cores=4, family=poisson)

m<-bbnao.mod
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
dflong<- tidyr::gather(dftot, var, value, cc:`cc:m.index`, factor_key=TRUE)

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

estimates<-c("Before or After 1983", "NAO", "CC x NAO")
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


dsamp<-df[sample(nrow(df), 500), ]
### Some plots!
ggplot(df, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)), stat="smooth", method="auto")

df$mat<-as.integer(round(df$sp.temp, digits=0))
df$matx<-NA
df$matx<-ifelse(df$mat<=-10, 1, df$matx)
df$matx<-ifelse(df$mat>-10&df$mat<=-6, 2, df$matx)
df$matx<-ifelse(df$mat>-6&df$mat<=-2, 3, df$matx)
df$matx<-ifelse(df$mat>-2&df$mat<=2, 4, df$matx)
df$matx<-ifelse(df$mat>2&df$mat<=6, 5, df$matx)
df$matx<-ifelse(df$mat>6&df$mat<=10, 6, df$matx)
df$matx<-ifelse(df$mat>10, 7, df$matx)
df$matx<-as.integer(df$matx)

df$fs<-ave(df$fs.count, df$matx)
df$fs<-as.integer(round(df$fs, digits=0))

ggplot(df, aes(x=fs.count, y=sp.temp)) + geom_bar(aes(col=as.factor(cc)), position="dodge")

### Just using the cleaned bb dataset - ELEVATION
aeship<-subset(bb, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                       panel.background = element_blank(), 
                                                                       axis.line = element_line(colour = "black"), legend.key=element_blank(),
                                                                       plot.margin = unit(c(2,2,2,2), "lines"),
                                                                       plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

alnglu<-subset(bb, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                panel.background = element_blank(), 
                                                                axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                plot.margin = unit(c(2,2,2,2), "lines"),
                                                                plot.title=element_text(colour = "orangered1"),legend.position = "none") +
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

betpen<-subset(bb, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Betula pendula")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "orange3")) +
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

fagsyl<-subset(bb, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fagus sylvatica"))))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "sienna2"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

fraexc<-subset(bb, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank(), 
                                                                   axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                   plot.margin = unit(c(2,2,2,2), "lines"),
                                                                   plot.title=element_text(colour = "green4"), legend.position = "none")+
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

querob<-subset(bb, species=="QUEROB")
qrob<-ggplot(querob, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Number of False Springs") + 
  ggtitle(expression(paste(italic("Quercus robur")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                              panel.background = element_blank(), 
                                                              axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                              plot.margin = unit(c(2,2,2,2), "lines"),
                                                              plot.title=element_text(colour = "purple2"))+
  coord_cartesian(xlim=c(0, 1500), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))


ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)


#### Now for Mean spring temp
#aeship<-subset(bb, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                       panel.background = element_blank(), 
                                                                       axis.line = element_line(colour = "black"), legend.key=element_blank(),
                                                                       plot.margin = unit(c(2,2,2,2), "lines"),
                                                                       plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#alnglu<-subset(bb, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                panel.background = element_blank(), 
                                                                axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                plot.margin = unit(c(2,2,2,2), "lines"),
                                                                plot.title=element_text(colour = "orangered1"), legend.position = "none") +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#betpen<-subset(bb, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Betula pendula")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "orange3")) +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

#fagsyl<-subset(bb, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fagus sylvatica"))))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "sienna2"), legend.position = "none") +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#fraexc<-subset(bb, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank(), 
                                                                   axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                   plot.margin = unit(c(2,2,2,2), "lines"),
                                                                   plot.title=element_text(colour = "green4"), legend.position = "none")+
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#querob<-subset(bb, species=="QUEROB")
qrob<-ggplot(querob, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Number of False Springs") + 
  ggtitle(expression(paste(italic("Quercus robur")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                              panel.background = element_blank(), 
                                                              axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                              plot.margin = unit(c(2,2,2,2), "lines"),
                                                              plot.title=element_text(colour = "purple2"))+
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

quartz()
ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)


### Now for NAO
#aeship<-subset(bb, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                       panel.background = element_blank(), 
                                                                       axis.line = element_line(colour = "black"), legend.key=element_blank(),
                                                                       plot.margin = unit(c(2,2,2,2), "lines"),
                                                                       plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#alnglu<-subset(bb, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                panel.background = element_blank(), 
                                                                axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                plot.margin = unit(c(2,2,2,2), "lines"),
                                                                plot.title=element_text(colour = "orangered1"),legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#betpen<-subset(bb, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Betula pendula")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "orange3")) +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

#fagsyl<-subset(bb, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fagus sylvatica"))))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "sienna2"), legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#fraexc<-subset(bb, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank(), 
                                                                   axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                   plot.margin = unit(c(2,2,2,2), "lines"),
                                                                   plot.title=element_text(colour = "green4"), legend.position = "none")+
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#querob<-subset(bb, species=="QUEROB")
qrob<-ggplot(querob, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Number of False Springs") + 
  ggtitle(expression(paste(italic("Quercus robur")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                              panel.background = element_blank(), 
                                                              axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                              plot.margin = unit(c(2,2,2,2), "lines"),
                                                              plot.title=element_text(colour = "purple2"))+
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))


quartz()
ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)

## Now for Space
#aeship<-subset(bb, species=="AESHIP")
ahip<-ggplot(aeship, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Aesculus hippocastanum")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                       panel.background = element_blank(), 
                                                                       axis.line = element_line(colour = "black"), legend.key=element_blank(),
                                                                       plot.margin = unit(c(2,2,2,2), "lines"),
                                                                       plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#alnglu<-subset(bb, species=="ALNGLU")
aglu<-ggplot(alnglu, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Alnus glutinosa")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                panel.background = element_blank(), 
                                                                axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                plot.margin = unit(c(2,2,2,2), "lines"),
                                                                plot.title=element_text(colour = "orangered1"),legend.position = "none") +
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#betpen<-subset(bb, species=="BETPEN")
bpen<-ggplot(betpen, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Betula pendula")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "orange3")) +
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))

#fagsyl<-subset(bb, species=="FAGSYL")
fsyl<-ggplot(fagsyl, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fagus sylvatica"))))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                               panel.background = element_blank(), 
                                                               axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                               plot.margin = unit(c(2,2,2,2), "lines"),
                                                               plot.title=element_text(colour = "sienna2"), legend.position = "none") +
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#fraexc<-subset(bb, species=="FRAEXC")
fexc<-ggplot(fraexc, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") +
  ggtitle(expression(paste(italic("Fraxinus excelsior")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank(), 
                                                                   axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                                   plot.margin = unit(c(2,2,2,2), "lines"),
                                                                   plot.title=element_text(colour = "green4"), legend.position = "none")+
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c(0, 1))

#querob<-subset(bb, species=="QUEROB")
qrob<-ggplot(querob, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Number of False Springs") + 
  ggtitle(expression(paste(italic("Quercus robur")))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                              panel.background = element_blank(), 
                                                              axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank(),
                                                              plot.margin = unit(c(2,2,2,2), "lines"),
                                                              plot.title=element_text(colour = "purple2"))+
  coord_cartesian(xlim=c(-50, 90), ylim=c(0, 8)) + labs(col="Before or After 1983") + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"))


ggarrange(ahip, aglu, bpen, fsyl, fexc, qrob, ncol=3, nrow=2)


#### Not separated by Species
bb.samp<-bb[sample(nrow(bb), 1200, replace=FALSE),]



bb$fs.el<-round(bb$elev, digits=-1)
bb$fs.el<-ave(bb$fs.count, bb$fs.el)
elev<-ggplot(bb.samp, aes(x=elev, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1800), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0)) + facet_wrap(~species)

bb$fs.mat<-round(bb$sp.temp, digits=0)
bb$fs.mat<-ave(bb$fs.count, bb$fs.mat)
mat<-ggplot(bb.samp, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))

bb$fs.n<-round(bb$m.index, digits=2)
bb$fs.n<-ave(bb$fs.count, bb$fs.n)
nao<-ggplot(bb.samp, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))

bb$fs.sp<-round(bb$space, digits=-1)
bb$fs.sp<-ave(bb$fs.count, bb$fs.sp)
space<-ggplot(bb.samp, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(-50, 100), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))


quartz()
ggarrange(elev, mat, nao, space, ncol=2, nrow=2)



#bb$fs.yr<-round(bb$space)

mround <- function(x,base){ 
  base*round(x/base) 
} 

bb$fs.yr<-mround(bb$year, 5) 
bb$fs.yr<-ave(bb$fs.count, bb$fs.yr)
#bb$yr<-mround(bb$year, 5)
bb$matx<-bb$sp.temp+13.61348
bb$matx<-ave(bb$matx, bb$year)
bbx<-bb%>%dplyr::select(year, fs.yr, matx, cc)
bbx<-bbx[!duplicated(bbx),]
prop<-subset(dxx, select=c(year, fs.prop))
prop<-prop[!duplicated(prop),]
bbx<-inner_join(bbx, prop)
year<-ggplot(bbx, aes(x=year, y=fs.prop)) + ylab("Number of False Springs") + 
  geom_point(aes(shape=as.factor(cc))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(1950, 2016), ylim=c(0, .5)) + scale_color_manual(values=c("red4", "blue3"), labels=c("before", "after"), name="Before or After 1983") +
  geom_line(aes(y=matx/60), col="blue", stat="smooth", alpha=0.6, method="loess") + scale_y_continuous(sec.axis = sec_axis(~.*60, name="Mean Spring Temperature"))  +
  scale_shape_manual(values=c(3, 5), labels=c("before", "after"), name="Before or After 1983") + guides(shape = guide_legend(override.aes = list(alpha=1))) #+
  #geom_line(aes(col=as.factor(cc)),stat="smooth",method="auto") + xlab("Year")



#### Try with bigvis package by Hadley!
library('bigvis')
bb<-standardise(bb["fs.count"])
bbe<- condense(bin(bb$fs.count, 1), z=bb$elev, summary="mean")
bbes <- smooth(bbe, 50, var=".mean", type="robust")
elev<-ggplot() + geom_point(data=bbe, aes(y=bb.fs.count, x=.mean)) + geom_line(data=bbes, aes(y=bb.fs.count, x=.mean, col="Smoothed")) + xlab("Elevation") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1800), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))

elev<-ggplot(bb.samp, aes(x=elev, y=standardise(fs.count))) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Elevation") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1800), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))

bb$fs.mat<-round(bb$sp.temp, digits=0)
bb$fs.mat<-ave(bb$fs.count, bb$fs.mat)
mat<-ggplot(bb.samp, aes(x=sp.temp, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Mean Spring Temperature") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(-14, 15), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))

bb$fs.n<-round(bb$m.index, digits=2)
bb$fs.n<-ave(bb$fs.count, bb$fs.n)
nao<-ggplot(bb.samp, aes(x=m.index, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("NAO Index") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(-1.5, 0.75), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))

bb$fs.sp<-round(bb$space, digits=-1)
bb$fs.sp<-ave(bb$fs.count, bb$fs.sp)
space<-ggplot(bb.samp, aes(x=space, y=fs.count)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + xlab("Space") + ylab("Average Number \n of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(-50, 100), ylim=c(0, 9)) + scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  geom_jitter(alpha=0.1, aes(shape=as.factor(cc))) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") + guides(shape = guide_legend(override.aes = list(alpha=1))) +
  scale_y_continuous(expand = c(0, 0))


quartz()
ggarrange(elev, mat, nao, space, ncol=2, nrow=2)

















