### Additional Plots for Regional Risk
## Looking at MAT and NAO plus others
## 8 June 2018 - Cat

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(egg)
library(purrr)
library(broom)
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
  brm(fs.count~sp.temp, data=df, chains=2)
}

models<-mat%>%
  mutate(mod = map(data, sp_mod))

model<-models%>%
  mutate(tidy=map(mod, broom::tidy))

mat.tidy<-unnest(model, tidy)
#mat.df<-unnest(models, data)
#mat.df<-inner_join(mat.df, mat.tidy)
mat.df<-mat.tidy%>%
  select(cc, species, term, estimate)%>%
  spread(term, estimate)


mat<-ggplot(mat.df, aes(x=b_Intercept, y=b_sp.temp)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("Mean Spring Temperature") + xlab("Number of False Springs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 2)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  #geom_point(aes(shape=as.factor(cc))) + 
  geom_text(aes(label=species, col=as.factor(cc)),hjust=0, vjust=0, show.legend = FALSE, size=2) +
  scale_shape_manual(values=c(3, 5), labels=c("Before 1983", "After 1983"), name="") #+
#scale_y_continuous(expand = c(0, 0))

bb.ele<-subset(bb, select=c(year, species, cc, elev, fs.count))
bb.ele<-bb.ele[!duplicated(bb.ele),]

ele<-bb.ele%>%
  group_by(cc, species)%>%
  nest

ele_mod<-function(df){
  brm(fs.count~elev, data=df, chains=2)
}

ele.models<-ele%>%
  mutate(mod = map(data, ele_mod))

ele.model<-ele.models%>%
  mutate(tidy=map(mod, broom::tidy))

ele.tidy<-unnest(ele.model, tidy)
#mat.df<-unnest(models, data)
#mat.df<-inner_join(mat.df, mat.tidy)
ele.df<-ele.tidy%>%
  select(cc, species, term, estimate)%>%
  spread(term, estimate)

ele<-ggplot(ele.df, aes(x=b_Intercept, y=b_elev)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("Elevation \n (slope)") + xlab("Number of False Springs \n (intercept)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position = "none") +
  coord_cartesian(xlim=c(0, 1.5)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  #geom_point(aes(shape=as.factor(cc))) + 
  geom_text(aes(label=species, col=as.factor(cc)),hjust=0, vjust=0, show.legend = FALSE, size=2)
#scale_y_continuous(expand = c(0, 0))

bb.nao<-subset(bb, select=c(year, species, cc, m.index, fs.count))
bb.nao<-bb.nao[!duplicated(bb.nao),]

nao<-bb.nao%>%
  group_by(cc, species)%>%
  nest

nao_mod<-function(df){
  brm(fs.count~m.index, data=df, chains=2)
}

nao.models<-nao%>%
  mutate(mod = map(data, nao_mod))

nao.model<-nao.models%>%
  mutate(tidy=map(mod, broom::tidy))

nao.tidy<-unnest(nao.model, tidy)
#mat.df<-unnest(models, data)
#mat.df<-inner_join(mat.df, mat.tidy)
nao.df<-nao.tidy%>%
  select(cc, species, term, estimate)%>%
  spread(term, estimate)

nao<-ggplot(nao.df, aes(x=b_Intercept, y=b_m.index)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("NAO \n (slope)") + xlab("Number of False Springs \n (intercept)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(2, 6.5)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  #geom_point(aes(shape=as.factor(cc))) + 
  geom_text(aes(label=species, col=as.factor(cc)),hjust=0, vjust=0, show.legend = FALSE, size=2)
#scale_y_continuous(expand = c(0, 0))

bb.space<-subset(bb, select=c(species, cc, space, fs.count))
bb.space$space<-round(bb.space$space, digits=3)
bb.space<-bb.space[!duplicated(bb.space),]

space<-bb.space%>%
  group_by(cc, species)%>%
  nest

space_mod<-function(df){
  brm(fs.count~space, data=df, chains=2)
}

space.models<-space%>%
  mutate(mod = map(data, space_mod))

space.model<-space.models%>%
  mutate(tidy=map(mod, broom::tidy))

space.tidy<-unnest(models, tidy)
#mat.df<-unnest(models, data)
#mat.df<-inner_join(mat.df, mat.tidy)
space.df<-space.tidy%>%
  select(cc, species, term, estimate)%>%
  spread(term, estimate)

space<-ggplot(space.df, aes(x=b_Intercept, y=b_space)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("Space \n (slope)") + xlab("Number of False Springs \n (intercept)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        #plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(0, 3.5)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  #geom_point(aes(shape=as.factor(cc))) + 
  geom_text(aes(label=species, col=as.factor(cc)),hjust=0, vjust=0, show.legend = FALSE, size=2)
#scale_y_continuous(expand = c(0, 0))

quartz()
ggarrange(ele, mat, nao, ncol=2, nrow=2)


########### Just linear models below!!! #################
#########################################################

mat<-bb.mat%>%
  group_by(cc, species)%>%
  nest

sp_mod<-function(df){
  lm(fs.count~sp.temp, data=df)
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


mat<-ggplot(mat.df, aes(x=`(Intercept)`, y=sp.temp)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("Mean Spring Temperature \n (slope)") + xlab("Number of False Springs \n (intercept)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        #plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position="none") +
  coord_cartesian(xlim=c(1.5, 6)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  #geom_point(aes(shape=as.factor(cc))) + 
  geom_text(aes(label=species, col=as.factor(cc)),hjust=0, vjust=0, show.legend = FALSE, size=2)
  #scale_y_continuous(expand = c(0, 0))

bb.ele<-subset(bb, select=c(year, species, cc, elev, fs.count))
bb.ele<-bb.ele[!duplicated(bb.ele),]

ele<-bb.ele%>%
  group_by(cc, species)%>%
  nest

ele_mod<-function(df){
  lm(fs.count~elev, data=df)
}

models<-ele%>%
  mutate(mod = map(data, ele_mod))

models<-models%>%
  mutate(tidy=map(mod, broom::tidy))

ele.tidy<-unnest(models, tidy)
#mat.df<-unnest(models, data)
#mat.df<-inner_join(mat.df, mat.tidy)
ele.df<-ele.tidy%>%
  select(cc, species, term, estimate)%>%
  spread(term, estimate)

ele<-ggplot(ele.df, aes(x=`(Intercept)`, y=elev)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("Elevation \n (slope)") + xlab("Number of False Springs \n (intercept)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        #plot.margin = unit(c(1.5,1.5,1.5,2.5), "lines"),
        plot.title=element_text(colour = "firebrick3"), legend.position="none") +
  coord_cartesian(xlim=c(1, 4)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  #geom_point(aes(shape=as.factor(cc))) + 
  geom_text(aes(label=species, col=as.factor(cc)),hjust=0, vjust=0, show.legend = FALSE, size=2)
#scale_y_continuous(expand = c(0, 0))

bb.nao<-subset(bb, select=c(year, species, cc, m.index, fs.count))
bb.nao<-bb.nao[!duplicated(bb.nao),]

nao<-bb.nao%>%
  group_by(cc, species)%>%
  nest

nao_mod<-function(df){
  lm(fs.count~m.index, data=df)
}

models<-nao%>%
  mutate(mod = map(data, nao_mod))

models<-models%>%
  mutate(tidy=map(mod, broom::tidy))

nao.tidy<-unnest(models, tidy)
#mat.df<-unnest(models, data)
#mat.df<-inner_join(mat.df, mat.tidy)
nao.df<-nao.tidy%>%
  select(cc, species, term, estimate)%>%
  spread(term, estimate)

nao<-ggplot(nao.df, aes(x=`(Intercept)`, y=m.index)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("NAO \n (slope)") + xlab("Number of False Springs \n (intercept)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        #plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(4, 7)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  #geom_point(aes(shape=as.factor(cc))) + 
  geom_text(aes(label=species, col=as.factor(cc)),hjust=0, vjust=0, show.legend = FALSE, size=2)
#scale_y_continuous(expand = c(0, 0))

bb.space<-subset(bb, select=c(year, species, cc, space, fs.count))
bb.space<-bb.space[!duplicated(bb.space),]

space<-bb.space%>%
  group_by(cc, species)%>%
  nest

space_mod<-function(df){
  lm(fs.count~space, data=df)
}

models<-space%>%
  mutate(mod = map(data, space_mod))

models<-models%>%
  mutate(tidy=map(mod, broom::tidy))

space.tidy<-unnest(models, tidy)
#mat.df<-unnest(models, data)
#mat.df<-inner_join(mat.df, mat.tidy)
space.df<-space.tidy%>%
  select(cc, species, term, estimate)%>%
  spread(term, estimate)

space<-ggplot(space.df, aes(x=`(Intercept)`, y=space)) + geom_line(aes(col=as.factor(cc)),stat="smooth",method="lm") + ylab("Space \n (slope)") + xlab("Number of False Springs \n (intercept)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        #plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines"),
        plot.title=element_text(colour = "firebrick3")) +
  coord_cartesian(xlim=c(0, 3.5)) + 
  scale_color_manual(values=c("red4", "blue3"), labels=c("Before 1983", "After 1983"), name="") + 
  #geom_point(aes(shape=as.factor(cc))) + 
  geom_text(aes(label=species, col=as.factor(cc)),hjust=0, vjust=0, show.legend = FALSE, size=2)
#scale_y_continuous(expand = c(0, 0))

quartz()
ggarrange(mat, ele, nao, ncol=2, nrow=2)























