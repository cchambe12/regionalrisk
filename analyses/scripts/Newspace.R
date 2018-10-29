### Cat 1 Oct 2018
# Taking another stab at the space parameter

library(adespatial)
library(spdep)
library(vegan)


bb<-read.csv("/n/wolkovich_lab/Lab/Cat/regrisk.nov.csv", header=TRUE)
bb<-read.csv("~/Documents/git/regionalrisk/analyses/output/regrisk.nov.csv", header=TRUE)
MEM_model<-"positive"
style<-"B"

C<-subset(bb, select=c(long, lat))
C$xy<-paste(C$long, C$lat)
C<-C[!duplicated(C$xy),]
C<-subset(C, select=c(long, lat))

nb <- graph2nb(gabrielneigh(as.matrix(C), nnmult=5), sym=TRUE)
listw <- nb2listw(nb, style=style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)

bb$Y<-ave(bb$fs.count, bb$lat.long, FUN=sum)
bbs<-bb[!duplicated(bb$lat.long),]
Y<-bbs$Y

Y <- decostand(Y, method = "hellinger")
C <- as.matrix(C)

X<-subset(bbs, select=c(elev, distkm, mst))

source("/n/wolkovich_lab/Lab/Cat/MEM.moransel.R")
moransel<-MEM.moransel(Y, listw, MEM.autocor = MEM_model, nperm=999, alpha=0.05)

all<-as.data.frame(moransel[["MEM.all"]])

write.csv(all, file="/n/wolkovich_lab/Lab/Cat/memall.csv", row.names=FALSE)

dselect<-as.data.frame(moransel[["MEM.select"]])

#select<-ME(Y~., data=as.data.frame(X), listw = listw, family="gaussian", nsim=80, alpha=0.02)
#MEM.select<-select$vectors

write.csv(dselect, file="/n/wolkovich_lab/Lab/Cat/memselect.csv", row.names=FALSE)
dselect<-read.csv("~/Documents/git/regionalrisk/analyses/output/memselect.csv", header=TRUE)

dx<-cbind(bbs, dselect)
library(dplyr)
rex<-dx%>%dplyr::select(-lat.long, -lat, -long, -species, -distance, -space, -cc, -nao, -year)
rex.mod<-lm(fs.count~ ., data=rex)
space<-residuals(rex.mod)
eigen<-space

bb<-bb%>%dplyr::select(-space)
b_space<-cbind(bbs, eigen)
beig<-subset(b_space, select=c("lat.long", "eigen"))
prep_space<-full_join(bb, beig, by="lat.long")



write.csv(prep_space, file="~/Documents/git/regionalrisk/analyses/output/fs_space_new.csv", row.names=FALSE)
#write.csv(dx, file="/n/wolkovich_lab/Lab/Cat/mem_select.csv", row.names=FALSE)

