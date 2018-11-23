### Baumen code

library(vegan)
library(adespatial)
library(spdep)
library(rstan)

rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

data(mite)
data(mite.xy)
data(mite.env)
X<-mite.env[, 1:2]

Y<-mite
Y<-Y[,2]

C<-mite.xy
C<-as.matrix(C)

MEM_model<-"positive"
style<-"B"

nb<-graph2nb(gabrielneigh(as.matrix(C), nnmult=5), sym=TRUE)
listw<-nb2listw(nb, style=style)
MEM<-scores.listw(listw, MEM.autocor=MEM_model)

select<-ME(Y~., data=as.data.frame(X), listw=listw, family=gaussian, nsim=5000, alpha=0.05)


