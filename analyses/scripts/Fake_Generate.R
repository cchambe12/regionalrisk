### Make Fake data for Regional Risk model choice
## Cat - 24 October 2018

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(truncnorm)


nsp = 6     #total number of species
years= 50 #number of years
nind = 1500 # number of individuals per species
N = nsp*years*nind

spnum <- rep(c(1:nsp), each=nind*years)   #list of species numbers for each observation (length N)
cc <- rep(c(0,1),each=N/2) #assign half to pre 1984 (0) and half to post (1)
mst <- rnorm(N, 8, 1.5)
nao <- rnorm(N, 0.08, 0.3)
space <- rnorm(N, -0.03, 0.33)
elev <- rnorm(N, 260, 215)
dist <- rnorm(N, 13751.49, 270)

################ To fix below this point.... ############


#predictors (t=treatment,s=sex,r=year)
b_cc = -0.8
b_mst = -1.2
b_nao = 0.09
b_space = 0.01
b_elev = 0.6
b_dist = -0.4

#mu_a and sig_a are the mean and variance of the intercept across sites
mu_a <- 7
sig_a <- 2

#for each island, draw the intercept from the appropriate site mean and sd
a_species <- rep(0,nsp)
#assume same within-site variance for all sites
for (j in 1:nsp){
  a_species[j] <- rnorm(5,mu_a, sig_a);
}

#simulate data
sig_y <- 3 #residual variance
y <- rep(0,N)
L <-0 # lower bound on response variable
for (n in 1:N){
  y[n] <- rtruncnorm(7, a=L, b=Inf, a_species[spnum[n]] + b_cc*cc[n] + b_mst*mst[n] +
                       b_nao*nao[n] + b_space*space[n] + b_elev*elev[n] + b_dist*dist[n], sig_y)
}

fake<-data.frame(fs = y, cc = cc, mst = mst, nao = nao, space = space, elev = elev, dist = dist)


