### Started 10 July 2011 ###
### By Lizzie & Megan ###
### This executes a single run of the model and writes out

rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(deSolve)

#define run conditions
#loc <- "C:/Users/Megan/Documents/GitHub/temporalvar/R/"  
#loc <- "/n/wolkovich_lab/temporalvar/R/"
loc <- "/n/regal/wolkovich_lab/temporalvar/R/"

source(paste0(loc, "sourcefiles/getRunParms.R")) #define runtime parameters

for (j in c(1:nruns)){
  itertime1 <- Sys.time()
  print(paste("run ",j,"of ",nruns))
  #define parameters and functions for this run
  source(paste0(loc,"sourcefiles/getEnvt.R"))  #get constant and time-varying envt parms
  source(paste0(loc,"sourcefiles/getSpecies.R"))  #get species characteristics and Rstar
  source(paste0(loc,"sourcefiles/ResCompN.R")) # define within-season ode solver
  
  #Define arrays
  #interannual dynamics set-up (R0 is in getEnvt.R)
  N0 <- rep(100,nsp)          # initial number of seeds (per meter square?)
  N <- matrix(rep(-1), nyrs, nsp) # number of seeds by yr and spp
  rcrt <- matrix(rep(-1), nyrs, nsp) # number of rcrts by yr and spp
  coexist <- matrix(rep(-1),nyrs,nsp)
  
  ## Within-season dynamics set-up
  Bfin <- matrix(rep(0),nyrs,nsp) # biomass at end of year y
  B0  <- matrix(rep(0),nyrs,nsp) # biomass at beginning of year y
  Bout <- list() #each year has a dataframe with time,R(t),Bi(t) of dims(2+nsp,tsteps)
  yout <- NA #nyrs  #last year of output; default is nyrs, unless loop breaks early
  
  for (y in yrs){
    #include a flag for runs with initial and final stationary periods, but no nonstationary period
    if (y==(nonsta[1]+1) && nonsta[2]==0) N[y,] <- N0 #if no ns period, reset for final stationary period
    #get initial biomass for year y
    if (y==1) {
      N[y,] <- N0
      rcrt[y,] <- N0
      coexist[y,] <- (N[y,]>0)*1
      } 
    else {
      rcrt[y,] <- phi*Bfin[y-1,]*s  #number of seeds added from last year's growth
      N[y,]<- N[y-1,]*(1-g[y-1,])*s + rcrt[y,]  #note Bfin already includes N(t) as init cond; USES g here!
      N[y,] <- N[y,]*(N[y,]>ext)  #if density does not exceed ext, set to zero
      coexist[y,] <- (N[y,]>0)*1
      if (!(sum(N[y,]>0))) {
        #yout <- y  #if break the loop, then y-1 is last year before extinction
        #give Bout a value for the last iteration
        Bout[[y]] <- Bout[[y-1]][1,]*0 + c(0,R0[y],b*g[y,]*N[y,]) #get list formatting from prior year
        Bfin[y,] <- b*g[y,]*N[y,]
        print(paste("All species have gone extinct in year",y ))
        break    #if all species have gone extinct, go to next run
      }
    }
    B0[y,] <- b*g[y,]*N[y,] 
    #use deSolve for ResCompN
    R<-R0[y]
    B<-B0[y,]
    State<-c(R=R,B=B)
    Time <- seq(0,ndays,by=dt)
    #set Rstarmin threshold & update rootfun; this accounts for case where the spp with min R* goes extinct
    Rstarmin <- min(Rstar[(N[y,]!=0)])
    rootfun <- function(Time, State, Pars) State[1] -Rstarmin

    Bout[[y]] <- as.data.frame(ode(func = ResCompN, y = State, parms = Pars, times = Time,
                                   rootfun=rootfun))
    Bfin[y,] <-  apply(Bout[[y]][3:(2+nsp)],2,FUN=max)  #final biomass
  }
  ## modelruns includes the variables that are constant across years in one dataframe...
  # then tauI, tauP and Bfin for each year
  yout <- y
  itertime2 <- Sys.time()
  itertime <- difftime(itertime2, itertime1)
  print(paste("run ended at step =", yout," this run took ", itertime," sec"))
  source(paste0(loc,"sourcefiles/getOutput.R"))

}
#stop redirecting Rout that was initiated in getRunParms
sink(type="message")
sink(type="output")

