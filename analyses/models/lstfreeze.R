### 15 Oct 2018 - can't install brms, rstanarm or I think rstan properly
## Libraries
require(rstan)
require(brms)

options(stringsAsFactors = FALSE)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

lstfrz <- read.csv("/n/wolkovich_lab/Lab/Cat/lastfreezedates.csv")
lstfrz$cc <- ifelse(lstfrz$year<=1983, 0, 1)

lstfrz.mod <- brm(lastfreeze ~ cc + species + cc:species, data=lstfrz, control=list(max_treedepth = 15,adapt_delta = 0.99), 
                  iter=4000, warmup = 2500, chains=4, cores=4)

save(lstfrz.mod, file="/n/wolkovich_lab/Lab/Cat/lstfrzmod.Rdata")