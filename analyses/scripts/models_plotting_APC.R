## Cat - 5 November 2018
# Final model - now some plots!

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(rstan)
library(brms)
library(ggplot2)
library(egg)
library(RColorBrewer)
library(tidybayes)
library(broom)
library(dplyr)

setwd("~/Documents/git/regionalrisk/")

#load("orig_full.Rdata")

check <- bb %>% add_fitted_draws(orig.full, prob=0.5) %>%
  filter(species==c("FRAEXC", "BETPEN"))

foo <- ungroup(check) %>%
  dplyr::select(species, cc, elev, .value)

bb %>%
  add_fitted_draws(orig.full) %>%
  filter(species==c("FRAEXC", "BETPEN")) %>%
  ggplot(aes(x = .value, y = elev, linetype=as.factor(cc), col=species)) +
  stat_pointintervalh(.width = c(.25, .75))




foo <- foo[!duplicated(foo),] ### super close!!! Need to find a way to get 50% cred intervals

ggplot(foo, aes(x=elev, y=.value, col=species, linetype=as.factor(cc))) + geom_line(aes(col=species, linetype=as.factor(cc)), 
                                                                                 stat="smooth", method="lm", se=TRUE, span=0.5)

goober <- posterior_interval(orig.full, prob=0.5)

sumer.orig<-as.data.frame(tidy(orig.full,robust = TRUE, prob=0.5))

sumer.orig$term<-gsub(".*b_","",sumer.orig$term)

sumer.orig<-sumer.orig[(sumer.orig$term=="mat.z:cc.z:speciesBETPEN" | sumer.orig$term=="mat.z:cc.z:speciesFRAEXC" |
                          sumer.orig$term=="elev.z:cc.z:speciesBETPEN" | sumer.orig$term=="elev.z:cc.z:speciesFRAEXC" ),]


