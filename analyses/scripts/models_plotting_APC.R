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
library(sjmisc)
library(sjPlot)
library(ggeffects)
library(broom)

setwd("~/Documents/git/regionalrisk/")

load("orig_full.Rdata")

sumer.orig<-as.data.frame(tidy(orig.full,robust = TRUE, prob=0.5))



