### 16 January 2019 - Cat
## Making an Average Predictive Comparison plot
# Or Marginal Effects plot in brms

## "Could you try adding another figure [to Fig 4] using average predictive comparisons 
# (see Gelman and Hill, these are easy to do in BRMS I think as 'marginal effects')...
# you could pick two contrasting species and show what the model predicts across elevations
# for each before and after climate change (4 lines with 50% credible intervals). This may
# help readers interpret the model!!" - Lizzie (21 Nov 2018)

# Housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)
library(ggplot2)

# Set working directory
setwd("~/Documents/git/regionalrisk")

# Load model
load("orig_full.Rdata")

# .. and the data
bb <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)

bb$fs<-ifelse(bb$fs.count>0, 1, 0)

bb$nao.z <- (bb$nao-mean(bb$nao,na.rm=TRUE))/(2*sd(bb$nao,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$eigen-mean(bb$eigen,na.rm=TRUE))/(2*sd(bb$eigen,na.rm=TRUE))

# Start with marginal effects
me <- marginal_effects(orig.full, effects =c("elev.z", "species") )

# Libraries
library(tidybayes)
library(dplyr)
library(modelr)

bb %>%
  group_by(species) %>%
  data_grid(elev.z = seq_range(elev.z, n=1)) %>%
  add_fitted_draws(orig.full, n = 100) %>%
  ggplot(aes(x = elev.z, y = fs, color = ordered(species))) +
  geom_line(aes(y = .value, group = paste(species, .draw)), alpha = .1) +
  geom_point(data = bb) +
  scale_color_brewer(palette = "Dark2")


