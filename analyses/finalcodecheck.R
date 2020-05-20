rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(ggplot2)
library(egg)
library(RColorBrewer)
library(broom)
library(dplyr)

setwd("~/Documents/git/regionalrisk/analyses/output")

orig <- read.csv("fs_newspace_orig.csv")
dvr <- read.csv("fs_newspace_dvr.csv")
five <- read.csv("fs_newspace_five.csv")
dvrtemps <- read.csv("fs_newspace_dvrtemps.csv")
verylong <- read.csv("fs_newspace_verylong.csv")
fullleaf <- read.csv("fs_newspace_fullleaf.csv")



table(orig$fs)
#    0      1 
# 582211 172877 
table(orig$fs, orig$species)
#  AESHIP ALNGLU BETPEN FAGSYL FRAEXC QUEROB
#0 111722  63798 108212  99561  84258 114660
#1  44746  27296  46685  29237   8256  16657
table(orig$fs, orig$cc)
#      cc0      cc1
#fs0  317457 264754
#fs1  92154  80723



table(dvr$fs)
#    0      1 
# 622565 132463 
table(dvr$fs, dvr$species)
#  AESHIP ALNGLU BETPEN FAGSYL FRAEXC QUEROB
#0 115458  63798 111954 118572  88798 123985
#1  41009  27296  42940  10199   3705   7314
table(dvr$fs, dvr$cc)
#     0      1
#0 339970 282595
#1  69615  62848



table(five$fs)
#    0      1 
#  730996  23855
table(five$fs, five$species)
#  AESHIP ALNGLU BETPEN FAGSYL FRAEXC QUEROB
#0 150175  86468 147868 125167  91766 129552
#1   6230   4612   6974   3582    729   1728
table(five$fs, five$cc)
#     0      1
#0  396901 334095
#1  12578  11277




table(dvrtemps$fs)
#   0      1 
# 709538  45369 
table(dvrtemps$fs, dvrtemps$species)
#  AESHIP ALNGLU BETPEN FAGSYL FRAEXC QUEROB
#0 148619  91077 146065 114717  87600 121460
#1   7786      0   8767  14063   4905   9848
table(dvrtemps$fs, dvrtemps$cc)
#    0      1
#0 383227 326311
#1  26287  19082



table(verylong$fs)
#    0      1 
# 525272 229785 
table(verylong$fs, verylong$species)
#  AESHIP ALNGLU BETPEN FAGSYL FRAEXC QUEROB
#0  94241  54368  89514  93463  82505 111181
#1  62217  36729  65368  35332  10008  20131
table(verylong$fs, verylong$cc)
#    0      1
#0 280390 244882
#1 129196 100589


table(fullleaf$fs)
#    0      1 
#745813   9007
table(fullleaf$fs, fullleaf$species)
#   AESHIP ALNGLU BETPEN FAGSYL FRAEXC QUEROB
#0 153583  89273 151653 128054  92305 130945
#1   2818   1805   3176    687    190    331
table(fullleaf$fs, fullleaf$cc)
#    0      1
#0 403712 342101
#1   5747   3260


