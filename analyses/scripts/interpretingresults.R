### Working on Log-Odds and back converting model output for manuscript
# 24 May 2019 by Cat

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(brms)

## Set Working Directory
setwd("~/Documents/git/regionalrisk")

load("orig_full.Rdata")

fs <- read.csv("analyses/output/fs_newspace_orig.csv", header=TRUE)

#### Using Gelman-Hill method
invlogit <- function(x) {(1/(1+exp(-(x))))}

#                        Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
#Intercept                -0.88      0.01    -0.90    -0.87       2340 1.00
#nao.z                     0.14      0.01     0.12     0.17       1408 1.00
#mat.z                    -0.48      0.01    -0.50    -0.45       1291 1.00
#dist.z                    0.40      0.02     0.37     0.43       1184 1.00
#elev.z                    0.19      0.02     0.16     0.22       1137 1.00
#space.z                  -0.06      0.01    -0.09    -0.04       1460 1.00
#cc.z                      0.35      0.01     0.33     0.38       1212 1.00
#nao.z:cc.z               -0.83      0.01    -0.85    -0.80       3596 1.00
#mat.z:cc.z                0.42      0.01     0.39     0.45       2608 1.00
#dist.z:cc.z              -0.12      0.02    -0.16    -0.09       2580 1.00
#elev.z:cc.z               0.00      0.02    -0.03     0.03       2106 1.00
#space.z:cc.z             -0.05      0.01    -0.07    -0.03       3971 1.00

int = -0.88
linpreddiff <- function(coef, pred, interval) {(invlogit(int + (coef/(sd(pred)*2))*mean(pred)) - 
                                                invlogit(int + (coef/(sd(pred)*2))*(mean(pred)-interval)))*100} ## Based on UCLA and Gelman-Hill, 2007 pg 81

####### ADD DIVIDE BY 4 RULE BACK INTO CODE!!! #####

sd(fs$mst) # 1.56
linpreddiff(-0.48, fs$mst, 2) # -3.331% for every 2 degrees
sd(fs$nao) #0.28
linpreddiff(0.14, fs$nao, 0.3) # 1.54% for every 0.3 index
sd(fs$distkm) # 141.67
linpreddiff(0.4, fs$distkm, 150) # 4.83% for every 150 kilometers
sd(fs$elev) # 214.193
linpreddiff(0.19, fs$elev, 200) # 1.89% for every 200m
2*sd(fs$cc) # before vs after
linpreddiff(0.35, fs$cc, 1) # 7.225%


### Now let's work on the interactions...
# According to Gelman & Hill 2007, should just say the predictor's effect was reduced (if opposite signs) or increased (if both positive or both negative) with interaction


#### Now try and make fake data to really tease this apart and test
set.seed(1234)
ndata <- 1000
agri <- rnorm(ndata, 0, 10)
cc <- rnorm(ndata, 0, 1)
ba <- -0.5
bc <- 0.5
a <- -1
z <- a + ba*agri + bc*cc
p <- 1/(1+exp(-z))
y <- rbinom(ndata, 1, p)

m0 <- lm(y ~ agri + cc + agri + agri:cc)

m1 <- glm(y ~ agri + cc +agri:cc, family = "binomial")

(coef(m1)[2])/4


int = coef(m1)[1]
testnonz <- function(coef, pred) {(invlogit(int + coef*mean(pred)) - 
                                          invlogit(int + coef*(mean(pred)-1)))*100}

testnonz((coef(m1)[2]), agri) ## -9.922911


agri.z <- (agri-mean(agri,na.rm=TRUE))/(2*sd(agri,na.rm=TRUE))
cc.z <- (cc-mean(cc,na.rm=TRUE))/(2*sd(cc,na.rm=TRUE))

m2 <- glm(y ~ agri.z + cc.z + agri.z:cc.z, family = "binomial")

int = -0.8653
testz <- function(coef, pred) {(invlogit(int + (coef/(sd(pred)*2))*mean(pred)) - 
                                        invlogit(int + (coef/(sd(pred)*2))*(mean(pred)-1)))*100}
zpred <- testz(-8.715, agri)


testz <- function(coef, pred) {(invlogit(int + (coef*(sd(pred)*2))*mean(pred)) - 
                                  invlogit(int + (coef*(sd(pred)*2))*(mean(pred)-1)))*100}

testz <- function(coef, pred) {(invlogit(int + coef*mean(pred))) - 
                                  invlogit(int + (coef*(mean(pred)-1)))*100}

testz <- function(coef, pred) {(int + coef*mean(pred)) - (int + (coef*(mean(pred)-1)))}

div4 = coef/4 #-2.17875


 ##

backpred <- (zpred*2*sd(pred))+mean(pred)


