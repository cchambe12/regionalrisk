## Started 22 July 2019 (though really started in June or such) ##
## Trying to understand log-odds, again ##
## Seems like we're good on back-converting, finally! ##

## Lizzie's personal code, with edits by Cat ##
## See also converting.pdf ##

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/teaching") 
} else setwd("~/Documents/git")

set.seed(7777)
ndata <- 1000
agri <- rnorm(ndata, 0, 10)
cc <- rnorm(ndata, 0, 1)
ba <- -0.5
bc <- 0.5
a <- -1
z <- a + ba*agri + bc*cc
p <- 1/(1+exp(-z))
y <- rbinom(ndata, 1, p)

m1 <- glm(y ~ agri + cc +agri:cc, family = "binomial")

agri.z <- (agri-mean(agri,na.rm=TRUE))/(2*sd(agri,na.rm=TRUE))
cc.z <- (cc-mean(cc,na.rm=TRUE))/(2*sd(cc,na.rm=TRUE))

m2 <- glm(y ~ agri.z + cc.z + agri.z:cc.z, family = "binomial")

### Divide by 4 rule: need to multiply by 100 to convert to percent as above
(coef(m1)[2]/4)*100 # without back-converting
(coef(m2)[2]/4)/(sd(agri)*2)*100
(coef(m2)[2]/(4*2*sd(agri)))*100
 
## Gelman-Hill Long Equation: page 81, final bullet point to find different of 1 unit
invlogit <- function(x) {(1/(1+exp(-(x))))}
# Without back converting
intm1 = coef(m1)[1]
testnonz <- function(coef, pred) {(invlogit(intm1 + coef*mean(pred)) - invlogit(intm1 + coef*(mean(pred)-1)))*100}
testnonz((coef(m1)[2]), agri) # -8.6

# With back converting
intm2 = coef(m2)[1]
testz <- function(coef, pred) {(invlogit(intm2 + (coef/(sd(pred)*2))*mean(pred)) - 
                                  invlogit(intm2 + (coef/(sd(pred)*2))*(mean(pred)-1)))*100}
testz(coef(m2)[2], agri) #-6.36



