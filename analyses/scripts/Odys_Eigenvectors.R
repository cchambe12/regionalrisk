#### 29 January 2018 - Cat
### Building eigenvector matrix for spatial autocorrelation to reduce collinearity issues
## Amazing advice from Nacho

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(dplyr)
library(spdep)
library(adespatial)


# Set Working Directory
xx<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_yearsitespp.csv")
bb<-subset(xx, xx$year>1950)
bb$lat.long<-paste(bb$lat, bb$long)
bb<-subset(bb, select=c("long", "lat", "year", "fs.count", "lat.long"))
bb<-bb[!duplicated(bb),]
bb$fs.num<-ave(bb$fs.count, bb$lat.long, FUN=sum)

bprep<-bb%>%dplyr::select(fs.num, lat.long)
bprep<-bprep[!duplicated(bprep),]
bprep$y<-bprep$fs.num
bprep<-dplyr::select(bprep, lat.long, y)
bprep<-bprep[!duplicated(bprep),]
bcoord<-bprep%>%dplyr::select(lat.long)
bcoords<-as.data.frame(bcoord[!duplicated(bcoord),])
bcoords<-separate(data = bcoords, col = 1, into = c("lat", "long"), sep = "\\ ")
bcoords$lat<-as.numeric(bcoords$lat)
bcoords$long<-as.numeric(bcoords$long)

## Based on Bauman et al... ##
MEM_model <-"positive"
nb<-graph2nb(gabrielneigh(as.matrix(bcoords)), sym=TRUE)
listw<-nb2listw(nb, style ="B")


y<-as.vector(bprep$y)

MEM.moransel <- function (y, listw, MEM.autocor = c("positive", "negative", "all"), 
                          nperm = 999, alpha = 0.0001) {
  
  SPATIAL = "FALSE"
  # number of regions:
  nb_sites <- length(y)
  
  MEM.autocor <- match.arg(MEM.autocor)
  MEM <- scores.listw(listw, MEM.autocor = MEM.autocor)
  
  I.test <- function (y, listw, nperm, MEM.autocor, alpha) {
    # The function I.test() tests the significance of the Moran's I of 'y' by 'nperm'
    # permutations. If 'MEM.autocor' = 'all', two p-values are computed to test
    # whether the observed Moran's I is greater or smaller than expected by chance, 
    # respectively. The p-values are then corrected by the Sidak correction for multiple
    # tests to control the type I error rate.
    # The function returns "TRUE" if at least one (corrected) p-value is <= 'alpha', and
    # "FALSE" otherwise.
    if (MEM.autocor == "all")
      alter <- c("greater", "less")
    else {
      if (MEM.autocor == "positive") 
        alter <- "greater"
      else alter <- "less"
    }
    p <- sapply(alter, function (x) moran.mc(y, listw, nperm, alternative = x)$p.value)
    # Correction of both p-values with the Sidak correction if 'MEM.autocor' = 'all':
    if (MEM.autocor == "all") p <- 1-(1-p)^2
    if (length(which(p <= alpha)) == 0)
      signif <- FALSE
    else signif <- TRUE
    return(signif)
  }
  
  I <- I.test(y, listw, nperm, MEM.autocor, alpha)
  if (I == TRUE) {
    SPATIAL <- "TRUE"
    MEM.sel <- data.frame(row.names = row.names(MEM))
  }
  
  nbloop <- c()
  while (I == TRUE) {
    nbloop <- c(nbloop, 1)                   # Loop counter
    I.vector <- vector("numeric", ncol(MEM)) # For the I computed with each MEM variable
    for (i in 1:ncol(MEM)) {
      mod <- lm(y ~ MEM[, i])
      I.vector[i] <- moran(residuals(mod), listw, nb_sites, Szero(listw))$I
    }
    min.moran <- which.min(abs(I.vector))
    # Selection of the MEM variable(s) best minimizing the Moran's I value of the residuals:
    MEM.sel[, sum(nbloop)] <- MEM[, min.moran]
    colnames(MEM.sel)[sum(nbloop)] <- colnames(MEM)[min.moran]
    y <- residuals(lm(y ~ MEM.sel[, sum(nbloop)]))
    I <- I.test(y, listw, nperm, MEM.autocor, alpha)
  }
  
  if (SPATIAL == "FALSE") return("No significant spatial structure")
  else list(MEM.all = MEM, MEM.select = MEM.sel)
  # By David Bauman; davbauman@gmail.com
}

moransel<-MEM.moransel(y, listw, MEM.autocor=MEM_model, nperm=999, alpha=0.001)

dselect<-as.data.frame(moransel[["MEM.select"]])
dx<-cbind(bprep, dselect)
rex<-dx%>%dplyr::select(-lat.long)
rex.mod<-lm(y~ ., data=rex)
space<-residuals(rex.mod)

b_space<-cbind(bprep, space)
prep_space<-full_join(bb, b_space, by="lat.long")
write.csv(prep_space, file="/n/wolkovich_lab/Lab/Cat/fs_matspspace_new.csv", row.names=FALSE)





