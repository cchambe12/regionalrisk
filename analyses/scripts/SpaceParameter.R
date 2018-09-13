#### 29 January 2018 - Cat
### Building eigenvector matrix for spatial autocorrelation to reduce collinearity issues
## Amazing advice from Nacho

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(tidyr)
library(spdep)
library(adespatial)


bb<-read.csv("/n/wolkovich_lab/Lab/Cat/fs_yearsitespp.csv", header=TRUE)
bb$lat.long<-paste(bb$lat, bb$long)
bb<-subset(bb, select=c("long", "lat", "year", "fs.count", "lat.long"))
bb<-bb[!duplicated(bb),]
bb$fs.num<-ave(bb$fs.count, bb$lat.long, FUN=sum)

bprep<-subset(bb, select=c("fs.num", "lat.long"))
bprep<-bprep[!duplicated(bprep),]
bprep$y<-bprep$fs.num
bprep<-subset(bprep, select=c("lat.long", "y"))
bprep<-bprep[!duplicated(bprep),]
bcoord<-subset(bprep, select="lat.long")
bcoords<-as.data.frame(bcoord[!duplicated(bcoord),])
bcoords<-tidyr::separate(data = bcoords, col = 1, into = c("lat", "long"), sep = "\\ ")
bcoords$lat<-as.numeric(bcoords$lat)
bcoords$long<-as.numeric(bcoords$long)

MEM_model <-"positive"
bcoords<-as.matrix(bcoords)

graph2nb <- function(gob, row.names=NULL,sym=FALSE) {
  if (!inherits(gob, "Graph")) stop("Not a Graph object")
  res <- vector(mode="list", length=gob$np)
  if (!is.null(row.names)) {
    if(length(row.names) != gob$np)
      stop("row.names wrong length")
    if (length(unique(row.names)) != length(row.names))
      stop("non-unique row.names given")
  }
  if (gob$np < 1) stop("non-positive gob$np")
  if (is.null(row.names)) row.names <- as.character(1:gob$np)
  if(sym){
    for (i in 1:gob$np) {
      res[[i]] <- sort(unique(c(gob$to[gob$from==i],
                                gob$from[gob$to==i])))
      if(length(res[[i]]) == 0L) res[[i]] <- 0L
    }
  }
  else{
    for (i in 1:gob$np) {
      res[[i]] <- sort(gob$to[gob$from==i])
      if(length(res[[i]]) == 0L) res[[i]] <- 0L
    }
  }
  attr(res, "region.id") <- row.names
  attr(res, "call") <- attr(gob, "call")
  attr(res, "type") <- attr(gob, "type")
  class(res) <- "nb"
  res <- sym.attr.nb(res)
  res
}

gabrielneigh <- function(coords, nnmult=3) {
  x <- coords
  if (!is.matrix(x)) stop("Data not in matrix form")
  if (any(is.na(x))) stop("Data cannot include NAs")
  np <- nrow(x)
  if(ncol(x)!=2) stop("Planar graphs only work in 2d")
  ngaballoc <- np*nnmult
  g1<-g2<-rep(0,ngaballoc)
  nogab <- 0
  storage.mode(x) <- "double"
  z <- .C("compute_gabriel", np=as.integer(np), from=as.integer(g1),
          to=as.integer(g2), nedges=as.integer(nogab), 
          ngaballoc=as.integer(ngaballoc), x=x[,1], 
          y=x[,2], PACKAGE="spdep")
  z$from<-z$from[1:z$nedges]
  z$to<-z$to[1:z$nedges]
  attr(z, "call") <- match.call()
  class(z)<-c("Graph","Gabriel")
  z
}

#bc<-bcoords[sample(nrow(bcoords), 100, replace=FALSE),]
nb<-graph2nb(gabrielneigh(as.matrix(bcoords)), sym=TRUE)

nb2listw <- function(neighbours, glist=NULL, style="W", zero.policy=FALSE)
{
  if(class(neighbours) != "nb") stop("Not a neighbours list")
  n <- length(neighbours)
  cardnb <- card(neighbours)
  if (!zero.policy)
    if (any(cardnb == 0)) stop("Empty neighbour sets found")
  vlist <- vector(mode="list", length=n)
  if (is.null(glist)) {
    glist <- vector(mode="list", length=n)
    for (i in 1:n)
      if(cardnb[i] > 0) glist[[i]] <- rep(1, length=cardnb[i])
      else glist[[i]] <- NULL
      attr(vlist, "binary") <- TRUE
  } else {
    attr(vlist, "general") <- TRUE
    source <- deparse(substitute(glist))
    attr(vlist, as.character(source)) <- TRUE
  }
  if (length(glist) != n) stop("glist wrong length")
  if (cardnb != unlist(lapply(glist, length)))
    stop("neighbours and glist do not conform")
  if (any(is.na(unlist(glist))))
    stop ("NAs in general weights list")
  attr(vlist, as.character(style)) <- TRUE
  if (style == "W") {
    d <- unlist(lapply(glist, sum))
    for (i in 1:n) {
      if (cardnb[i] > 0) vlist[[i]] <- (1/d[i]) * glist[[i]]
      else vlist[[i]] <- NULL
    }
  }
  if (style == "B") {
    for (i in 1:n) {
      if (cardnb[i] > 0) vlist[[i]] <- rep(1, cardnb[i])
      else vlist[[i]] <- NULL
    }
  }
  if (style == "C") {
    D <- sum(unlist(glist))
    if (is.na(D) || !(D > 0))
      stop(paste("Failure in sum of weights:", D))
    for (i in 1:n) {
      if (cardnb[i] > 0)
        vlist[[i]] <- (n/D) * glist[[i]]
      else vlist[[i]] <- NULL
    }
  }
  if (style == "S") {
    glist2 <- lapply(glist, function(x) x^2)
    q <- sqrt(unlist(lapply(glist2, sum)))
    for (i in 1:n) {
      if (cardnb[i] > 0)
        glist[[i]] <- (1/q[i]) * glist[[i]]
      else glist[[i]] <- NULL
    }
    Q <- sum(unlist(glist))
    if (is.na(Q) || !(Q > 0))
      stop(paste("Failure in sum of intermediate weights:", Q))
    for (i in 1:n) {
      if (cardnb[i] > 0)
        vlist[[i]] <- (n/Q) * glist[[i]]
      else glist[[i]] <- NULL
    }
  }
  style <- style
  if (!zero.policy)
    if (any(is.na(unlist(vlist))))
      stop ("NAs in coding scheme weights list")
  res <- list(style=style, neighbours=neighbours, weights=vlist)
  class(res) <- "listw"
  attr(res, "region.id") <- attr(neighbours, "region.id")
  attr(res, "call") <- match.call()
  invisible(res)
}

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


write.csv(prep_space, file="/n/wolkovich_lab/Lab/Cat/fs_space_new.csv", row.names=FALSE)





