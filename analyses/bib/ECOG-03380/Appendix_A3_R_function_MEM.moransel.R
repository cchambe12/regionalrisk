# Bauman, D. et al. 2018. Disentangling good from bad practices in the selection of spatial or 
# phylogenetic eigenvectors. – Ecography      
# Appendix A3: Function MEM.moransel used to the perform the MIR procedures.

# The function computes MEM variables based on any given listw provided by the user, and 
# select the smallest subset of spatial eigenvectors minimising the autocorrelation (Moran's I) 
# of the residuals of a model containing only an intercept term (no environmental variables).

# 'y' is a univariate response vector; listw is a spatial weighting matrix of class 'listw' 
# (package spdep).

# 'MEM.autocor' indicates whether one is only interested in positive spatial autocorrelation,
# negative spatial autocorrelation, or both.

# If a significant spatial signal was found in 'y', the function returns a list of two elements:
# 'MEM.all' contains the whole set of MEM variables generated with 'listw'
# 'MEM.select' contains the subset of selected MEM variables.
# If no significant spatial signal was found in 'y', then a message indicating that no spatial
# structure could be detected is printed.

MEM.moransel <- function (y, listw, MEM.autocor = c("positive", "negative", "all"), 
                          nperm = 999, alpha = 0.05) {
  
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
