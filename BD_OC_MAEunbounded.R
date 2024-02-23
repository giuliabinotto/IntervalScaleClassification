########################################################################
##### Interval scale classification with unbounded rightmost class #####
############################# SMAE Measure ##############################
########################################################################

### Arguments and notation
# C             = (matrix) Confusion matrix obtained by a validation procedure
# L             = (vector) Length of the n-1 intervals (all except the rightmost)
# Lmin          = (number) Lower extreme of the interval in which to look for the minimum
# Lmax          = (number) Upper extreme of the interval in which to look for the minimum
# eps           = (number) Step size for evaluate MAEmax.ril


### Sources for functions
source("BD_OC_MAEintervals.R")


### Functions

## MAEmax.unbounded: Interval scale Total misclassification Cost with unbounded rightmost class
# Given a confusion matrix C and a vector of interval lengths L (excluding the length of the last interval), the function returns the Total misclassification Cost of C.
# Lmin and Lmax represent the extremes of the interval in which to look for the minimum.
# eps represents the length of the grid used to evaluate the function and generate its plot.
MAEmax.unbounded <- function(C, L, Lmin = 0.0001, Lmax, eps) {
    # Check parameters
    stopifnot("The argument C of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument C of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument C of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    stopifnot("The number of classes and the number of intervals do not match" = nrow(C) == (length(L)+1) )
    stopifnot("The argument L is not a lengths vector (not all its elements are positive)" = all(L > 0))
    
    # MAEmax depending on the rightmost interval length (ril)
    MAEmax.ril <- function(x) {
        Lx <- c(L, x)
        MAEmax <- MAEmax.int(C, Lx)
        MAEmax$MAEmx
    }
    
    # Minimum MAEmax
    minMAEmax <- optimize(MAEmax.ril, interval = c(Lmin, Lmax), maximum = FALSE, tol = 10^(-10))
    names(minMAEmax) <- c("min.rightint", "min.MAEmax")
    
    # Data for plot MAEmax
    l <- seq(Lmin, Lmax, by = eps)
    MAEmax.l <- sapply(l, MAEmax.ril)
    plotMAEmax <- data.frame(rightint = l, MAEmax.rightint = MAEmax.l)
    
    # Output
    output <- list(minMAEmax = minMAEmax, plotMAEmax = plotMAEmax)
    output
}


## SMAE.unbounded: Interval scale Standard Total misclassification Cost with unbounded rightmost class
# Given a confusion matrix C and a vector of interval lengths L (excluding the length of the last interval), the function returns the Standard Total misclassification Cost of C.
# Lmin and Lmax represent the extremes of the interval in which to look for the minimum.
# eps represents the length of the grid used to evaluate the function and generate its plot.
SMAE.unbounded <- function(C, L, Lmin = 0.0001, Lmax, eps) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all elements are integer numbers)" = all(C == floor(C)))
    stopifnot("The number of classes and the number of intervals do not match" = nrow(C) == (length(L)+1) )
    stopifnot("The argument L is not a lengths vector (not all its elements are positive)" = all(L > 0))
    
    # Minimum for MAEmax
    Lr <- MAEmax.unbounded(C = C, L = L, Lmin = Lmin, Lmax = Lmax, eps = eps)$minMAEmax$min.rightint
    
    # New vector of intervals' lengths
    L <- c(L, Lr)
    
    # SMAE
    SMAE <- MAE.int(C, L)/MAEmax.int(C, L)$MAEmx
    SMAE
}
