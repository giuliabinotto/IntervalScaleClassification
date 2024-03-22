########################################################################
##### Interval scale classification with unbounded rightmost class #####
############################# STC Measure ##############################
########################################################################

### Arguments and notation
# C             = (matrix) Confusion matrix obtained by a validation procedure
# L             = (vector) Length of the n-1 intervals (all except the rightmost)
# Lmin          = (number) Lower extreme of the interval in which to look for the minimum
# Lmax          = (number) Upper extreme of the interval in which to look for the minimum
# eps           = (number) Step size for evaluate TCmax.ril


### Sources for functions
source("BD_OC_TCintervals.R")


### Functions

## xmin.upper: Supremum for xmin (given by theory).
# It can be used for Lmax in the following functions
xmin.upper <- function(C, L) {
    n <- colSums(C)
    if (length(unique(n)) == 1) {
        Lmax <- sum(L)
    } else {
        Lmax <- sum(C[,ncol(C)])*sum(L)
    }
    Lmax
}
    

## TCmax.unbounded: Interval scale Total misclassification Cost with unbounded rightmost class
# Given a confusion matrix C and a vector of interval lengths L (excluding the length of the last interval), the function returns the Total misclassification Cost of C.
# Lmin and Lmax represent the extremes of the interval in which to look for the minimum.
# eps represents the length of the grid used to evaluate the function and generate its plot.
TCmax.unbounded <- function(C, L, Lmin = 0.0001, Lmax, eps) {
    # Check parameters
    stopifnot("The argument C of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument C of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument C of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    stopifnot("The number of classes and the number of intervals do not match" = nrow(C) == (length(L)+1) )
    stopifnot("The argument L is not a lengths vector (not all its elements are positive)" = all(L > 0))
    
    # TCmax depending on the rightmost interval length (ril)
    TCmax.ril <- function(x) {
        Lx <- c(L, x)
        TCmax <- TCmax.int(C, Lx)
        TCmax$TCmx
    }
    
    # Minimum TCmax
    minTCmax <- optimize(TCmax.ril, interval = c(Lmin, Lmax), maximum = FALSE, tol = 10^(-10))
    names(minTCmax) <- c("min.rightint", "min.TCmax")
    
    # Data for plot TCmax
    l <- seq(Lmin, Lmax, by = eps)
    TCmax.l <- sapply(l, TCmax.ril)
    plotTCmax <- data.frame(rightint = l, TCmax.rightint = TCmax.l)
    
    # Output
    output <- list(minTCmax = minTCmax, plotTCmax = plotTCmax)
    output
}


## STC.unbounded: Interval scale Standard Total misclassification Cost with unbounded rightmost class
# Given a confusion matrix C and a vector of interval lengths L (excluding the length of the last interval), the function returns the Standard Total misclassification Cost of C.
# Lmin and Lmax represent the extremes of the interval in which to look for the minimum.
# eps represents the length of the grid used to evaluate the function and generate its plot.
STC.unbounded <- function(C, L, Lmin = 0.0001, Lmax, eps) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all elements are integer numbers)" = all(C == floor(C)))
    stopifnot("The number of classes and the number of intervals do not match" = nrow(C) == (length(L)+1) )
    stopifnot("The argument L is not a lengths vector (not all its elements are positive)" = all(L > 0))
    
    # Minimum for TCmax
    Lr <- TCmax.unbounded(C = C, L = L, Lmin = Lmin, Lmax = Lmax, eps = eps)$minTCmax$min.rightint
    
    # New vector of intervals' lengths
    L <- c(L, Lr)
    
    # STC
    STC <- TC.int(C, L)/TCmax.int(C, L)$TCmx
    STC
}


## Particular case: r=3, n1=n2=n3, l1=1 (Proposition from theory)
# Theoric proposition
prop.min <- function(l2, n) {
    L <- l2
    
    if (L <= 1) {
        min.x <- sqrt(L/(1+L))
        min.TCmax <- n*(2*sqrt((L+1)/L)+2*L+4+1/L)
    } else if (L > 1 & L <= (1+sqrt(5))/2) {
        min.x <- L/sqrt(1+L)
        min.TCmax <- n*(2*sqrt(L+1)+3*L+3+1/L)
    } else if (L > (1+sqrt(5))/2 & L <= (3+sqrt(5))/2) {
        min.x <- L*(sqrt(5)-1)/2
        min.TCmax <- n*L/2*((sqrt(5)+1)*L+sqrt(5)+7)
    } else {
        min.x <- sqrt(L)
        min.TCmax <- n*L*(2*sqrt(L)+L+3)
    }
    minimum <- list(min.x = min.x, min.TCmax = min.TCmax)
    minimum
}


