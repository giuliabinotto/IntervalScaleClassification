##########################################################
############## Interval scale classification #############
###################### STC Measure #######################
##########################################################

### Arguments and notation
# C             = (matrix) Confusion matrix obtained by a validation procedure
# r             = (number) Number of classes (intervals)
# n             = (vector) Number of instances for each interval
# N             = (number) Total number of instances
# L             = (vector) Length of the intervals
# delta         = (vector) Density of the intervals
# haus.dist     = (function) Hausdorff distance between two intervals given in matrix form
# gamma.ij      = (matrix) Matrix used for the calculation of the cost matrix 
# m             = (matrix) Cost matrix
# index.max     = (vector) Vector composed by i_j for j=1,...,r
# delta.index.max   = (vector) Density of the interval i_j for j=1,...,r
# Amax          = (matrix) Matrix representing the class Smax 
# K             = (vector) Vector used for the calculation of TCmax


### Complementary functions

# Density of the intervals (given the confusion matrix and the length of the intervals)
dens <- function(C, L) {
    n <- colSums(C)
    delta <- n/L
    delta
}

# Hausdorff distance between intervals (in matrix form)
haus.dist <- function(L) {
    r <- length(L)
    
    # Lower and upper endpoints of the intervals
    endpoints <- c(0, cumsum(L))
    L.low <- endpoints[1:r]
    L.up <- endpoints[2:(r+1)]
    
    # indexes of the intervals
    d.row <- rep(1:r, times = r)
    d.col <- rep(1:r, each = r)
    
    # Hausdorff distance
    d.low <- abs(L.low[d.row]-L.low[d.col])
    d.up <- abs(L.up[d.row]-L.up[d.col])
    d <- pmax(d.low, d.up)
    d <- matrix(d, nrow = r)
    d
}


### Functions

## TC.int: Interval scale Total misclassification Cost
# Given a confusion matrix C and a vector of interval lengths L, the function returns the Total misclassification Cost of C
TC.int <- function(C, L) {
    # Check parameters
    stopifnot("The argument C of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument C of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument C of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    stopifnot("The number of classes and the number of intervals do not match" = nrow(C) == length(L))
    stopifnot("The argument L is not a lengths vector (not all its elements are positive)" = all(L > 0))
    
    # Variables
    r <- nrow(C)
    n <- colSums(C)
    delta <- dens(C, L)
    Delta <- sum(delta)
    
    # gamma.ij
    gamma.ij <- matrix(rep(Delta-delta, r), nrow = r, byrow=TRUE)/delta
    
    # Cost matrix m.ij
    m <- gamma.ij*haus.dist(L)
    
    # TC
    TC <- sum(C*m)
    TC
}


## TCmax.int = TC(Amax) Interval scale Maximum Total misclassification Cost
# Given a confusion matrix C and a vector of interval lengths L, the function returns a list including:
# index     = Vector of indexes i_j for j=1,...,r
# delta.index   = Vector of density of the class i_j for j=1,...,r
# K         = Vector of K_j for j=1,...,r
# Amax      = Matrix Amax
# TCmx      = Maximum Total misclassification Cost
TCmax.int <- function(C, L) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all elements are integer numbers)" = all(C == floor(C)))
    stopifnot("The number of classes and the number of intervals do not match" = nrow(C) == length(L))
    stopifnot("The argument L is not a lengths vector (not all its elements are positive)" = all(L > 0))
    
    # Variables
    r <-  nrow(C)
    n <- colSums(C)
    delta <- dens(C, L)
    Delta <- sum(delta)
    
    # i.j
    index.max <- haus.dist(L)/delta
    index.max <- apply(index.max, 2, which.max)
    
    # delta.i.j
    delta.index.max <- delta[index.max]
    
    # Amax
    Amax <- matrix(0, r, r)
    for (i in 1:r) {
        Amax[index.max[i],i] <- n[i]
        }
    
    # K
    K <- n*(Delta-delta) / delta.index.max * haus.dist(L)[cbind(1:r,index.max)]
    
    # TCmax
    TCmx <- sum(K)
    
    # results
    results <- list(index = index.max, delta.index = delta.index.max, K = K, Amax = Amax, TCmx = TCmx)
    results
}


## STC.int: Interval scale Standard Total misclassification Cost
# Given a matrix confusion C, the function returns the Standard Total misclassification Cost of C
STC.int <- function(C, L) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all elements are integer numbers)" = all(C == floor(C)))
    stopifnot("The number of classes and the number of intervals do not match" = nrow(C) == length(L))
    stopifnot("The argument L is not a lengths vector (not all its elements are positive)" = all(L > 0))
    
    # STC
    STC <- TC.int(C, L)/TCmax.int(C, L)$TCmx
    STC
}
