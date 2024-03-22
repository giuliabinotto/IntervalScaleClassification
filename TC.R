##########################################################
################# Ordinal classification #################
###################### STC Measure #######################
##########################################################

### Arguments and notation
# C             = (matrix) Confusion matrix obtained by a validation procedure
# r             = (number) Number of classes
# n             = (vector) Number of instances for each class
# N             = (number) Total number of instances
# distance      = (function) Distance between two classes given in matrix form
# gamma.ij      = (matrix) Matrix used for the calculation of the cost matrix 
# m             = (matrix) Cost matrix
# index.max     = (vector) Vector composed by i_j for j=1,...,r
# n.index.max   = (vector) Number of instances of the class i_j for j=1,...,r
# Amax          = (matrix) Matrix representing the class Smax 
# K             = (vector) Vector used for the calculation of TCmax


### Complementary functions
# Distance between two classes: |i-j|
distance <- function(C) {
    r <- nrow(C)
    d <- as.data.frame(which(C !='NA', arr.ind = T))
    d <- abs(d$row-d$col)
    d <- matrix(d, nrow = r)
    d
}


### Functions

## TC: Total misclassification Cost
# Given a confusion matrix C, the function returns the Total misclassification Cost of C
TC <- function(C) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    
    # Variables
    r <- nrow(C)
    n <- colSums(C)
    N <- sum(n)
    
    # gamma.ij
    gamma.ij <- matrix(rep(N-n, r), nrow = r, byrow=TRUE)/n
    
    # Cost matrix m.ij
    m <- gamma.ij*distance(C)
    
    # TC
    TC <- sum(C*m)
    TC
}


## TCmax = TC(Amax) Maximum Total misclassification Cost
# Given a confusion matrix C, the function returns a list including:
# index     = Vector of indexes i_j for j=1,...,r
# n.index   = Vector of instances of the class i_j for j=1,...,r
# K         = Vector of K_j for j=1,...,r
# Amax      = Matrix Amax
# TCmx      = Maximum Total misclassification Cost
TCmax <- function(C) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    
    # Variables
    r <-  nrow(C)
    n <- colSums(C)
    N <- sum(n)
    
    # i.j
    index.max <- distance(C)/n
    index.max <- apply(index.max, 2, which.max)
    
    # n.i.j
    n.index.max <- n[index.max]
    
    # Amax
    Amax <- matrix(0, r, r)
    for (i in 1:r) {
        Amax[index.max[i],i] <- n[i]
        }
    
    # K
    K <- n*(N-n)/n.index.max*abs(1:r-index.max)
    
    # TCmax
    TCmx <- sum(K)
    
    # results
    results <- list(index = index.max, n.index = n.index.max, K = K, Amax = Amax, TCmx = TCmx)
    results
}


# STC: Standard Total misclassification Cost
# Given a confusion matrix C, the function returns the Standard Total misclassification Cost of C
STC <- function(C) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    
    # STC
    STC <- TC(C)/TCmax(C)$TCmx
    STC
}
