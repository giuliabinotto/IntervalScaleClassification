##########################################################
################# Ordinal classification #################
###################### MAE Measure #######################
##########################################################
################ G. Binotto and R. Delgado ###############
##########################################################

### Arguments and notation
# C             = (matrix) Confusion matrix obtained by a validation procedure
# r             = (number) Number of classes
# n             = (vector) Number of instances for each class
# N             = (number) Total number of instances
# distance      = (function) Distance between two classes given in matrix form
# m             = (matrix) Cost matrix
# index.max     = (vector) Vector composed by i_j for j=1,...,r
# Amax          = (matrix) Matrix representing the class Smax 


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

## MAE: Mean Absolute Error
# Given a confusion matrix C, the function returns the Mean Absolute Error of C
MAE <- function(C) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    
    # Variables
    r <- nrow(C)
    n <- apply(C, 2, sum)
    N <- sum(n)
    
    # Cost matrix m.ij
    m <- distance(C)/N
    
    # MAE
    MAE <- sum(C*m)
    MAE
}


## MAEmax = MAE(Amax) Maximum Mean Absolute Error
# Given a confusion matrix C, the function returns a list including:
# Amax      = Matrix Amax
# MAEmx     = Maximum Mean Absolute Error
MAEmax <- function(C) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    
    # Variables
    r <-  nrow(C)
    n <- apply(C, 2, sum)
    N <- sum(n)
    
    # i.j
    index.max <- apply(distance(C), 2, which.max)
    
    # Amax
    Amax <- matrix(0, r, r)
    for (i in 1:r) {
        Amax[index.max[i],i] <- n[i]
        }
    
    # MAEmax
    MAEmx <- sum(n*abs(index.max-1:r))/N
    
    # results
    results <- list(Amax = Amax, MAEmx = MAEmx)
    results
}


# MAE: Standard Mean Absolute Error
# Given a confusion matrix C, the function returns the Standard Mean Absolute Error of C
SMAE <- function(C) {
    # Check parameters
    stopifnot("The argument of the function should be a matrix" = class(C)[1] == 'matrix')
    stopifnot("The argument of the function should be a square matrix" = nrow(C) == ncol(C))
    stopifnot("The argument of the function is not a confusion matrix (not all its elements are integer numbers)" = all(C == floor(C)))
    
    # STC
    SMAE <- MAE(C)/MAEmax(C)$MAEmx
    SMAE
}


