#########################################################
####### Ordinal and interval scale classification #######
################## Simulation function ##################
#########################################################
############### G. Binotto and R. Delgado ###############
#########################################################


### Sources
source("TC.R")
source("TCintervals.R")
source("TCunbounded.R")


### Functions to determine the optimum length of the last interval for the TC metric

## Simulation given the number of instances in each class (n.col) and the length of the intervals except the last one (L).
# OBS: Lmax has to be set previously.
simulation <- function(n.col, L) {
    # r
    r <- length(n.col)
    
    # Matrix
    M <- matrix(c(n.col, rep(0, times = r*(r-1))), nrow= r, byrow = TRUE)
    
    # Estimation of xmin and min.TCmax
    simul <- TCmax.unbounded(M, L, Lmin = 0.01, Lmax = Lmax, eps = 0.01)
    
    # Output
    result <- list(n.col = n.col, M = M, L = L, xmin = simul$minTCmax$min.rightint, min.TCmax = simul$minTCmax$min.TCmax,
                   rightint.TCmax = simul$plotTCmax$TCmax.rightint)
    result
}


## Simulation for r=3 with fix number of instances in each class (n.col). l1 is fixed to 1. l2 varies within a range of values given by the variable l2.
# OBS: Lmax has to be set previously.
simulation.L <- function(n.col, l2) {
    # Define the vector L (without l2)
    L <- c(1, NA)
    
    # Use Map to apply simulation() to each value of l2
    minimum <- Map(function(x) simulation(n.col = n.col, L = ifelse(is.na(L), x, L))[4:5], l2)
    # Convert the list elements to vectors
    minimum <- lapply(minimum, unlist)
    # Convert the result list into a dataframe and add the column l2
    result <- as.data.frame(do.call(rbind, minimum))
    result$l2 <- l2
    # Rearrange columns to place l2 first
    result <- result[, c(3, 1, 2)]
    
    # Print the dataframe
    result
}


## Simulation for r=4 with fix number of instances in each class (n.col). l1 is fixed to 1. l2 and l3 vary within a range of values given by the variables l2 and l3.
# OBS: Lmax has to be set previously.
simulation.r4.L <- function(n.col, l2, l3) {
    # Create all combinations of l2 and l3
    combinations <- expand.grid(l2 = l2, l3 = l3)
    
    # Use Map to apply simulation() to each combination of l2 and l3
    results <- Map(function(x, y) {
        L <- c(1, x, y)  # Define the vector L with 1, x, and y
        simulation(n.col = n.col, L = L)[4:5]
    }, combinations$l2, combinations$l3)
    
    # Convert the list elements to vectors
    results <- lapply(results, unlist)
    
    # Convert the result list into a dataframe and add the columns l2 and l3
    result <- as.data.frame(do.call(rbind, results))
    result$l2 <- combinations$l2
    result$l3 <- combinations$l3
    
    # Rearrange columns to place l2 and l3 first
    result <- result[, c(3, 4, 1, 2)]
    # Order by l2 and then l3
    result <- result[order(result$l2, result$l3), ]
    
    # Print the dataframe
    result
}
