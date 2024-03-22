#########################################################
####### Ordinal and interval scale classification #######
################### Training examples ###################
##########################################################


### Libraries
library(ggplot2)
library(latex2exp)

### Sources
source("BD_OC_MAE.R")
source("BD_OC_MAEintervals.R")
source("BD_OC_MAEunbounded.R")
source("BD_OC_TC.R")
source("BD_OC_TCintervals.R")
source("BD_OC_TCunbounded.R")


### Matrices
# Matrices studied in George et al. (2016)
M <- matrix(c(444,144,2,10,37,25,2,2,27,18,8,14,57,0,7,3), nrow = 4)
M <- matrix(c(553,18,15,14,8,1,37,20,18,43,4,2,2,53,11,2), nrow = 4)
M <- matrix(c(539,19,42,0,53,3,2,8,33,24,3,7,52,0,0,15), nrow = 4)
M <- matrix(c(589,0,4,7,4,4,14,44,3,25,6,33,1,17,8,41), nrow = 4)

# Matrices and intervals' length vector generator (r = number of classes, n/nmax = maximum integer value for the matrix, size = sample size, Lmn = maximum length for intervals, Lmx = maximum length for intervals)
r <- 6
nmax <- 100
M <- matrix(sample.int(n = nmax, size = r^2, replace = TRUE), nrow = r)
Lmn <- 0.1
Lmx <- 30
L <- round(runif(r, min = Lmn, max = Lmx), 1)     # Length r (general case)
L <- round(runif(r-1, min = Lmn, max = Lmx), 1)     # Length r-1 (rightmost unbounded case)


### Examples for MAE
MAE(M)
MAEmax(M)
SMAE(M)


### Examples for MEAintervals
haus.dist(L)

MAE.int(M, L)
MAEmax.int(M, L)
SMAE.int(M, L)


### Examples for MAEintervals with unbounded rightmost class
# OBS: L has to have length r-1.

## MAEmax
Lsum <- sum(L)
MAEmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)
MAEmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)$minMAEmax
MAEmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)$plotMAEmax

# Plot of MAEmax(x)
x <- MAEmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)$plotMAEmax$rightint
y <- MAEmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)$plotMAEmax$MAEmax.rightint
plot(x, y, type = 'l')

## SMAE
SMAE.unbounded(M, L, Lmin = 0.1, Lmax = 6, eps = 0.1)


### Examples for TC
# M could be one of George et al. (2016)'s matrices
TC(M)
TCmax(M)
STC(M)


### Examples for TCintervals
dens(M, L)
haus.dist(L)

TC.int(M, L)
TCmax.int(M, L)
STC.int(M, L)


### Examples for TCintervals with unbounded rightmost class
# OBS: L has to have length r-1.

## TCmax
Lsum <- sum(L)
TCmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)
TCmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)$minTCmax
TCmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)$plotTCmax

# Plot of TCmax(x)
x <- TCmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)$plotTCmax$rightint
y <- TCmax.unbounded(M, L, Lmin = 0.1, Lmax = Lsum, eps = 0.1)$plotTCmax$TCmax.rightint
plot(x, y, type = 'l')

## STC
STC.unbounded(M, L, Lmin = 0.1, Lmax = 6, eps = 0.1)


### Examples for the theoretical proposition (Particular case: r=3, n1=n2=n3, l1=1)
# Comparison with an example matrix
M <- matrix(c(10,0,2, 3,4,5, 6,1,5), nrow = 3)
apply(M, 2, sum)

n <- sum(M[,1])
l2 <- 0.8
l2 <- 1.5
l2 <- 2.13
l2 <- 5

prop.min(l2 = l2, n = n)
# Check that the result is the same with TCmax.unbounded()
L <- c(1, l2)
TCmax.unbounded(M, L, Lmin = 0.1, Lmax = 600, eps = 1)$minTCmax

# Plot
prop.min2 <- function(l2) {prop.min(l2 = l2, n = n)}
x <- seq(0.01, 3, by = 0.01)
xmin <- as.numeric(sapply(x, prop.min2)[1,])
TCmin <- as.numeric(sapply(x, prop.min2)[2,])
plot(x,xmin, type = 'l')
plot(x,TCmin, type = 'l')

df <- data.frame(x, xmin)
ggplot(df, aes(x = x, y = xmin)) + 
#    geom_line(color = "darkgreen", size = 1) +
    geom_line(color = "#335c67", size = 1.5) +
    labs(x = "L", y = TeX("$x_{min}$")) +
    theme_grey(base_size = 25)

df <- data.frame(x, TCmin)
ggplot(df, aes(x = x, y = TCmin)) + 
#    geom_line(color = "darkgreen", size = 1) +
    geom_line(color = "#335c67", size = 1.5) +
    labs(x = "L", y = TeX("$TC_{max}^{int}(x_{min})$")) +
    theme_grey(base_size = 25)
