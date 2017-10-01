# Loading the required libraries
library(ggplot2)

# Setting the seed
set.seed(123)

# Drawing Xn from a random uniform distribution
X <- runif(n=100, min=1, max=10)

# Writing a function to generate Y conditional
# on X and given variance
simulate.OLS <- function(Xn, vY){
    # Generating reciprocal of X
    Xinv <- 1/Xn
    # Generating y_n
    Y <- rnorm(n=100, mean=10-X-(25/X), sd=sqrt(vY))
    reg <- lm(Y~Xn+Xinv)
    Coeff <- summary(reg)$coefficients[, 1]
    return(Coeff)
}

# Running the regression 1000 times keeping Xn constant
regRuns <- lapply(1:1000, function(...) simulate.OLS(Xn=X, vY=25))
# Storing the sample coefficients
sampleCoefs <- data.frame(do.call(rbind, regRuns))
colnames(sampleCoefs) <- c("b1", "b2", "b3")

# Storing the sample mean of coefficients
sampleMeans <- colMeans(sampleCoefs)

hist(sampleCoefs[, 1], freq=FALSE,
     density=50, col="midnightblue",
     xlab="",
     main=expression(paste("Distribution of ", beta[01])))
lines(density(sampleCoefs[, 1]), col="firebrick", lwd=2)

hist(sampleCoefs[, 2], freq=FALSE,
     density=50, col="midnightblue",
     xlab="",
     main=expression(paste("Distribution of ", beta[02])))
lines(density(sampleCoefs[, 2]), col="firebrick", lwd=2)

hist(sampleCoefs[, 3], freq=FALSE, ylim=c(0, 0.08),
     density=50, col="midnightblue",
     xlab="",
     main=expression(paste("Distribution of ", beta[03])))
lines(density(sampleCoefs[, 3]), col="firebrick", lwd=2)
