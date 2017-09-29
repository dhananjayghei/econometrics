# Sourcing the functions file
source("functions.R")

# Loading the required libraries
library(ggplot2)
library(dplyr)

# Question 1
# Finding \tilde{F} 
baseline <- simulate.OLS(muX=10, vX=.04, muE=0, vE=1, r=0,
                         n=1000, N=1000, b0=9, b1=2)

# Histogram of F distribution from the regression
# under the null that beta1=2
hist(baseline[[3]][, 1], freq=FALSE, ylim=c(0, 4),
     ylab="Density", xlab="", main=expression(paste("F-Distribution under the null ", beta[1], "=2", sep="")), col="midnightblue", density=50)
# Theoretical F distribution
x <- rf(n=100000, df1=1, df2=998)
curve(df(x=x, df1=1, df2=998), lwd=2, type="l", col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1))

# Histogram of F distribution from the regression
# under the null that beta1=0
hist(baseline[[4]][, 1], freq=FALSE, xlim=c(0, 250),
     ylab="Density", xlab="", main=expression(paste("F-Distribution under the null ", beta[1], "=0", sep="")), col="midnightblue", density=50)
# Theoretical F distribution
x <- rf(n=100000, df1=1, df2=998)
curve(df(x=x, df1=1, df2=998), lwd=2, type="l", col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1), cex=.8)

# Histogram of t-distribution from the regression
# under the null that beta1=2
hist(c(baseline[[3]][, 2], -baseline[[3]][, 2]), freq=FALSE, ylim=c(0, 1), xlim=c(-4, 4),
     ylab="Density", xlab="",
     main=expression(paste("t-Distribution under the null ",
                           beta[1], "=2", sep="")),
     col="midnightblue", density=50)
# Theoretical t distribution
x <- rt(n=1000, df=998)
curve(dt(x=x, df=998), type="l", lwd=2, col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1))

# Question 2 
# Changing the parameter values
param <- simulate.OLS(muX=10, vX=.04, muE=0, vE=1, r=0,
                      n=1000, N=1000, b0=9, b1=4)


hist(param[[3]][, 1], freq=FALSE, ylim=c(0, 3.5),
     ylab="Density", xlab="", main=expression(paste("F-Distribution under the null ", beta[1], "=4", sep="")), col="midnightblue", density=50)
# Theoretical F distribution
x <- rf(n=100000, df1=1, df2=998)
curve(df(x=x, df1=1, df2=998), lwd=2, type="l", col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1))

# Histogram of F distribution from the regression
# under the null that beta1=0
hist(param[[4]][, 1], freq=FALSE, xlim=c(0, 900),
     ylab="Density", xlab="", main=expression(paste("F-Distribution under the null ", beta[1], "=0", sep="")), col="midnightblue", density=50)
# Theoretical F distribution
x <- rf(n=100000, df1=1, df2=998)
curve(df(x=x, df1=1, df2=998), lwd=2, type="l", col="firebrick", add=TRUE)
legend("topleft", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1), cex=.8)

# Theoretical t distribution
hist(c(param[[3]][, 2], -param[[3]][, 2]), freq=FALSE,
     ylim=c(0, 1), xlim=c(-4, 4),
     ylab="Density", xlab="",
     main=expression(paste("t-Distribution under the null ",
                           beta[1], "=4", sep="")),
     col="midnightblue", density=50)
x <- rt(n=1000, df=998)
curve(dt(x=x, df=998), type="l", lwd=2, col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1))

# Question 3
# Changing the sample size
sampleSize <- simulate.OLS(muX=10, vX=.04, muE=0, vE=1, r=0,
                      n=10, N=1000, b0=9, b1=2)

hist(sampleSize[[3]][, 1], freq=FALSE, ylim=c(0, .4),
     ylab="Density", xlab="", main=expression(paste("F-Distribution under the null ", beta[1], "=2", sep="")), col="midnightblue", density=50)
# Theoretical F distribution
x <- rf(n=100000, df1=1, df2=998)
curve(df(x=x, df1=1, df2=998), lwd=2, type="l", col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1))

# Theoretical t distribution
hist(c(sampleSize[[3]][, 2], -sampleSize[[3]][, 2]), freq=FALSE, ylim=c(0, .4), xlim=c(-4,4), ylab="Density", xlab="",
     main=expression(paste("t-Distribution under the null ",
                           beta[1], "=2", sep="")),
     col="midnightblue", density=50)
x <- rt(n=1000, df=998)
curve(dt(x=x, df=998), type="l", lwd=2, col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1))

# Question 4
# Changing to MV Normal for X
set.seed(123)
dat <- lapply(1:1000, function(y){
    k <- mvrnorm(n=1000, mu=c(5,6,0),
                 Sigma=matrix(c(.04,.001,0,.001,.04,0,0,0,1), nrow=3),
                 empirical=FALSE)
    k <- data.frame(k)
    colnames(k) <- c("x1", "x2", "eps")
    return(k)
})

dat <- lapply(dat, function(y){
    # Generating a column for intercecpt
    y[, "const"] <- rep(1, nrow(y))
    # Creating the true beta matrix
    betaT <- as.matrix(x=c(9, 2, 5), nrow=2, ncol=1)
    # Generating y = b0 + b1*x + epsilon
    y[, "y"] <- as.numeric(as.matrix(y[, c("const", "x1", "x2")]) %*% betaT) + y[, "eps"]
    # Re-arranging the columns for easy readability
    y <- y[, c("y", "const", "x1", "x2", "eps")]
    return(y)
})

# Running OLS
OLS <- lapply(dat, function(z){
    reg <- lm(y~x1+x2, data=z)
    regC <- lm(y~1, data=z)
    return(list(Full=reg, Int=regC))
})

# Getting the F Test
Fstat <- lapply(OLS, function(x){
    F <- anova(x[[2]], x[[1]])$F[2]
    return(F)
})
Fstat <- data.frame(do.call(rbind, Fstat))
colnames(Fstat)[1] <- "Fstat"
Fstat$tstat <- sqrt(Fstat[, 1])

# Histogram of F distribution from the regression
# under the null that beta1, beta2=0
hist(Fstat[, 1], freq=FALSE, xlim=c(0, 800), ylim=c(0, 0.008), 
     ylab="Density", xlab="", main=expression(paste("F-Distribution under the null ", beta[1], ",", beta[2], "=0", sep="")), col="midnightblue", density=50)
# Theoretical F distribution
x <- rf(n=100000, df1=2, df2=997)
curve(df(x=x, df1=2, df2=997), lwd=2, type="l", col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1), cex=.8)


# Theoretical t distribution
hist(c(Fstat[, 2], -Fstat[, 2]), freq=FALSE, ylim=c(0, .5), xlim=c(-30, 30),
     ylab="Density", xlab="",
     main=expression(paste("t-Distribution under the null ",
                           beta[1], ",", beta[2], "=0", sep="")),
     col="midnightblue", density=50)
x <- rt(n=1000, df=997)
curve(dt(x=x, df=997), type="l", lwd=2, col="firebrick", add=TRUE)
legend("topright", col=c("midnightblue", "firebrick"),
       legend=c("Monte-carlo distribution", "True distribution"),
       bty="n", lty=c(1,1))
