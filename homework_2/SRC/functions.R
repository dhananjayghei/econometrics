# Loading the required libraries
library(MASS)
# Function for generating 'N' samples of 'n' observations each
# muX - True mean of X
# vX - True variance of X
# muE - True mean of epsilon
# vE - True variance of epsilon
# r - Correlation between X and epsilon
# n - Number of observations
# N - Number of samples
draw <- function(muX, vX, muE, vE, r, n, N){
    set.seed(123)
    dat <- lapply(1:N, function(y){
        # Generating 2 random variables X and epsilon
        k <- mvrnorm(n=n, mu=c(muX, muE),
                     Sigma=matrix(c(vX, r, r, vE), nrow=2), empirical=FALSE)
        k <- data.frame(k)
        colnames(k) <- c("x", "eps")
        return(k)
    })
    return(dat)
}

# Function for designing a data frame for regression with intercept
# data - The output from the function 'draw' (data.frame)
# b0 - True value of \beta_0
# b1 - True value of \beta_1
design <- function(data, b0, b1){
    # Generating a column for intercecpt
    data[, "const"] <- rep(1, nrow(data))
    # Creating the true beta matrix
    betaT <- as.matrix(x=c(b0, b1), nrow=2, ncol=1)
    # Generating y = b0 + b1*x + epsilon
    data[, "y"] <- as.numeric(as.matrix(data[, c("const", "x")]) %*% betaT) + data[, "eps"]
    # Re-arranging the columns for easy readability
    data <- data[, c("y", "const", "x", "eps")]
    return(data)
}

# Wrapper function to implement the exercise
# muX - True mean of X
# vX - True variance of X
# muE - True mean of epsilon
# vE - True variance of epsilon
# r - Correlation between X and epsilon
# n - Number of observations
# N - Number of samples
# b0 - True value of \beta_0
# b1 - True value of \beta_1
simulate.OLS <- function(muX, vX, muE, vE, r, n, N, b0, b1){
    dat <- draw(muX=muX, vX=vX, muE=muE, vE=vE, r=r, n=n, N=N)
    dat <- lapply(dat, function(x) design(data=x, b0=b0, b1=b1))
    # Running OLS
    OLS <- lapply(dat, function(z){
        reg <- lm(y~x, data=z)
    })
    # Storing the f-stat
    Fstat <- lapply(OLS, function(x){
        k <- summary(x)
        Fstat <- (k$coefficients[2, 1]-b1)^2/k$coefficients[2, 2]
        return(Fstat)
        })
    FstatMod <- do.call(rbind, lapply(OLS, function(x) summary(x)$fstatistic))
    Dist <- data.frame(do.call(rbind, Fstat))
    colnames(Dist)[1] <- "Fstat"
    Dist$tstat <- sqrt(Dist$Fstat)
#    Dist <- Dist[, c(1, 4)]
    # Storing the estimated betas
    bhat <- data.frame(do.call(rbind, lapply(OLS, coef)))
    colnames(bhat) <- c("b0hat", "b1hat")
    rownames(bhat) <- NULL
    # Finding the x'x inverse
    xxInv <- lapply(dat, function(z){
            mat <- as.matrix(z[, c("const", "x")])
            Inv <- ginv(crossprod(mat))
            return(Inv)
    })
    # Finding the mean of inverse matrices
    xxInv.avg <- Reduce('+', xxInv)/n
    return(list(bEst=bhat, xxInv=xxInv.avg, Dist=Dist, FstatMod=FstatMod))
}
