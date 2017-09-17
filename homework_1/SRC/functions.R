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
