source("functions.R")
library(xtable)
# ----------------- Monte Carlo Q2
# Drawing random normal variables
dat <- draw(n=100, N=100, t=20, r=1)
# Constructing the datasets for regressions
dat <- lapply(dat, function(x) design(x, b0=1, b1=2))
# Running OLS, RE and FE regressions
reg <- lapply(dat, simulateReg)
# Generating output
result <- genOutput(reg)
# Storing the mean of the slope and standard errors
tab <- round(data.frame(do.call(rbind, lapply(result, colMeans))), 4)

# ------------------- Monte Carlo Q3
# Drawing random normal variables
dat2 <- draw(n=100, N=100, t=20, r=0.1) # Notice r=0.1 in this case
# Constructing the datasets for regressions
dat2 <- lapply(dat2, function(x) design(x, b0=1, b1=2))
# Running OLS, RE and FE regressions
reg2 <- lapply(dat2, simulateReg)
# Generating output
result2 <- genOutput(reg2)
# Storing the mean of the slope and standard errors
tab2 <- round(data.frame(do.call(rbind, lapply(result2, colMeans))), 4)


# Generating correct SE
correctSE <- lapply(reg, function(x){
    k <- resid(x[[1]])
    X <- cbind(rep(1, length(k)), x[[1]]$model$x.cor)
    correctSE <-  sqrt((drop(t(k)%*%k) * ginv(t(X)%*%X))[2,2]/1898)
    return(correctSE)
})
mean(do.call(rbind, correctSE))
