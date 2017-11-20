library(plm)
library(MASS)

# Function to draw random variable for error components model
# n - sample size
# N - Number of samples
# t - time periods 
# r - correlation between x and alpha
draw <- function(n, N, t, r){
    set.seed(123)
    dat <- lapply(1:N, function(x){
        eps <- rnorm(n*t, mean=0, sd=1)
        x <- rnorm(n*t, mean=4, sd=9)
        nut <- rnorm(t, mean=0, sd=1)
        alphan <- rnorm(n, mean=0, sd=1)
        x.cor <- x*r+sqrt(1-r^2)*rep(alphan, times=rep(t, n))
        datM <- data.frame(x=x, x.cor=x.cor, nut=rep(nut, n),
                           alphan=rep(alphan, times=rep(t, n)), eps=eps)
        return(datM)
    })
    return(dat)
}

# Function to construct the data set for regression
# dat - data frame - the output from the draw object
# b0 - the true intercept 
# b1 - the true slope coefficient
design <- function(dat, b0, b1){
    dat$y <- t(rep(b0, nrow(dat)) + b1%*%dat$x.cor + rowSums(dat[, -c(1:2)]))
    dat$const <- rep(1, nrow(dat))
    dat <- dat[, c("y", "const", "x", "x.cor", "nut", "alphan", "eps")]
    return(dat)
}

# Function to run regressions
# dat - data frame - the output from the design object
# This function runs three different regressions (OLS, Random effects and fixed effects)
# In addition, it also does a Hausman test to check for RE vs FE in the model
# The Hausman test is a sanity check to identify the most efficient estimator
simulateReg <- function(dat){
    temp <- pdata.frame(dat, index=c("alphan", "nut"))
    # OLS regression
    lmReg <- lm(y~x.cor, data=dat)
    # Two way random effects (Efficient SE)
    pols <- plm(y~x.cor, data=temp, effect="twoways", model="random")
    # Fixed effects
    feReg <- plm(y~x.cor, data=temp, effect="individual", model="within")
    # (Sanity check) Hausman test for RE vs FE
    if(phtest(pols, feReg)$p.value>0.05){
        cat("RE estimator works \n")
    }else{
        cat("FE estimator works \n")
    }
    return(list(OLS=lmReg, RE=pols, FE=feReg))    
}

# Generating tables for output
# Reg - the output from simulateReg 
genOutput <- function(Reg){
    estimates <- lapply(Reg, function(x){
        se.OLS <- summary(x[[1]])$coefficients[2, 1:2]
        se.RE <- summary(x[[2]])$coefficients[2, 1:2]
        se.FE <- summary(x[[3]])$coefficients[, 1:2]
        return(list(se.OLS=se.OLS, se.RE=se.RE, se.FE=se.FE))
    })
    olsEst <- data.frame(do.call(rbind, lapply(estimates, function(x) x[[1]])))
    reEst <- data.frame(do.call(rbind, lapply(estimates, function(x) x[[2]])))
    feEst <- data.frame(FE=do.call(rbind, lapply(estimates, function(x) x[[3]])))
    return(list(OLS=olsEst, RE=reEst, FE=feEst))
}
