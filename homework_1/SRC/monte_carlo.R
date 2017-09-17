source("functions.R")

# Loading the required libraries
library(ggplot2)
library(dplyr)

# Generating a random sample using 'draw' function
# The output is a list object
dat <- draw(muX=10, vX=4, muE=0, vE=1, r=0, n=100, N=100)

# Use the 'design' function to generate a data.frame for
# each'N' samples
dat <- lapply(dat, function(x) design(data=x, b0=9, b1=2))

# Running OLS on each of the 'N' samples and
# storing the results
dat.reg <- lapply(dat, function(z){
    reg <- lm(y~x, data=z)
    return(reg)
})

# Storing the estimated b0 and b1 from
# the regression results
bhat <- data.frame(do.call(rbind, lapply(dat.reg, coef)))
colnames(bhat) <- c("b0hat", "b1hat")

# Finding the theoretical distribution of b0 and b1
# Inverse of X'X
xxInv <- lapply(dat, function(z){
    mat <- as.matrix(z[, c("const", "x")])
    Inv <- ginv(crossprod(mat))
    return(Inv)
})

# Density plots
# Intercept (\hat \beta_0)
ggplot(bhat, aes(x=b0hat))+geom_density(colour="midnightblue",
                                        fill="midnightblue", alpha=0.3) +
    theme_bw()

# Slope (\hat \beta_1)
ggplot(bhat, aes(x=b1hat))+geom_density(colour="midnightblue",
                                        fill="midnightblue", alpha=0.3) +
    theme_bw() + labs(x="", y="Density") +
    theme(legend.background=element_rect())

dnorm(x=8, mean=9, sd=1)

