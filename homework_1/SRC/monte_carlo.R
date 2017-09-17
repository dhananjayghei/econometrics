source("functions.R")

# Loading the required libraries
library(ggplot2)
library(dplyr)

# -------------- Question 1
baseline <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=100,
                         N=100, b0=9, b1=2)
# Storing the betas
bhat.Baseline <- baseline[[1]]

# Density plots
# Intercept (\hat \beta_0)
ggplot(bhat.Baseline, aes(x=b0hat))+geom_density(colour="midnightblue",
                                        fill="midnightblue", alpha=0.3) +
    theme_bw() + labs(x="", y="Density")+
    theme(legend.background=element_rect())

# Slope (\hat \beta_1)
ggplot(bhat.Baseline, aes(x=b1hat))+geom_density(colour="midnightblue",
                                        fill="midnightblue", alpha=0.3) +
    theme_bw() + labs(x="", y="Density") +
    theme(legend.background=element_rect()) 
# TODO: Add the theoretical distribution of beta hat for comparison


# -------------------- Question 2
# Varying the sample size n
# n = 10
smallSample <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=10, N=100, b0=9, b1=2)
largeSample <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=1000, N=100, b0=9, b1=2)

# Storing the estimated betas
bhat.n <- data.frame(do.call(rbind, list(smallSample[[1]], largeSample[[1]])))
bhat.n$type <- c(rep("Small Sample", 100), rep("Large Sample", 100))

# Plotting
ggplot(bhat.n, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() +
    labs(x="", y="Density") +
    theme(legend.background=element_rect())  +
    scale_fill_manual(values=c("midnightblue", "darkorange4"),
                      name = "",
                      breaks=c("Small Sample", "Large Sample"),
                      labels=c("Sample size - 10", "Sample size - 1000"))

# -------------------- Question 3
# Varying the number of samples N
# N = 10
smallN <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=100,
                       N=10, b0=9, b1=2)
# N =1000
largeN <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=100,
                       N=1000, b0=9, b1=2)

# Storing the estimated betas
bhat.N <- data.frame(do.call(rbind, list(smallN[[1]], largeN[[1]])))
bhat.N$type <- c(rep("Small Sample", 10), rep("Large Sample", 1000))

# Plotting
ggplot(bhat.N, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() +
    labs(x="", y="Density") +
    theme(legend.background=element_rect())  +
    scale_fill_manual(values=c("midnightblue", "darkorange4"),
                      name = "",
                      breaks=c("Small Sample", "Large Sample"),
                      labels=c("No. of samples - 10", "No. of samples - 1000"))

# -------------------- Question 4
# Varying the distribution of X


# -------------------- Question 5
# Varying the mean of \epsilon
epsDist <- simulate.OLS(muX=10, vX=4, muE=2, vE=1, r=0, n=100,
                        N=100, b0=9, b1=2)
bhat.eps <- epsDist[[1]]
bhat.eps <- rbind(bhat.eps, bhat.Baseline)
bhat.eps$type <- c(rep("Mean", 100), rep("No mean", 100))

library(latex2exp)

# Change in the intercept value. The mean will shift from 9 to 11
ggplot(bhat.eps, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    labs(x="", y="Density") +
    theme(legend.background=element_rect()) +
    scale_fill_manual(values=c("midnightblue", "darkorange4"),
                      name="",
                      breaks=c("Mean", "No mean"),
                      labels=c(TeX("$mu_{\\epsilon} = 2$"), TeX("$mu_{\\epsilon} = 0")))

# No change in the distribution of \hat \beta_1
ggplot(bhat.eps, aes(x=b1hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    labs(x="", y="Density") +
    theme(legend.background=element_rect()) +
    scale_fill_manual(values=c("midnightblue", "darkorange4"),
                      name="",
                      breaks=c("Mean", "No mean"),
                      labels=c(TeX("$mu_{\\epsilon} = 2$"), TeX("$mu_{\\epsilon} = 0")))
# TODO: Add LaTeX expressions in legends for ggplot2

# -------------------- Question 6
# Varying the correlation between \epsilon and X
# Positive correlation
baselineCor <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0.8, n=100,
                            N=100, b0=9, b1=2)
# TODO: Negative correlation

# Storing the betas
bhat.cor <- baselineCor[[1]]

# Comparing with the baseline model (Stacking them up)
bhat.cor <- rbind(bhat.cor, bhat.Baseline)
bhat.cor$type <- c(rep("Correlated", 100), rep("Uncorrelated", 100))

ggplot(bhat.cor, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    theme(legend.background=element_rect()) +
    labs(x="", y="Density") +
    scale_fill_manual(values=c("midnightblue", "darkorange4"),
                      name="",
                      breaks=c("Correlated", "Uncorrelated"),
                      labels=c("Correlated", "Uncorrelated"))

