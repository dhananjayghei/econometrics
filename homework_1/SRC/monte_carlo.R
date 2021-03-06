# Sourcing the functions file 
source("functions.R")

# Loading the required libraries
library(ggplot2)
library(knitr)
# -------------- Question 1
baseline <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=100,
                         N=100, b0=9, b1=2)
# Storing the betas
bhat.Baseline <- baseline[[1]]

# Theoretical distribution of beta
x <- seq(7.5, 11, length=100)
b0True <- cbind(x, dnorm(x=x, mean=9, sd=sqrt(baseline[[2]][1, 1])))

# Density plots
# Intercept (\hat \beta_0)
plot(density(bhat.Baseline$b0hat),
     col="midnightblue", xlab="", ylab="Density", lwd=2, 
     main=expression(paste("Distribution of ", beta[0], sep="  ")))
lines(x=b0True[, 1], y=b0True[, 2], lwd=2, col="firebrick")
legend("topright", col=c("midnightblue", "firebrick"),
       lwd=c(2,2), bty="n", legend=c("Monte-carlo distribution",
                                     "True distribution"))

## ggplot(bhat.Baseline, aes(x=b0hat)) +
##     geom_density(colour="midnightblue", fill="midnightblue", alpha=0.3) +
##     theme_bw() + labs(x="", y="Density") +
##     theme(legend.background=element_rect())

## ggplot() + 
##     geom_histogram(breaks=seq(7,11,length=100), 
##                     colour="black", 
##                     fill="midnightblue") +
##     stat_function(fun=dnorm, args=list(mean=9, sd=sqrt(baseline[[2]][1, 1])))


# Slope (\hat \beta_1)
z <- seq(1.85, 2.15, length=100)
b1True <- cbind(z, dnorm(x=z, mean=2, sd=sqrt(baseline[[2]][2, 2])))

# Density plots
# Intercept (\hat \beta_0)
plot(density(bhat.Baseline$b1hat),
     col="midnightblue", xlab="", ylab="Density", lwd=2, 
     main=expression(paste("Distribution of ", beta[1], sep="  ")))
lines(x=b1True[, 1], y=b1True[, 2], lwd=2, col="firebrick")
legend("topright", col=c("midnightblue", "firebrick"),
       lwd=c(2,2), bty="n", legend=c("Monte-carlo distribution",
                                     "True distribution"))

## ggplot(bhat.Baseline, aes(x=b1hat)) +
##     geom_density(colour="midnightblue", fill="midnightblue", alpha=0.3) +
##     theme_bw() + labs(x="", y="Density") +
##     theme(legend.background=element_rect())

## plot(density(rnorm(1000, 9, sd=sqrt(baseline[[2]][1, 1]))))

# TODO: Add the theoretical distribution of beta hat for comparison

# -------------------- Question 2
# Varying the sample size n
# n = 10
smallSample <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=10, N=100, b0=9, b1=2)
largeSample <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=1000, N=100, b0=9, b1=2)

# Storing the estimated betas
bhat.n <- data.frame(do.call(rbind, list(smallSample[[1]],
                                         baseline[[1]], largeSample[[1]])))
bhat.n$type <- c(rep("Small Sample", 100),
                 rep("Baseline Sample", 100),
                 rep("Large Sample", 100))

# Storing the results
library(dplyr)

bMean.n <- bhat.n %>%
    group_by(type) %>%
    summarise(b0=round(mean(b0hat), 3),
              b1=round(mean(b1hat), 3)) %>%
    ungroup() %>% data.frame()


# Plotting
# Slope
ggplot(bhat.n, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[0], sep="   "))) +
    theme(legend.background=element_rect(),
          legend.position=c(.8,.85),
          plot.title=element_text(hjust=.5))  +
    scale_fill_manual(values=c("firebrick", "midnightblue", "forestgreen"),
                      name = "",
                      breaks=c("Small Sample",
                               "Baseline Sample", "Large Sample"),
                      labels=c("Sample size - 10", "Sample size - 100",
                               "Sample size - 1000"))

# Intercept
ggplot(bhat.n, aes(x=b1hat, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[1], sep="  "))) +
    theme(legend.background=element_rect(),
          legend.position=c(.8,.85),
          plot.title=element_text(hjust=.5))  +
    scale_fill_manual(values=c("firebrick", "midnightblue", "forestgreen"),
                      name = "",
                      breaks=c("Small Sample",
                               "Baseline Sample", "Large Sample"),
                      labels=c("Sample size - 10", "Sample size - 100",
                               "Sample size - 1000"))

# -------------------- Question 3
# Varying the number of samples N
# N = 10
smallN <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=100,
                       N=10, b0=9, b1=2)
# N = 10000
largeN <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=100,
                       N=10000, b0=9, b1=2)

# Comparing with the baseline model (Stacking them up)
bhat.N <- data.frame(do.call(rbind, list(smallN[[1]],
                                         baseline[[1]], largeN[[1]])))
bhat.N$type <- c(rep("Small Sample", 10), rep("Baseline", 100), rep("Large Sample", 10000))

bMean.N <- bhat.N %>%
    group_by(type) %>%
    summarise(b0=round(mean(b0hat), 3),
              b1=round(mean(b1hat), 3)) %>%
    ungroup() %>% data.frame()

# Plotting
## Slope
ggplot(bhat.N, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[0]))) +
    theme(legend.background=element_rect(),
          legend.position=c(.77,.85),
          plot.title=element_text(hjust=.5))  +
    scale_fill_manual(values=c("firebrick", "midnightblue", "forestgreen"),
                      name = "",
                      breaks=c("Small Sample", "Baseline", "Large Sample"),
                      labels=c("No. of samples - 10",
                               "No. of samples - 100 (Baseline Model)",
                               "No. of samples - 10000"))
## Intercept
ggplot(bhat.N, aes(x=b1hat, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[1], sep="   "))) +
    theme(legend.background=element_rect(),
          legend.position=c(.77,.85),
          plot.title=element_text(hjust=.5))  +
    scale_fill_manual(values=c("firebrick", "midnightblue", "forestgreen"),
                      name = "",
                      breaks=c("Small Sample", "Baseline", "Large Sample"),
                      labels=c("No. of samples - 10",
                               "No. of samples - 100 (Baseline Model)",
                               "No. of samples - 10000"))

# -------------------- Question 4
# Varying the distribution of X
lowXvar <- simulate.OLS(muX=10, vX=2, muE=0, vE=1, r=0, n=100,
                        N=100, b0=9, b1=2)
highXvar <- simulate.OLS(muX=10, vX=16, muE=0, vE=1, r=0, n=100,
                        N=100, b0=9, b1=2)

# Comparing with the baseline model (Stacking them up)
bhat.Xvar <- data.frame(do.call(rbind, list(lowXvar[[1]],
                                       baseline[[1]], highXvar[[1]])))
bhat.Xvar$type <- c(rep("Low", 100), rep("Baseline", 100),
               rep("High", 100))

bMean.Xvar <- bhat.Xvar %>%
    group_by(type) %>%
    summarise(b0=round(mean(b0hat), 3),
              b1=round(mean(b1hat), 3)) %>%
    ungroup() %>% data.frame()

# Plotting
## Slope
ggplot(bhat.Xvar, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[0]))) +
    theme(legend.background=element_rect(),
          legend.position=c(.9,.9),
          plot.title=element_text(hjust=.5)) +
    scale_fill_manual(values=c("firebrick", "midnightblue", "forestgreen"),
                      name="",
                      breaks=c("Low", "Baseline", "High"),
                      labels=c(expression(paste(sigma[x]^2,"=", 2)),
                               expression(paste(sigma[x]^2,"=",4)),
                               expression(paste(sigma[x]^2,"=", 16))))

## Intercept
ggplot(bhat.Xvar, aes(x=b1hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[1]))) +
    theme(legend.background=element_rect(),
          legend.position=c(.9,.9),
          plot.title=element_text(hjust=.5)) +
    scale_fill_manual(values=c("firebrick", "midnightblue", "forestgreen"),
                      name="",
                      breaks=c("Low", "Baseline", "High"),
                      labels=c(expression(paste(sigma[x]^2,"=", 2)),
                               expression(paste(sigma[x]^2,"=",4)),
                               expression(paste(sigma[x]^2,"=", 16))))


# -------------------- Question 5
# Varying the mean of \epsilon
epsDist <- simulate.OLS(muX=10, vX=4, muE=2, vE=1, r=0, n=100,
                        N=100, b0=9, b1=2)
bhat.eps <- epsDist[[1]]

# Comparing with the baseline model (Stacking them up)
bhat.eps <- rbind(bhat.eps, bhat.Baseline)
bhat.eps$type <- c(rep("Mean", 100), rep("No mean", 100))

bMean.eps <- bhat.eps %>%
    group_by(type) %>%
    summarise(b0=round(mean(b0hat), 3),
              b1=round(mean(b1hat), 3)) %>%
    ungroup() %>% data.frame()

# Change in the intercept value. The mean will shift from 9 to 11
ggplot(bhat.eps, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[0]))) +
    theme(legend.background=element_rect(),
          legend.position=c(.9,.9),
          plot.title=element_text(hjust=.5)) +
    scale_fill_manual(values=c("firebrick", "midnightblue"),
                      name="",
                      breaks=c("Mean", "No mean"),
                      labels=c(expression(paste(mu[epsilon],"=", 2)),
                               expression(paste(mu[epsilon],"=",0))))

# No change in the distribution of \hat \beta_1
ggplot(bhat.eps, aes(x=b1hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[1]))) +
    theme(legend.background=element_rect(),
          legend.position=c(.9,.9),
          plot.title=element_text(hjust=.5)) +
    scale_fill_manual(values=c("firebrick", "midnightblue"),
                      name="",
                      breaks=c("Mean", "No mean"),
                      labels=c(expression(paste(mu[epsilon],"=", 2)),
                               expression(paste(mu[epsilon],"=",0))))


# -------------------- Question 6
# Varying the correlation between \epsilon and X
# Positive correlation
baselinePCor <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0.8, n=100,
                             N=100, b0=9, b1=2)
# Negative correlation
baselineNCor <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=-0.8, n=100,
                            N=100, b0=9, b1=2)

# Storing the betas
bhat.pcor <- baselinePCor[[1]]
bhat.ncor <- baselineNCor[[1]]

# Comparing with the baseline model (Stacking them up)
bhat.cor <- rbind(bhat.pcor, bhat.Baseline, bhat.ncor)
bhat.cor$type <- c(rep("Correlated", 100),
                   rep("Baseline", 100), rep("Uncorrelated", 100))

bMean.cor <- bhat.cor %>%
    group_by(type) %>%
    summarise(b0=round(mean(b0hat), 3),
              b1=round(mean(b1hat), 3)) %>%
    ungroup() %>% data.frame()

# Plotting
## Slope
ggplot(bhat.cor, aes(x=b0hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    theme(legend.background=element_rect(),
          legend.position=c(.9,.85),
          plot.title=element_text(hjust=.5)) +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[0]))) +
    scale_fill_manual(values=c("firebrick", "midnightblue", "forestgreen"),
                      name="",
                      breaks=c("Correlated", "Baseline", "Uncorrelated"),
                      labels=c(expression(paste(r[x][epsilon],"=",0.8)),
                               expression(paste(r[x][epsilon],"=",0)),
                               expression(paste(r[x][epsilon],"=",-0.8))))

## Intercept
ggplot(bhat.cor, aes(x=b1hat, fill=type)) +
    geom_density(alpha=0.3) +
    theme_bw() +
    theme(legend.background=element_rect(),
          legend.position=c(.9,.85),
          plot.title=element_text(hjust=.5)) +
    labs(x="", y="Density", title=expression(paste("Distribution of ", beta[1]))) +
    scale_fill_manual(values=c("firebrick", "midnightblue", "forestgreen"),
                      name="",
                      breaks=c("Correlated", "Baseline", "Uncorrelated"),
                      labels=c(expression(paste(r[x][epsilon],"=",0.8)),
                               expression(paste(r[x][epsilon],"=",0)),
                               expression(paste(r[x][epsilon],"=",-0.8))))
