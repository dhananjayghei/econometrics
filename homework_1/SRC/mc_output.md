# Monte-Carlo Simulation
Dhananjay Ghei  
17th Sep 2017  


<style type="text/css">

body{ /* Normal  */
   font-size: 14px;
}

h1 { /* Header 1 */
 font-size: 22px;
 color: DarkBlue;
}
h2 { /* Header 2 */
   font-size: 19px;
   color: DarkBlue;
}
td{font-size: 11pt;
   padding:0px;
   cellpadding="0";
   cellspacing="0"
}
th {font-size: 11pt;
   height: 20px;
   font-weight: bold;
   text-align: center;
   background-color:
   #ccccff;
}
table{border-spacing:
	0px;
	border-collapse:
	collapse;
}

</style>
								
# Monte-Carlo Simulation

This article demonstrates a method to do Monte-Carlo simulations in R
for a two variable linear regression. The source code for this exercise
is available [online](https://www.github.com/dhananjayghei/econometrics) in the
Github repository.

We will operate with the following model:

   $y_i = \beta_0 + x_i \beta_1 + \epsilon_i$
   
where, $y_i$ and $x_i$ are random variables. We observe $N$ samples
   of $i=1,2, \dots, n$ obersvations of $y$ and $x$.

We will estimate
   
   $y_i = \hat \beta_0 + x_i \hat \beta_1 + e_i$




# Question 1
First assume that the $\epsilon$ and the regressors are
uncorrelated. Assume that both $X$ and $\epsilon$ are drawn from a
normal distribution $\epsilon \sim N(0, \sigma_{\epsilon}^2), X \sim
N(\mu_x, \sigma_x^2)$. Start with a sample size ($n$) of
$n=100$ and define the number of samples to be simulated ($N$) to be
$N=100$ as well. You also need to define the true values of $\beta_0$
and $\beta_1$. Report the results that you fnid for the means and
variance-covariance matrix of $\hat \beta_0$ and $\hat
\beta_1$. Finally, plot the distribution of the estimated parameters
and the theoretical normal distribution that they are supposed to
follow. 

## Generating data

### Random samples
I write a function that draws $N$ random samples of size $n$ for the
variables $\epsilon$ and $x_i$. This function takes as arguments the
following:

1. ```muX``` - True mean of X
2. ```vX``` - True variance of X
3. ```muE``` - True mean of $\epsilon$
4. ```vE``` - True variance of $\epsilon$
5. ```r``` - Correlation between $\epsilon$ and $X$
6. ```n``` - Sample size
7. ```N``` - Number of samples



```r
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
```
### Generating $y$ from given values of $\beta_0$ and $\beta_1$

Next, I define a function that designs a matrix which generates $y$
for given $X$ and $\epsilon$. It also adds a column of 1s to make
matrix multiplication easier. This function takes the following
arguments:

1. ```data``` - A sample of $X$ and $\epsilon$
2. ```b0``` - True value of $\beta_0$
3. ```b1``` - True value of $\beta_1$


```r
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
```
The next step is to build a wrapper function that draws random
samples, designs a matrix for OLS, runs OLS on each sample and returns
output as the predicted values of coefficients ($\hat \beta_0$, $\hat
\beta_1$) and the variance-covariance matrix of $\hat \beta$


```r
simulate.OLS <- function(muX, vX, muE, vE, r, n, N, b0, b1){
    dat <- draw(muX=muX, vX=vX, muE=muE, vE=vE, r=r, n=n, N=N)
    dat <- lapply(dat, function(x) design(data=x, b0=b0, b1=b1))
    # Running OLS
    OLS <- lapply(dat, function(z){
        reg <- lm(y~x, data=z)
    })
    # Storing the estimated betas
    bhat <- data.frame(do.call(rbind, lapply(OLS, coef)))
    colnames(bhat) <- c("b0hat", "b1hat")
    rownames(bhat) <- NULL
    # Finding x'x inverse
    xxInv <- lapply(dat, function(z){
            mat <- as.matrix(z[, c("const", "x")])
            Inv <- ginv(crossprod(mat))
            return(Inv)
    })
    # Finding the mean of inverse matrices
    xxInv.avg <- Reduce('+', xxInv)/n
    return(list(bEst=bhat, xxInv=xxInv.avg))
}
```



![](mc_output_files/figure-html/geoDistribution-1.png)<!-- -->![](mc_output_files/figure-html/geoDistribution-2.png)<!-- -->

The graph indicates that a significant percentage (22%) of the

# Question 2
Now vary the sample size ($n$) and redo 1) under the new
values. Report and plot your results. Comment.
![](mc_output_files/figure-html/q2-1.png)<!-- -->

# Question 3
Now vary the number of samples ($N$) and redo 1) under the new value.s
Report and plot your results. Comment.

![](mc_output_files/figure-html/q3-1.png)<!-- -->

# Question 4
Now vary the distribution of X and redo 1) under the new
assumptions. Report and plot your results. Comment.

# Question 5
Now set the mean of the error $\epsilon$ to be different from zero and
redo 1) under the new values. Comment.

# Question 6
Now draw the $\epsilon$ in such a way that it is in fact correlated
with $X$ and redo 1) under the new assumptions. Report and plot your
results. Comment.

