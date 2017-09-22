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

## Setup
We will operate with the following model:

   $y_i = \beta_0 + x_i \beta_1 + \epsilon_i$
   
where, $y_i$ and $x_i$ are random variables. We observe $N$ samples
   of $i=1,2, \dots, n$ obersvations of $y$ and $x$.

We will estimate
   
   $y_i = \hat \beta_0 + x_i \hat \beta_1 + e_i$






## Drawing random samples
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
## Generating $y$ (dependent variable)

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

## Putting it all together
The next step is to build a wrapper function that draws random
samples, designs a matrix for OLS, runs OLS on each sample and returns
output as the predicted values of coefficients ($\hat \beta_0$, $\hat
\beta_1$) and the variance-covariance matrix of $\hat \beta$. This
function takes as arguments values for both the ```draw``` function and the
```design``` function defined above.


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
    # Finding the x'x inverse
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

# Question 1
*First assume that the $\epsilon$ and the regressors are
uncorrelated. Assume that both $X$ and $\epsilon$ are drawn from a
normal distribution $\epsilon \sim N(0, \sigma_{\epsilon}^2), X \sim
N(\mu_x, \sigma_x^2)$. Start with a sample size ($n$) of
$n=100$ and define the number of samples to be simulated ($N$) to be
$N=100$ as well. You also need to define the true values of $\beta_0$
and $\beta_1$. Report the results that you fnid for the means and
variance-covariance matrix of $\hat \beta_0$ and $\hat
\beta_1$. Finally, plot the distribution of the estimated parameters
and the theoretical normal distribution that they are supposed to
follow.*

I use the following values for estimation:
$\beta_0=9$, $\beta_1=2$, $\mu_x=10$, $\mu_{\epsilon}=0$,
$\sigma_x^2=4$, $\sigma_{\epsilon}^2=1$, $r_{x\epsilon}=0$

The implementation is given using the ```simulate.OLS``` function as
shown below:

```r
baseline <- simulate.OLS(muX=10, vX=4, muE=0, vE=1, r=0, n=100, N=100, b0=9, b1=2)
```

![](mc_output_files/figure-html/beta0-1.png)<!-- -->

![](mc_output_files/figure-html/beta1-1.png)<!-- -->



# Question 2
*Now vary the sample size ($n$) and redo 1) under the new
values. Report and plot your results. Comment.*

| Sample size ($n$) | $E[\hat \beta_0|X]$ | $E[\hat \beta_1|X]$ |
|-------------------|--------------------:|--------------------:|
| n=10              |               8.826 |               2.019 |
| n=100             |               9.043 |               1.997 |
| n=1000            |               8.988 |               2.001 |



![](mc_output_files/figure-html/q2b0hat-1.png)<!-- -->


![](mc_output_files/figure-html/q2b1hat-1.png)<!-- -->

# Question 3
*Now vary the number of samples ($N$) and redo 1) under the new value.s
Report and plot your results. Comment.*

| No. of samples ($N$) | $E[\hat \beta_0|X]$ | $E[\hat \beta_1|X]$ |
|----------------------|--------------------:|--------------------:|
| N=10                 |               9.025 |               1.998 |
| N=100                |               9.043 |               1.997 |
| N=1000               |               9.001 |               2.000 |



![](mc_output_files/figure-html/q3b0hat-1.png)<!-- -->

![](mc_output_files/figure-html/q3b1hat-1.png)<!-- -->

# Question 4
*Now vary the distribution of X and redo 1) under the new
assumptions. Report and plot your results. Comment.*

We vary the distribution of $X$ by changing the variance of $X$. Note
that, we derived in class that the variance of slope coefficients is
inversly proportional to $(X'X)$. In particular,

$var(\hat \beta | X) = \sigma_{\epsilon}^2 (X'X)^{-1}$

Thus, if we increase (decrease) the variation in $X$, one would expect that the
variation in $\hat \beta$ will decrease (increase).

| Dist. of X      | $E[\hat \beta_0|X]$ | $E[\hat \beta_1|X]$ |
|-----------------|--------------------:|--------------------:|
| $\sigma_X^2=2$  |               9.054 |               1.996 |
| $\sigma_X^2=4$  |               9.043 |               1.997 |
| $\sigma_X^2=16$ |               9.029 |               1.999 |


![](mc_output_files/figure-html/q4b0hat-1.png)<!-- -->
The figure shows the variation in $\hat \beta_0$ for three cases a)
$\sigma_X^2=2$, b) $\sigma_X^2=4$ (baseline model), and c)
$\sigma_X^2=16$. In case *a)*, $X$ has lower variance, which implies
that the variation in $\hat \beta_0$ will be higher and thus, the
distribution will be wider. Intuitively, lower variance in X implies
that we have concentration of data in a smaller range. It becomes difficult
to find the true $\hat \beta$ since we can not know how the
relationship holds outside the range. Similarly, in case *c)*, $X$ has
higher variance and hence, the distribution of $\hat \beta_0$ will
shrink compared to the baseline model in *b)*.

        
![](mc_output_files/figure-html/q4b1hat-1.png)<!-- -->


# Question 5
*Now set the mean of the error $\epsilon$ to be different from zero and
redo 1) under the new values. Comment.*

In particular, one can, mathematically, show that a change in the mean of $\epsilon$
affects *only* the intercept. Consider $E[\mu_{\epsilon}] = a$ (some constant,
say). One can re-write the original model as:

$y_i = \tilde{\beta_0} + x_i \beta_1 + u_i$

where, $\tilde{\beta_0}=\beta_0+a$, and, $u_i = \epsilon_i-a$.
Thus, we get, $E[\mu_{u_i}]=0$
Therefore, one would expect the intercept to shift by $a$ and the
slope coefficient to remain unchanged.

| Error term         | $E[\hat \beta_0|X]$ | $E[\hat \beta_1|X]$ |
|--------------------|--------------------:|--------------------:|
| $\mu_{\epsilon}=2$ |              11.043 |               1.997 |
| $\mu_{\epsilon}=0$ |               9.043 |               1.997 |


![](mc_output_files/figure-html/q5b0hat-1.png)<!-- -->

The figure above compares the estimate for $\hat \beta_0$ for two
cases, namely: a) $\mu_{\epsilon}=2$ and b) $\mu_{\epsilon}=0$. As
explained above, we see that case *a)* with non-zero (positive) mean for the
error shifts the distribution of $\hat \beta_0$ to the right when
compared to case *b)* (Baseline model)


![](mc_output_files/figure-html/q5b1hat-1.png)<!-- -->

The figure compares the distribution of $\hat \beta_1$ for the cases
mentioned above. As expected, the slope coefficients do not change,
and the two distributions superimpose on each other.

# Question 6
*Now draw the $\epsilon$ in such a way that it is in fact correlated
with $X$ and redo 1) under the new assumptions. Report and plot your
results. Comment.*

| Correlation coeff      | $E[\hat \beta_0|X]$ | $E[\hat \beta_1|X]$ |
|------------------------|--------------------:|--------------------:|
| $r_{X, \epsilon}=0.8$  |               7.040 |               2.197 |
| $r_{X, \epsilon}=0$    |               9.043 |               1.997 |
| $r_{X, \epsilon}=-0.8$ |              11.036 |               1.798 |

![](mc_output_files/figure-html/q6b0hat-1.png)<!-- -->

The figure above compares the distribution of $\hat \beta_0$ for three
cases: namely a) $r_{X,\epsilon} = 0.8$, b) $r_{X, \epsilon} = 0$, and c)
$r_{X,\epsilon}=-0.8$. When $X$ and $\epsilon$ are positively (or, negatively)
correlated, the strict exogeneity assumption (A1.2) is violated. This
leads to OLS estimates being biased. In particular, positive
(negative) correlation leads to a downward (upward) bias in our
estimates.

![](mc_output_files/figure-html/q6b1hat-1.png)<!-- -->

The figure shown above compares the distribution of $\hat \beta_1$ for
the three cases as mentioned above. Once again, correlation between
the regressor and error terms leads to a violation of strict
exogeneity assumption resulting in biased estimates of $\hat
\beta_1$. However, note that in this case, positive (negative)
correlation leads to an upward (downward) bias for $\hat \beta_1$.
