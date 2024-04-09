## ----setup, include=FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------
bisection <- function(f, a, b, maxiter = 100, eps = 1.0e-5) {
  if (f(a) * f(b) > 0) stop("f(a) and f(b) must have different signs")
  u <- b; l <- a; iter <- 0
  while (iter < maxiter) {
    m <- (u + l)/2
    if (f(u) * f(m) < 0) { l <- m
    } else if (f(l) * f(m) < 0) { u <- m
    } else break
  if (abs(l - u) < eps) break
  iter <- iter + 1
  }
  if (iter == maxiter) warning("maximum iteration reached!")
  list(sol = m, iteration = iter)
}


## --------------------------------------------------------------------------------------------------------
f <- function(x) cos(x) - x
a <- -10
b <- 10


## --------------------------------------------------------------------------------------------------------
obj <- bisection(f, a, b)
print(obj)


## --------------------------------------------------------------------------------------------------------
newton <- function(f, df, init = 0, maxiter = 100, eps = 1.0e-8)
{
  x <- init; iter <- 0
  while (iter < maxiter) {
    new.x <- x - f(x)/df(x)
    if (abs((new.x - x)/x) < eps) break
    iter <- iter + 1
    x <- new.x
  }
  if (iter == maxiter) warning("maximum iteration reached!")
  list(sol = new.x, iteration = iter)
}


## --------------------------------------------------------------------------------------------------------
f <- function(x) cos(x) - x; df <- function(x) -sin(x) - 1
obj <- newton(f, df, init = 1)
print(obj)


## ---- echo = F-------------------------------------------------------------------------------------------
my.logit <- function(X, y, init = NULL, max.iter = 100, eps = 1.0e-5)
{
  if (is.null(init)) init <- rep(0, ncol(X))
  beta <- init
  for (iter in 1:max.iter)
  {
    
    eta <- X %*% beta
    p <- exp(eta)/ (1 + exp(eta))
    w <- c(p * (1 - p))
    z <- X %*% beta + (y-p)/w
    
    tilde.X <- X * sqrt(w)
    tilde.z <- z * sqrt(w)
    qr.obj <- qr(tilde.X)
    new.beta <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.z))
    
    if (max(abs(new.beta - beta))/max(abs(beta)) < eps) break
    beta <- new.beta
  }
  if (iter == max.iter) warning("Algorithm may not be converged!")
  obj <- list(est = c(beta), iterations = iter)
}






## --------------------------------------------------------------------------------------------------------
set.seed(1)
n <- 100 # sample size
p <- 3   # predictor dimension
x <- matrix(rnorm(n*p), n, p) # generate predictor
X <- cbind(rep(1, n), x)      # design matrix
beta <- rep(1, p+1) # true beta
eta <- X %*% beta   # true eta (linear term)
pi <- exp(eta)/(1 + exp(eta)) # pi = mu = E(y|x)
y <- rbinom(n, 1, pi)         # generate reponse


## --------------------------------------------------------------------------------------------------------
#  my function based on NR (IWLS)
obj1 <- my.logit(X, y, max.iter = 100)
hat.beta1 <- obj1$est


## --------------------------------------------------------------------------------------------------------
# check with R-built-in function, glm  
obj2 <- glm(y ~ x, family = "binomial")
hat.beta2 <- coefficients(obj2)


## --------------------------------------------------------------------------------------------------------
# compare
print(head(cbind(hat.beta1, hat.beta2)))

