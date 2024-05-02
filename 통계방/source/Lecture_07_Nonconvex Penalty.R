## ----setup, include=FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo = F-------------------------------------------------------------------------------------------
S <- function(z, lambda) {    # soft-thresholding operator
  (z - lambda) * (z > lambda) + (z + lambda) * (z < -lambda) + 0 * (abs(z) <= lambda)
}


## --------------------------------------------------------------------------------------------------------
lasso.update <- S # LASSO
scad.update <- function(z, lambda, a = 3.7){ # SCAD
  if (abs(z) < 2 * lambda){
    S(z, lambda)
  } else if ((2 * lambda <= abs(z)) & (abs(z) <= a * lambda)){
    S(z, a * lambda / (a - 1)) / (1 - 1 / (a - 1))
  } else z
}
mcp.update = function(z, lambda, gamma){ # MCP
  if (abs(z) <= gamma * lambda){ㄴ
    S(z, lambda) / (1 - 1 / gamma)
  } else z
}


## ---- echo = F-------------------------------------------------------------------------------------------
# coordinate decent algorithm
ncv <- function(x, y, lambda, type = "lasso", a = 3.7, init = rep(0, p), max.iter = 100, eps = 1.0e-8){
  n <- length(y)
  p <- ncol(x)
  x <- scale(x)
  m <- attr(x, "scaled:center")
  s <- attr(x, "scaled:scale")
  my <- mean(y)
  y <- (y - my)    # centering of y
  beta <- init    # initialize beta
  r <- (y - x %*% beta)    # residual
  if (type == "lasso") {
    update.ft <- function(x, lambda) lasso.update(x, lambda)
  } else if (type == "scad") {
    update.ft <- function(x, lambda) scad.update(x, lambda, a)
  } else if (type == "mcp") {
    update.ft <- function(x, lambda) mcp.update(x, lambda, a)
  } else stop("type should be lasso, scad, or mcp!")
  for (t in 1:max.iter){    # start update
    new.beta <- beta
    for (j in 1:p){
      xj <- 1/n * crossprod(x[,j],  r) + beta[j]
      new.beta[j] <- update.ft(xj, lambda/s[j]) 
      r <- r - (new.beta[j] - beta[j]) * x[,j]  
    }
    if (max(abs(beta - new.beta)) < eps) break
    beta <- new.beta
  }
  beta <- beta / s[j]
  beta0 <- my - m %*% beta    # transform back
  index <- which(abs(beta) > eps)
  beta.info <- beta[index]
  obj = list(intercept = beta0,beta = beta.info, index = index)
}






## --------------------------------------------------------------------------------------------------------
set.seed(1); n <- 100; p <- 5
d <- 2; beta <- c(rep(1, d), rep(0, p-d))
x <- matrix(rnorm(n*p), n, p); e <- rnorm(n)
y <- x %*% beta + e; lambda <- 0.2

obj1 <- ncv(x, y, lambda, type = "lasso")    # lasso
obj2 <- ncv(x, y, lambda, type = "scad")    # scad
obj3 <- ncv(x, y, lambda, type = "mcp")     # mcp


## --------------------------------------------------------------------------------------------------------
beta1 <- beta2 <- beta3 <- rep(0, p)
beta1[obj1$index] <- obj1$beta
beta2[obj2$index] <- obj2$beta
beta3[obj3$index] <- obj3$beta
est <- cbind(beta, beta1, beta2, beta3)
colnames(est) <- c("true", "lasso", "scad", "mcp")
round(est, 3)

