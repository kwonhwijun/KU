## ----setup, include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## Group Lasso : Update Blockwise CD

# Simulate Data
set.seed(1); n <- 100; p <- 6 
x <- matrix(rnorm(n*p, 2, 1), n, p) # predictor
e <- rnorm(n, 0, 0.5)               # noise
true.beta <- c(3, -2,-1, 0, 0, 2, 1)

# Group Structure
J <- 3; id <- as.list(1:J); id[[1]] <- 1:2; id[[2]] <- 3:4; id[[3]] <- 5:6
y <- true.beta[1] + x %*% true.beta[-1] + e # response

# Regularization Parameter
lambda <- 10

# Centering
mx <- apply(x, 2, mean); x <- t(t(x) - mx)
my <- mean(y)          ; y <- y - my

# Blockwise Orthonomalization
z <- x; Qj.list <- lj.list <- as.list(1:J)
for (j in 1:J){
  idj <- id[[j]]
  obj <- eigen(crossprod(x[,idj,drop = F], x[,idj,drop = F]))
  Qj.list[[j]] <- Qj <- obj$vectors; lj.list[[j]] <- lj <- obj$values
  z[,idj] <- x[,idj,drop = F] %*% Qj %*% diag(1/sqrt(lj)) 
}

# Initialization
beta <- coef(lm(y ~ z - 1))    # initialization
r <- y - z %*% beta    # full residual


## -------------------------------------------------------------------------------
for (i in 1:100){
  new.beta <- beta
  for (j in 1:J) { # blockwise update
  # partial residual
    zj <- z[,id[[j]], drop = F]
    rj <- r + zj %*% beta[id[[j]], drop = F]
  # update beta
    zjrj <- crossprod(zj, rj)
    lj <- sqrt(sum(sqrt(lj.list[[j]])^2))
    a <- max(0, 1 - (lambda/lj)/sqrt(crossprod(zjrj, zjrj)))
    new.beta[id[[j]]] <- a * zjrj
  # update residuals
    r <- rj - zj %*% new.beta[id[[j]]]
  } # end of update for each block
  if (max(abs(beta - new.beta)) < 1.0e-4) {break 
  } else {beta <- new.beta}
}

# Transform back to original scale
for (j in 1:J){
  beta[id[[j]]] <- Qj.list[[j]] %*% diag(1/sqrt(lj.list[[j]])) %*% beta[id[[j]]]
}

# Result
beta0 <- my - mx %*% beta #Intercept
beta <- c(beta0, beta)
print(cbind(beta, true.beta))


## Grapical LASSO
S <- function(z, lambda) {    # set soft-thresholding function
  (z - lambda) * (z > lambda) + (z + lambda) * (z < -lambda) + 0 * (abs(z) <= lambda)
}


## ---- echo = F------------------------------------------------------------------
cd.lasso <- function(x, y, lambda){
  # CD algorithm for lasso    # marginal standardization of x
  n <- length(y); p <- nrow(x)
  s <- sqrt(apply(x^2, 2, sum)/n); z <- t(t(x)/s)
  apply(z^2, 2, sum)
  # initialization
  beta <- coef(lm(y ~ z - 1)); r <- y - z %*% beta
  
  for (iter in 1:100) {
    new.beta <- beta
    for (j in 1:p) {
      temp <- beta[j] + crossprod(z[,j], r)/n
      new.beta[j] <- S(temp, lambda/s[j]) 
      r <- r - (new.beta[j] - beta[j]) * z[,j]
    }
    delta <- max(abs(new.beta - beta))
    if (delta < 1.0e-3) break
    beta <- new.beta
  }
  beta <- new.beta/s 
  c(beta)
}


## -------------------------------------------------------------------------------
library(mvtnorm); set.seed(1)
# inverse covariance matrix
Phi <- matrix(0,nrow=4,ncol=4); 
Phi[row(Phi) >= col(Phi)] <- c(10,0,5,4,10,0,6,10,0,10)
Phi <- (Phi + t(Phi)); diag(Phi) <- 10
# sample covariance matrix
Sigma <- solve(Phi)
n <- 500; x <- rmvnorm(n, rep(0, 4), Sigma)
s <- cov(x); lambda <- 0.02
p <- nrow(s)


## -------------------------------------------------------------------------------
W <- s    # Initialization
for (i in 1:100){
  W.old <- W; beta <- matrix(0, p-1, p)
  for (j in 1:p)  {
    W11 <- W[-j,-j,drop = F]
    
    Wsqrt <- chol(W11)  # W_11^1/2
    Wsqrt.i <- t(backsolve(Wsqrt, diag(p-1))) # Its inverse
    
    beta[,j] <- cd.lasso(x = Wsqrt, y = Wsqrt.i %*% s[-j,j], lambda)
    W[-j,j] <- W[j,-j] <- c(W11 %*% beta[,j])
  }
  if (max(abs(W - W.old)) < 1.0e-5) break
}


## -------------------------------------------------------------------------------
# compute precision matrix theta
theta <- matrix(0, p, p) # storage for Theta
for (j in 1:p){
  theta[j,j] <- 1/(W[j,j] - W[-j,j] %*% beta[,j])
  theta[j,-j] <- theta[-j,j] <- c(-beta[,j]  * theta[j,j])
}


## -------------------------------------------------------------------------------
print(round(Phi, 3))
print(round(theta, 3))


## -------------------------------------------------------------------------------
print(round(Sigma, 3))
print(round(W, 3))

