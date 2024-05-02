
# Soft Thresholding Operater
S <- function(z, lambda){
  (z - lambda) * (z > lambda) + (z + lambda) * (z < -lambda) + 0 * (abs(z) < lambda)
}

# Coordinate Decent Alogorithm for Lasso
cd.lasso <- function(x, y, lambda){
  z <- scale(x);
  m <- attr(z, "scaled:center") # m: sample mean of x
  s <- attr(z, "scaled:scale") # s: sample variance of x
  u <- (y-mean(y)) # u : centered y
  p = ncol(x)
  # Initialization
  beta <- coef(lm(u~z-1)) # Initalization
  r<- u - z %*% beta # r : full residual
  
  # Iteration
  for (iter in 1:200){
    new.beta <- beta
    for (j in 1:p){
      temp <- beta[j] + (z[,j] %*% r)/n
      new.beta[j] <- S(temp, lambda/s[j])
      r <- r - (new.beta[j] - beta[j]) * z[,j]
    }
    delta <- max(abs(new.beta - beta))
    if (delta < 1.0e-5) break
    beta <- new.beta
  }
  beta <- new.beta/s
  beta0 <- mean(y) - crossprod(beta, m)
  c(beta0, beta)
}

sim_data <- function(n, p){
  set.seed(1) ; n <- 100 ; p <- 5
  x <- matrix(rnorm(n*p, 1, 1), n, p)
  e <- rnorm(n ,0, 0.5)
  true.beta <- rep(0, p+1) ; true.beta[1] <- 1
  true.beta[2:(p+1)] <- c(rep(1, 3), rep(0, p-3))
  y <- true.beta[1] + x %*% true.beta[-1] + e
  
  list(x=x, y=y, beta= true.beta)
}

# debug


data = sim_data(100, 5)
x = data$x
y = data$y
true.beta = data$beta
est0 <- coef(lm(y~x))
est1 <- cd.lasso(x, y, lambda = 0.1)
est2 <- coef(glmnet::glmnet(x, y, lambda = 0.1, standardize = F))

result = cbind(true.beta, est0, est1, est2)
rownames(result) = 0:5
colnames(result) <- c("true", "lm", "ours", "glmnet")
print(round(result, 4))

