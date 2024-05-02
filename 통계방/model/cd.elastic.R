S <- function(z, lambda){ 
  (z - lambda) * (z > lambda) + (z + lambda) * (z < -lambda) + 0 *
    (abs(z) <= lambda)
}

# Coordinate decent algorithm for elastic net
cd.elastic <- function(x, y, lambda, alpha){
  # marginal standardization of x
  z <- scale(x); 
  m <- attr(z, "scaled:center"); 
  s <- attr(z, "scaled:scale")
  
  # centering of y
  u <- (y - mean(y))
  # initialization
  beta <- coef(lm(u ~ z - 1)); r <- u - z %*% beta
  for (iter in 1:100){
    new.beta <- beta
    for (j in 1:ncol(x)) {
      temp <- beta[j] + crossprod(z[,j], r)/nrow(x)
      new.beta[j] <- S(temp, lambda*alpha)/(1+lambda*(1-alpha)) 
      r <- r - (new.beta[j] - beta[j]) * z[,j]
    }
    delta <- max(abs(new.beta - beta))
    if (delta < 1.0e-5) break
    beta <- new.beta
  }
  beta <- new.beta/s ; beta0 <- mean(y) - crossprod(beta, m)
  obj <- list(beta = c(beta0,beta))
  return(obj)
}