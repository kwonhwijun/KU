data <- gen_data(p=3, rho = 0.7)
X =data$X ; y = data$y


S <- function(z, lambda){ 
  (z - lambda) * (z > lambda) + (z + lambda) * (z < -lambda) + 0 *
    (abs(z) <= lambda)
}


# Scaling
y_sd <- sd(y)
X_sd <- apply(X, 2, sd)
y <- y / y_sd
X <- t(t(X) / X_sd)

# Add intercept term
X <- cbind(1, X)
n <- nrow(X)
p <- ncol(X)

# Initialization
init <- rep(0.5, p)
beta <- init

# parameter
lambda = 0.3
alpha = 0.5


eta <- X %*% beta
w <- exp(eta) # Mean = Variance
v <- eta + (y - w) / w

# IRLS with Ridge penalty
X_tilde <-  diag(c(sqrt(w))) %*% X # mean : 1, sd :1
y_tilde <- diag(c(sqrt(w))) %*% v # var : 1

u <- (y_tilde -mean(y_tilde))
z <- X_tilde
r <- u - z %*% beta

for (iter in 1:1000) {
  eta <- X %*% beta
  w <- exp(eta) # Mean = Variance
  v <- eta + (y - w) / w
  
  # IRLS with Ridge penalty
  X_tilde <-  diag(c(sqrt(w))) %*% X # mean : 1, sd :1
  y_tilde <- diag(c(sqrt(w))) %*% v # var : 1

  u <- (y_tilde -mean(y_tilde))
  z <- X_tilde
  # residual
  new.beta <- beta
  r <- u - z %*% beta
  # Coordinate Descent
  for (j in 1:p){
    temp <- beta[j] + crossprod(z[, j], r) / n
    new.beta[j] <- S(temp, lambda*alpha)/(1+lambda*(1-alpha)) 
    r <- r - (new.beta[j] - beta[j]) * z[, j]
    print(r)
  }
  delta <- max(abs(new.beta - beta))
  if (delta < 1.0e-5) break
  beta <- new.beta
}