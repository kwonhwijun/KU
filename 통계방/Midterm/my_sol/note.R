
gen_data <- function(p = 3, rho = 0.7){
  mu <- rep(0, p) # E(X) = mu = 0
  sigma <- outer(1:p, 1:p, FUN = function(i, j) rho^(abs(i-j))) # Var(X) 
  
  beta0 = 1 # intercept = 1
  beta = c(rep(1,3), rep(0, p-3)) # beta = (1, 1, 1) or (1, 1, 1, 0, 0,0 )
  
  # 전체 데이터 생성
  x <- mvrnorm(n=500, mu=mu, Sigma=sigma)
  e <- rnorm(500, 0, 0.5)
  mu_x <- exp(beta0 + x %*% beta + e)
  y = rpois(500, mu_x)
  list(X = x, y =y)
}


test_func <- function(){
  print("my_method is successfully imported")
}
library(glmnet)

# 1. Unpenalized Poisson Regression
d = gen_data(3, 0.7)
x = d$X ; y = d$y
# Scaling
y_sd <- sd(y); x_sd <- apply(x, 2, sd)
y <- y /y_sd; x <- t(t(x)/ x_sd)


# Add intercept term
x <- cbind(1, x) ; n <- nrow(x); p <- ncol(x) # INIT
init <- rep(0, p);
beta <- init # 
beta
if (is.null(offset)) offset <- rep(0, n)
# Iteration

eta <- x %*% beta
w <- exp(eta + offset) # Mean = Variance
z <- eta + (y-w)/w
# IRLS
X_tilde <- diag(c(sqrt(w))) %*% x
X_tilde
z_tilde <- diag(c(sqrt(w))) %*% z
qr_obj <- qr(X_tilde)
new_beta <- backsolve(qr_obj$qr, qr.qty(qr_obj, z_tilde)) # Check Convergence

new_beta
if (max(abs(new_beta - beta))/ max(abs(beta)) < eps) break
beta <- new_beta

# Warning
if (iter == max_iter) warning("Algorithm may not have converged!") # Restore beta coef
beta <- c(beta) * c(1, 1/x_sd) + c(log(y_sd), rep(0, p-1))
beta
# Result
list(est = t(beta), iterations = iter)

beta
x_sd

my_poi(d$X, d$y)
