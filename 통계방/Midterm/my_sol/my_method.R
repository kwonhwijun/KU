
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
my_poi <- function(x, y, init = NULL, max_iter = 1000, eps = 1e-5, offset = NULL) { # Scaling
  X <- x
  y_sd <- sd(y); X_sd <- apply(X, 2, sd)
  y <- y /y_sd; X <- t(t(X)/ X_sd)
  # Add intercept term
  X <- cbind(1, X) ; n <- nrow(X); p <- ncol(X) # INIT
  if (is.null(init)) init <- rep(0, p); beta <- init # Offset
  if (is.null(offset)) offset <- rep(0, n)
  # Iteration
  for (iter in 1:max_iter) {
    eta <- X %*% beta
    w <- exp(eta + offset) # Mean = Variance
    z <- eta + (y-w)/w
    # IRLS
    X_tilde <- diag(c(sqrt(w))) %*% X
    z_tilde <- diag(c(sqrt(w))) %*% z
    qr_obj <- qr(X_tilde)
    new_beta <- backsolve(qr_obj$qr, qr.qty(qr_obj, z_tilde)) # Check Convergence
    if (max(abs(new_beta - beta))/ max(abs(beta)) < eps) break
    beta <- new_beta
  }
  # Warning
  if (iter == max_iter) warning("Algorithm may not have converged!") # Restore beta coef
  beta <- c(beta) * c(1, 1/X_sd) + c(log(y_sd), rep(0, p-1))
  # Result
  list(est = t(beta), iterations = iter)
}



# 2. Ridge 
my_ridge<-function(x,y,lambda =1,alpha=0){
  n<-nrow(x)
  p<-ncol(x)
  beta<-coef(glm(y ~ x - 1, family = "poisson"))
  eta<-x%*%beta
  mu<- exp(eta)
  w<-diag(c(mu),n)
  z<- x%*%beta+solve(w)%*%(y-mu)
  tilde.X<-sqrt(w)%*%x
  ss <- colSums(tilde.X^2)
  tilde.y<-sqrt(w)%*%z
  tilde.r<-tilde.y-tilde.X%*%beta
  for (iter in 1 : 100){
    new.beta<-beta
    for (j in 1 : p){
      temp<-crossprod(tilde.r,matrix(tilde.X[,j]))+beta[j]*ss[j]
      new.beta[j]<-S(temp,(n*alpha*lambda))/(ss[j]+n*lambda*(1-alpha))
      tilde.r <- tilde.r - (new.beta[j] - beta[j])*tilde.X[,j]
      
    }
    delta <- max(abs(new.beta - beta))
    if (delta<1.0e-3)break
    beta<-new.beta
    
  }
  beta<-new.beta
  beta0<-log(mean(tilde.y))-crossprod(beta,colMeans(tilde.X))
  list(est = t(c(beta0, beta)))
}







# 3. LASSO
my_lasso<-function(x,y,lambda = 1,alpha = 1){
  n<-nrow(x)
  p<-ncol(x)
  beta<-coef(glm(y ~ x - 1, family = "poisson"))
  eta<-x%*%beta
  mu<- exp(eta)
  w<-diag(c(mu),n)
  z<- x%*%beta+solve(w)%*%(y-mu)
  tilde.X<-sqrt(w)%*%x
  ss <- colSums(tilde.X^2)
  tilde.y<-sqrt(w)%*%z
  tilde.r<-tilde.y-tilde.X%*%beta
  for (iter in 1 : 100){
    new.beta<-beta
    for (j in 1 : p){
      temp<-crossprod(tilde.r,matrix(tilde.X[,j]))+beta[j]*ss[j]
      new.beta[j]<-S(temp,(n*alpha*lambda))/(ss[j]+n*lambda*(1-alpha))
      tilde.r <- tilde.r - (new.beta[j] - beta[j])*tilde.X[,j]
      
    }
    delta <- max(abs(new.beta - beta))
    if (delta<1.0e-3)break
    beta<-new.beta
    
  }
  beta<-new.beta
  beta0<-log(mean(tilde.y))-crossprod(beta,colMeans(tilde.X))
  list(est = t(c(beta0, beta)))
}


# 4. Elastic
my_elastic<-function(x,y,lambda = 5, alpha=0.5){
  n<-nrow(x)
  p<-ncol(x)
  beta<-coef(glm(y ~ x - 1, family = "poisson"))
  eta<-x%*%beta
  mu<- exp(eta)
  w<-diag(c(mu),n)
  z<- x%*%beta+solve(w)%*%(y-mu)
  tilde.X<-sqrt(w)%*%x
  ss <- colSums(tilde.X^2)
  tilde.y<-sqrt(w)%*%z
  tilde.r<-tilde.y-tilde.X%*%beta
  for (iter in 1 : 100){
    new.beta<-beta
    for (j in 1 : p){
      temp<-crossprod(tilde.r,matrix(tilde.X[,j]))+beta[j]*ss[j]
      new.beta[j]<-S(temp,(n*alpha*lambda))/(ss[j]+n*lambda*(1-alpha))
      tilde.r <- tilde.r - (new.beta[j] - beta[j])*tilde.X[,j]
      
    }
    delta <- max(abs(new.beta - beta))
    if (delta<1.0e-3)break
    beta<-new.beta
    
  }
  beta<-new.beta
  beta0<-log(mean(tilde.y))-crossprod(beta,colMeans(tilde.X))
  list(est = t(c(beta0, beta)))
}


# 5. SCAD

S_scad <- function(z, lambda, a = 3.7){
  if (abs(z) < 2* lambda){
    S(z, lambda)
  } else if ((2 * lambda <= abs(z)) & (abs(z) <= a * lambda)){
    S(z, a * lambda / (a-1))/(1 - 1/(a-1))
  } else z
}

my_scad <-function(x,y,lambda = 5, alpha=0.5){
  n<-nrow(x)
  p<-ncol(x)
  beta<-coef(glm(y ~ x - 1, family = "poisson"))
  eta<-x%*%beta
  mu<- exp(eta)
  w<-diag(c(mu),n)
  z<- x%*%beta+solve(w)%*%(y-mu)
  tilde.X<-sqrt(w)%*%x
  ss <- colSums(tilde.X^2)
  tilde.y<-sqrt(w)%*%z
  tilde.r<-tilde.y-tilde.X%*%beta
  for (iter in 1 : 100){
    new.beta<-beta
    for (j in 1 : p){
      temp<-crossprod(tilde.r,matrix(tilde.X[,j]))+beta[j]*ss[j]
      new.beta[j]<-S_scad(temp,(n*alpha*lambda))/(ss[j]+n*lambda*(1-alpha))
      tilde.r <- tilde.r - (new.beta[j] - beta[j])*tilde.X[,j]
      
    }
    delta <- max(abs(new.beta - beta))
    if (delta<1.0e-3)break
    beta<-new.beta
    
  }
  beta<-new.beta
  beta0<-log(mean(tilde.y))-crossprod(beta,colMeans(tilde.X))
  list(est = t(c(beta0, beta)))
}

data = gen_data(p= 50, rho = 0.7) ; y = data$y ;X = data$X
result = my_scad(x = X, y= y)
result

