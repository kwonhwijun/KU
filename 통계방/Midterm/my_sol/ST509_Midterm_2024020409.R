
library(MASS)
library(glmnet)

# 1. Unpenalized Poisson Regression
my_poi <- function(x, y, init = NULL, max_iter = 1000, eps = 1e-5, offset = NULL) {
  # Scaling
  y_sd <- sd(y); x_sd <- apply(x, 2, sd)
  y <- y /y_sd; x <- t(t(x)/ x_sd)
  # Add intercept term
  x <- cbind(1, x) ; n <- nrow(x); p <- ncol(x) # INIT
  if (is.null(init)) init <- rep(0, p); beta <- init # Offset
  if (is.null(offset)) offset <- rep(0, n)
  # Iteration
  for (iter in 1:max_iter) {
    eta <- x %*% beta
    w <- exp(eta + offset) # Mean = Variance
    z <- eta + (y-w)/w
    # IRLS
    X_tilde <- diag(c(sqrt(w))) %*% x
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



# Data Generating Function
generate_data <- function(p, nu, seed) {
  set.seed(seed)
  mu <- rep(0, p) # E(X) = mu = 0
  sigma <- outer(1:p, 1:p, FUN = function(i, j) nu^(abs(i-j))) # Var(X) 

  beta0 = 1 # intercept = 1
  beta = c(rep(1,3), rep(0, p-3)) # beta = (1, 1, 1) or (1, 1, 1, 0, 0,0 )
  
  x <- mvrnorm(n=5500, mu=mu, Sigma=sigma)
  e <- rnorm(5500, 0, 0.5)
  mu_x <- exp(beta0 + x %*% beta)
  y <- rpois(5500, mu_x)
  
  # Training sample_size : 500, Test sample size = 5000
  x_train <- x[1:500,] ; y_train <- y[1:500]
  x_test <- x[501:5500,] ; y_test <- y[501:5500]
  
  return(list(train = list(x = x_train, y = y_train), test = list(x = x_test, y = y_test), true_beta = c(beta0, beta)))
}


# Number of Independent Repetitions : 100
#---------------------------#

my_beta <- function(p, rho, method_func){
  data = vector('list', 100)
  seed = 2024020409
  beta_hat_list = vector('list', 100)
  residual_list = numeric(100)
  cs_list = numeric(100)
  time_list = numeric(100)
  for (i in 1:100){
    time_taken <- system.time({
      data = generate_data(p, rho, seed + i)
      x_train = data$train$x ; y_train = data$train$y
      x_test = data$test$x ; y_test = data$test$y
      true_beta = data$true_beta
      result = method_func(x = x_train, y = y_train)
    })
    beta_hat = result$est
    beta_hat_list[[i]] = beta_hat
    residual_list[i] = sum((beta_hat-true_beta)^2)
    
    selected_beta_hat <- ifelse(abs(beta_hat)>0 , 1, 0)
    cs_list = sum(selected_beta_hat[-1][1:3] == true_beta[-1][1:3])
    is_list = sum(selected_beta_hat[-1][4:p] != true_beta[-1][1:3])
    time_list[i] = time_taken["elapsed"]
  } 
  
  beta_bar <- Reduce("+", beta_hat_list) / length(beta_hat_list)
  
  # Caculate Variance 
  mc_variance <- 0
  for (beta_hat in beta_hat_list) {
    diff <- matrix(beta_hat - beta_bar, ncol = length(beta_hat))
    mc_variance <- mc_variance + diff %*% t(diff)
  }
  
  mse <- mean(residual_list)
  bias <- sum(abs(beta_bar - true_beta))
  mc_variance <- mc_variance / length(beta_hat_list)
  var <- sum(diag(mc_variance))  
  
  
  time <- mean(time_list)
  cs <- mean(cs_list)
  is <- sum(selected_beta_hat[-1] != true_beta[-1])
  if (cs == p) {
    ac <- 1
  } else ac <- 0
  
  list(beta_hat = beta_hat_list, 
       true_beta = true_beta, 
       MSE = mse,
       VAR = var,
       BIAS = bias,
       CS = cs,
       IS = is,
       AC= ac,
       TIME = time)
}


#-----------------------------#
# Monte Carlo Simulation 

# Parameter Setting
p_list = as.integer(c(3, 50)) ; rho_list = c(0, 0.7)
method_list <- list(
  unpenal = my_poi, ridge = my_ridge, lasso = my_lasso,
  elastic =my_elastic, scad = my_scad
)
# Result Table
results_df <- data.frame(
  rho=numeric(), p=numeric(), Method=character(), MSE=numeric(), VAR=numeric(),
  BIAS=numeric(), CS=numeric(), IS=numeric(), AC=numeric(), TIME=numeric(), stringsAsFactors=FALSE)

for (rho in rho_list) {
  for (p in p_list){
    for (method_name in names(method_list)) {
      method <- method_list[[method_name]]
      result = my_beta(p, rho, method)
      # compute result for each parameter
      results_df <- rbind(results_df, data.frame(
        rho=rho, p=p,
        Method=method_name,
        MSE=result["MSE"],
        VAR=result["VAR"],
        BIAS=result["BIAS"], 
        CS=result["CS"],
        IS=result["IS"],
        AC=result["AC"],
        TIME=result["TIME"])
        )
      
      print(results_df)
    }
  }
} 

results_df

