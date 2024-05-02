# 1-(a)
getwd()
train <- matrix(scan("./train.txt"), 500, 51)
test <- matrix(scan("./test.txt"), 500, 51)

x <- train[,-51]
y <- train[,51]

x.test <- test[,-51]
y.test <- test[,51]

# set soft-thresholding function
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
  beta <- coef(lm(u ~ z - 1));r <- u - z %*% beta
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
  beta <- new.beta/s ;
  beta0 <- mean(y) - crossprod(beta, m)
  list(beta = c(beta0, beta))
}

prediction_error_elastic <- function(x.test,y.test,object){
  y.pred <- x.test %*% matrix(object$beta[2:(ncol(x)+1)],ncol=1) +
    object$beta[1]
  mse <- mean((y.pred-y.test)^2)
  return(mse)
}

test.lambda2 <- seq(0,1,by=0.001)
n <- length(test.lambda2)

MSE_storage <- matrix(0,ncol=1,nrow=n)

for(i in 1:n){
  CD.ELASTIC <- cd.elastic(x,y,lambda=test.lambda2[i],alpha=0.5)
  MSE_storage[i,1] <- prediction_error_elastic(x.test,y.test,CD.ELASTIC)
}

best_lambda <- test.lambda2[which(MSE_storage == min(MSE_storage) ,
                                  arr.ind = TRUE)[1]]

min(MSE_storage)
best_lambda

# Compare results with glmnet
library(glmnet)
lambda_values <- seq(0,1,by=0.001)
mse_list <- numeric(length(lambda_values))

for (i in seq_along(lambda_values)) {
  model <- glmnet(x, y, alpha=0.5, lambda=lambda_values[i])
  y_pred<- predict(model, newx=x.test)
  mse_list[i] <- mean((y.test - y_pred)^2)
}

min_mse_idx <- which.min(mse_list)
lambda_min <- lambda_values[min_mse_idx]
min_mse <- mse_list[min_mse_idx]

cat("Lambda_Min:", lambda_min, "\nMin_MSE:", min_mse)

# MSE vs Lambda graph
plot(lambda_values, mse_list, col = "blue", lwd = 2,
     xlab = "Lambda", ylab = "Test MSE", main = "Test MSE vs. Lambda")
points(lambda_min, min_mse, col = "red", pch = 19, cex = 1.5)
text(lambda_min, min_mse, labels = sprintf("Min MSE at Lambda=%.4f", lambda_min), 
     pos = 4, col = "red")

##################################################################################
# 1-(b)
# with given dataset
# First coefficient is an intercept
cd.elastic(x,y,0.038,0.5)$beta[cd.elastic(x,y,0.038,0.5)$beta != 0]

model<-glmnet(x,y,alpha=0.5,lambda = 0.038)

# same result with "cd.elastic" function
# this result does not report intercept
model$beta[model$beta != 0]
