set.seed(1)

p <- 10
theta <- 1:p

mse1 <- mse2 <- NULL
for (iter in 1:1000)
{
  x <- rnorm(p) + theta
  
  mle <- x
  jse <- (1 - (p-2)/(sum(x^2))) * x
  
  mse1[iter] <- sum((mle - theta)^2)
  mse2[iter] <- sum((jse - theta)^2)
}

mean(mse1/mse2)
