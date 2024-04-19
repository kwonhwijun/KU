## ----setup, include=FALSE, message = F, warning=F-------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(kernlab)  


## -------------------------------------------------------------------------------
set.seed(2) 
n <- 20 # Toy Example
x1 <- rnorm(n); x2 <- rnorm(n); x <- cbind(x1, x2)
beta <- c(1,1); e <- rnorm(n, 0, 0); y <- sign(c(x %*% beta + e))


## ---- echo = F------------------------------------------------------------------
p <- ggplot(data.frame(x,y = as.factor(y)), aes(x=x1, y=x2, colour=y)) + 
  geom_point() + ggtitle("SVM") +
  theme(plot.title = element_text(hjust = 0.5))
p + geom_abline(intercept = 0, slope=-1, color = 'blue', lty = 2)


## -------------------------------------------------------------------------------
K <- x %*% t(x)
K.star <- K * (y %*% t(y))


## -------------------------------------------------------------------------------
C <- 1
H <- K.star; c <- rep(-1, n)
b <- 0; r <- 0
A <- matrix(y, 1, n)
l <- rep(0, n) 
u <- rep(1, n) 
obj <- ipop(c, H, A, b, l, u, r)


## -------------------------------------------------------------------------------
alpha <- obj@primal 
round(alpha, 2)


## -------------------------------------------------------------------------------
sv.index <- which(1.0e-7 < alpha & alpha < (C-1.0e-7))
sv.index


## -------------------------------------------------------------------------------
beta <- apply(y * alpha * x, 2, sum)
beta


## -------------------------------------------------------------------------------
temp <- y[sv.index] - x[sv.index,] %*% beta
beta0 <- mean(temp)
beta0


## ---- echo = F------------------------------------------------------------------
y[sv.index] = 0   # support vectors
p <- ggplot(data.frame(x,y = as.factor(y)), aes(x=x1, y=x2, colour=y)) + 
  geom_point() + ggtitle("SVM") + theme(plot.title = element_text(hjust = 0.5))

p + geom_abline(intercept= 0, slope=-1, color='blue', lty = 2) + 
  geom_abline(intercept = -beta0/beta[2], slope = -beta[1]/beta[2], col = 3) + 
  geom_abline(intercept = (1-beta0)/beta[2], slope = -beta[1]/beta[2], col = 3, lty = 3) + 
  geom_abline(intercept = (-1-beta0)/beta[2], slope = -beta[1]/beta[2], col = 3, lty = 3)

