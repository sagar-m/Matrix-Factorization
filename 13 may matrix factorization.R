#ALS
als_algorithm <- function(v,k)
{
  f <- function(V,W,H) return ((1/2) * sum((V - W%*% H)^2))
  n <- nrow(v)
  m <- ncol(v)
  W <- matrix(runif(n*k, min = 0, max = 2), nr = n, nc = k)
  H <- solve(t(W)%*%W)%*%t(W)%*%V
  H[H<0] <- 0
  W <- V%*%t(H)%*%solve(H%*%t(H))
  W[W<0] <- 0
  H0 <- H # storing the current values so that it is not overwritten
  W0 <- W
  b <- f(V,W0,H0) #previous value stored in b
  while(b > 0.00001)
  {
    b <- f(V, W0, H0)
    H <- solve(t(W0)%*%W0)%*%t(W0)%*%V # computing the new values
    H[H<0] <- 0
    W <- V%*%t(H)%*%solve(H%*%t(H))
    W[W<0] <- 0
    b <- (b - f(V,W,H))
    H0 <- H
    W0 <- W
  }
  return(list(W,H))
}

n <- 5
m <- 7
k <- 2
V <- matrix(round(runif(n*m, min = 0, max = 5)), nr = n, nc = m)
V
als_algorithm(V,2)


data <- read.csv("movie ratings by class.csv", stringsAsFactors = F, row.names = 1)
data["amol", "bahubali2"] <- 0
V <- as.matrix(data)
head(data,5)
class(data)
als_algorithm(V,2)








#MU
mu_algorithm <- function(v,k)
{
  #f <- function(V,W,H) return ((1/2) * sum((V - W%*% H)^2))
  f <- function(V,W,H) return ((1/2) * norm((V - W%*% H),"F")^2)
  
  n <- nrow(v)
  m <- ncol(v)
  W0 <- matrix(runif(n*k, min = 0, max = 2), nr = n, nc = k)
  H0 <- matrix(runif(m*k, min = 0, max = 2), nr = k, nc = m)
  H <- H0 * ((t(W0) %*% V) / (t(W0) %*% (W0) %*% H0))
  W <- W0 * ((V %*% t(H)) / (W0 %*% H %*% t(H)))
  while(abs(f(V,W0,H0) - f(V,W,H)) > 0.000001)
   {
    H0 <- H
    W0 <- W
    H <- H0 * ((t(W0) %*% V) / (t(W0) %*% (W0) %*% H0))
    W <- W0 * ((V %*% t(H)) / (W0 %*% H %*% t(H)))
  }
  return(list(W,H))
}

mu_algorithm(V,2)

n <- 5
m <- 7
k <- 2
V <- matrix(round(runif(n*m, min = 0, max = 5)), nr = n, nc = m)
V
als_algorithm(V,2)


data <- read.csv("movie ratings by class.csv", stringsAsFactors = F, row.names = 1)
data["amol", "bahubali2"] <- 0
V <- as.matrix(data)
head(data,5)
class(data)
als_algorithm(V,2)



a <- matrix(runif(10000), nr = 100, nc = 100)
