# Week 10 - Stat 554 - Computational Statistics


## MONTE CARLO implementation of the integral phi (Antithetic variable)
mc.phi <- function(x, R = 10000, antithetic = TRUE){
  u <- runif(R/2)
  if(!antithetic){
    v <- runif(R/2)
  } else{
    v <- 1-u
  }
  u <- c(u,v)
  cdf <- numeric(length(x))
  for(i in 1:length(x)){
    g <- x[i] * exp(-(u*x[i])^2/2)
    cdf[i] <- mean(g)/sqrt(2*pi) + 0.5
  }
  cdf
}

# comparison of estimates
x <- seq(0.1, 2.5, length =5)
phi <- pnorm(x)
set.seed(123)
mc_1 <- mc.phi(x,antithetic = FALSE)
set.seed(123)
mc_2 <- mc.phi(x,anti = TRUE) # normal conditions
print(round(rbind(x, mc_1, mc_2, phi), 5))


# Approximate Reduction in Var estimation under both methods

m <- 1000
mc1<-mc2<-numeric(m)
x <- 1.95
for(i in 1:m){
  mc1[i] <- mc.phi(x, R= 1000, antithetic = FALSE)
  mc2[i] <- mc.phi(x, R= 1000)
}
print(sd(mc1))
print(sd(mc2))
print((var(mc1)- var(mc2))/var(mc1))


## Basically just by using half of the values from the uniform(0,1) and
## the other half from the negatively correlated 1-u. Which is obviously negatively correlated.