x0 <- 2
n <- 10000
l <- 10
a = 7^5
m = 2^(31)-1
funcionUnif <- function(n){
  a = 7^5
  m = 2^(31)-1
  u = c()
  x0 = 2
  for(i in 1:n){
  
    xi = (a*x0)%%m
    ui = xi/m
    u = c(u, ui)
    x0 = xi
  
  }
  return(u)
}
##Exponencial
exponencial <- function(n, l){
  u = funcionUnif(n)
  b = c()
  for(i in 1:n){
    yi = (log(u[i]))/(-l)
    b = c(b, yi)
  }
  return(b)
}
e = exponencial(1000, 10); e

##Uniforme continua
uniforme <-function(n, a1, b1){
  u = funcionUnif(n)
  b = c()
  for (i in 1:n) {
    j = u[i]*(b1-a1)+a1
    b = c(b, j)
  }
  return(b)
}
un = uniforme(1000, 1, 5); un


##Bernoulli
bernoulli <-function(n, p){
  u = funcionUnif(n)
  b = c()
  for(i in 1:n){
    if(u[i] <= p){
      b = c(b,1)}
    else{
      b = c(b,0)}
  }
  return(b)
}
b = bernoulli(1000, .7); b

##Binomial
binomial <- function(m, n, p){
  u = funcionUnif(m)
  b = c()
  for(j in 1:m){
    unif = u[j]
    c = p/(1-p)
    pr = (1-p)^n
    dis = pr
    i = 0
    bool = TRUE
    while (!(unif < dis)) {
      prob = c*((n-i)/(i+1))*pr
      pr = prob
      dis = dis + pr
      i = i + 1
    }
    b = c(b,i+1)
  }
  return(b)
}

x = binomial(1000, 10, .3); x
 
##Uniforme
uniforme <- function(m, r){
  u = funcionUnif(m)
  b = c()
  for(j in 1:m){
    unif = u[j]
    i = 1
    while(!(((i-1)/r)<= unif  && unif < (i/r))){
      i = i + 1
    }
    b = c(b, i)
  }
  return(b)
}
n = uniforme(1000, 10); n

##Poisson
pois <- function(m, l){
  u = funcionUnif(m)
  b = c()
  for(j in 1:m){
    unif = u[j]
    i = 0
    p = exp(-l)
    dis = p
    while(!(unif < dis)){
      prob = p
      p = l*p/(i+1)
      dis = dis + p
      i = i + 1
    }
    b = c(b, i+1)
  }
  return(b)
}

p = pois(1000, 5); p
##(list = ls())
