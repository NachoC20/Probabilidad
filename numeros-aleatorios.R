
funcionUnif <- function(n){
  a = 7^5
  m = 2^(31)-1
  u = c()
  x0 = floor(runif(1, min = 1, max = 200))
  for(i in 1:n){
    xi = (a*x0)%%m
    ui = xi/m
    u = c(u, ui)
    x0 = xi
  }
  return(u)
}

prueba = funcionUnif(1000); prueba

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
e = exponencial(10, 5); e


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
un = uniforme(100, 10, 20); un

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
b = bernoulli(100, .9); b

##Binomial
binomial <- function(m, n, p){
  u = funcionUnif(m)
  b = c()
  for(j in 1:m){
    unif = u[j]
    c = p/(1-p)
    pr = (1-p)^(n)
    dis = pr
    i = 0
    while (!(unif < dis)) {
      prob = c*((n-i)/(i+1))*pr
      pr = prob
      dis = dis + pr
      i = i + 1
    }
    b = c(b,i)
  }
  return(b)
}

x = binomial(100, 10000, .3); x

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

p = pois(1000, 999); p
##(list = ls())

##Geometrica
geo <- function(n, p){
  u = funcionUnif(n)
  b = c()
  for(j in 1:n){
    unif = u[j]
    i = 1
    while(!((1-(1-p)^(i-1))<= unif  && unif < (1-(1-p)^(i)))){
      i = i + 1
    }
    b = c(b, i)
  }
  return (b)
}
g = geo(500, .3); g

##Binomial negativa
binNeg <- function(n, r, p){
  bin = c()
  for(i in 1:n){
    geometrica = (geo(r,1-p)-1) ##Requiere una geometrica por errores, no por ensayos
    suma = sum(geometrica)
    bin = c(bin, suma)
  }
  return (bin)
}

bn = binNeg(1000, 500, .5); bn



