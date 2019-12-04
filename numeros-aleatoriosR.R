funcionUnif <- function(n){
  a = 7^5
  m = 2^(31)-1
  u = c()
  x0 = floor(runif(1, min = 1, max = 200))
  xi = (a*x0)%%m
  x0 = xi
  for(i in 1:n){
    xi = (a*x0)%%m
    ui = xi/m
    u = c(u, ui)
    x0 = xi
  }
  return(u)
}

prueba = funcionUnif(10); prueba

##Exponencial
exponencial <- function(n, lamb){
  u = funcionUnif(n)
  b = c()
  for(i in 1:n){
    yi = (log(u[i]))/(-lamb)
    b = c(b, yi)
  }
  return(b)
}
#'Ejemplo:
#'Suponga que el tiempo en minutos que un usuario
#'cualquiera permanece revisando su correo electronico
#'sigue una distribucion exponencial de parametro
#'lambda=1/5. Eso significa que el tiempo de conexion
#'promedio al servidor de correos es de (1/lambda)=5 min.
#'Dicho evento ocurre con una probabilidad igual a 0.181.
#'¿Cuantas veces ocurrira en 10 observaciones? 
e = exponencial(10, 0.2); e

##Uniforme continua
uniforme <-function(n, inicioInt, finalInt){ 
  u = funcionUnif(n)
  b = c()
  for (i in 1:n) {
    j = u[i]*(finalInt-inicioInt)+inicioInt
    b = c(b, j)
  }
  return(b)
}
#'Ejemplo:
#'Un reloj de manecillas se detuvo en punto que no 
#'sabemos. La probabilidad de que se haya 
#'detenido en los primeros 25 minutos luego de senialar
#'la hora en punto es de 0.416. 
#'¿Cuantas veces se detendra en este intervalo en 12 observaciones?
un = uniforme(12, 0, 60); un

##Bernoulli
bernoulli <-function(n, prob){
  u = funcionUnif(n)
  b = c()
  for(i in 1:n){
    if(u[i] <= prob){
      b = c(b,1)}
    else{
      b = c(b,0)}
  }
  return(b)
}
b = bernoulli(100, .5); b

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
    while (dis <= unif) {
      prob = c*((n-i)/(i+1))*pr
      pr = prob
      dis = dis + prob
      i = i + 1
    }
    b = c(b,i)
  }
  return(b)
}
#'Ejemplo
#'Un examen tiene 10 preguntas y cada una tiene 3 opciones 
#'como respuesta, siendo solamente una de ellas la correcta.
#'Si un estudiante contesta al azar, la probabilidad de que
#'tenga mas de 5 preguntas correctas es de 0.2334.
#'¿Cuantas veces ocurrira este evento en 7 observaciones?
x = binomial(7, 10, 0.33); x

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
#'Ejemplo
#'Sea X una variable aleatoria con distribucion uniforme
#'en el conjunto {1,2,3,4,5}. La probabilidad de
#'que el area del rectangulo de lados X y 6-X sea mayor o 
#'igual a 8 es de 0.6 ¿Cuantas veces ocurrira esto 
#'en 10 experimentos? 
n = uniforme(10, 5); n
## Area del rectangulo
m = n*(6-n); m

##Poisson
pois <- function(m, lamb){
  u = funcionUnif(m)
  b = c()
  for(j in 1:m){
    unif = u[j]
    i = 0
    p = exp(-lamb)
    dis = p
    while(dis <= unif){
      prob = p
      p = lamb*p/(i+1)
      dis = dis + p
      i = i + 1
    }
    b = c(b, i+1)
  }
  return(b)
}
#'Ejemplo:
#'En promedio se reciben 2 peticiones de acceso
#'a una pagina web durante un minuto cualquiera.
#'Utilizando el modelo Poisson se calculo la 
#'probabilidad de que en un minuto cualquiera dado
#'se reciban mas de dos peticiones, es de 0.323
#'¿cuantas veces ocurrira en 30 experimentos?
p = pois(30, 2); p



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


##Binomial negativa
binNeg <- function(n, r, p){
  bin = c()
  for(i in 1:n){
    ##Requiere una geometrica por errores, no por ensayos
    geometrica = (geo(r,1-p)-1) 
    suma = sum(geometrica)
    bin = c(bin, suma)
  }
  return (bin)
}
#'Ejemplo
#'Se lanzan repetidas veces una moneda equilibrada y los
#'dos resultados son cara o cruz; la proabilidad de 
#'obtener la tercera cruz en el quinto lanzamiento es
#'de 0.1875 ¿Cuantas veces ocurrira en 10 lanzamientos?
bn = binNeg(10, 3, .5); bn



