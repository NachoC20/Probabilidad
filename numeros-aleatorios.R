
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
exponencial <- function(n, l){
  u = funcionUnif(n)
  b = c()
  for(i in 1:n){
    yi = (log(u[i]))/(-l)
    b = c(b, yi)
  }
  return(b)
}
#'Ejemplo:
#'Suponga que el tiempo en minutos que un usuario
#'cualquiera permanece revisando su correo electronico
#'sigue una distribución exponencial de parametro
#'lambda=1/5. Eso significa que el tiempo de conexión
#'promedio al servidor de correos es de (1/lambda)=5 min.
#'Calcule la probabilidad de que un usuario cualquiera
#'permanezca conectado al servidorde correo al 
#'menos de un minuto en diez observaciones.
e = exponencial(10, 0.2); e
##PROBABILIDAD=0.181


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
#'Ejemplo:
#'Un reloj de manecillas se detuvo en punto que no 
#'sabemos. Determine la probabilidad de que se haya 
#'detenido en los primeros 25 minutos, luego de senialar
#'la hora en punto.Se realizan 12 observaciones.
un = uniforme(12, 0, 60); un
## PROBABILIDAD=0.416

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
#'Ejemplo
#'Un examen tiene 10 preguntas y cada una tiene 3 opciones 
#'como respuesta, siendo solamente una de ellas la correcta.
#'Si un estudiante contesta al azar, cual es la probabilidad 
#'tenga más de 5 preguntas correctas. En 7 intentos.
x = binomial(7, 10, 0.33); x
##PROBABILIDAD=0.2334

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
#'en el conjunto {1,2,3,4,5}. ¿Cual es la probabilidad de
#'que el area del rectangulo de lados X y X-6 sea mayor o 
#'igual a 8? En 10 experimentos.
n = uniforme(10, 5); n
##PROBABIIDAD=0.6

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
#'Ejemplo:
#'En promedio se reciben 2 peticiones de acceso
#'a una pagina web durante un minuto cualquiera.
#'utilice el modelo Poisson para calcular la probabilidad
#'de que en un minuto dado cualquiera se reciban mas de 
#'dos peticiones.Se hacen 30 experimentos.
p = pois(30, 2); p
## PROBABILIDAD=0.323


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
    geometrica = (geo(r,1-p)-1) ##Requiere una geometrica por errores, no por ensayos
    suma = sum(geometrica)
    bin = c(bin, suma)
  }
  return (bin)
}
#'Ejemplo
#'Se lanzan repetidas veces una moneda equilibrada y los
#'dos resultados son cara o cruz ¿Cual es la proabilidad de 
#'obtener la tercera cruz en el quinto lanzamiento?.
#'Se hacen 10 experimentos
bn = binNeg(10, 3, .5); bn
## PROBABILIDAD=0.1875


