# Método de inversión

# Ejemplo 4.1 (simulación de una distribución exponencial)

tini <- proc.time()
lambda <- 2
nsim <- 10^5
set.seed(1)
u <- runif(nsim)
x <--log(u)/lambda #-log(1-u)/lambda

tiempo <- proc.time()- tini
tiempo

hist(x, breaks = "FD", freq = FALSE,
     main = "", xlim = c(0, 5), ylim = c(0, 2.5))
# lines(density(x))
curve(dexp(x, lambda), col = "blue", add = TRUE)

# Ejemplo 4.2 (simulación de una distribución dobleexponencial)

ddexp <-function(x, lambda= 1){
  #Densidaddobleexponencial
  lambda*exp(-lambda*abs(x))/2
}

rdexp <-function(lambda = 1){
  #Simulaciónporinversióndedobleexponencial
  u <-runif(1)
  if (u<0.5) {
    return(log(2*u)/lambda)
  } else {
    return(-log(2*(1-u))/lambda)
  }
}
rdexpn <- function(n = 1000, lambda = 1) {
  # Simulación n valores de doble exponencial
  x <- numeric(n)
  for(i in 1:n) x[i]<-rdexp(lambda)
  return(x)
}

set.seed(1)
system.time(x <- rdexpn(10^4, 2))

hist(x, breaks = "FD", freq = FALSE, main = "")
lines(density(x))
curve(ddexp(x, 2), col = "blue", add = TRUE)
