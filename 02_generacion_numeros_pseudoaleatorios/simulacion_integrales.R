#Aproximación de \int_{x = 0}^{x = \infin }e^{-x^2}\,dx = #\sqrt{\pi}

# ################# Método 1
set.seed(1357)
u <- runif(n = 1000) 
h <- exp(-(1 / u - 1) ^ 2) / u ^ 2
rm(u)
mean(h)
(error <- abs(sqrt(pi) / 2 - mean(h)))
2 * mean(h)
rm(h)

# Aplicacion del metodo 1 a la funcion gamma
# la funcion se tiene que aproximar de \int_{x = 0}^{x = \infin }x^(alpha-1)*exp(-x)\,dx = #\sqrt{\pi}
set.seed(1357)

# Inicializo los valores de los parámetros en 0
alpha <- 1/2

# Genero n = 1000 de una distribución uniforme
u <- runif(n = 100000) 

ejercicio <- replicate(n =100,
  expr = {
    alpha <- 1/2
     u <- runif(n = 10000)
     h <- (1/u - 1)^(alpha - 1)*exp(-1/u + 1) / u ^ 2
    rm(u)
    mean(h)
  }
)

mean(ejercicio)

h <- u^(alpha - 1)*exp(-u)
rm(u)
mean(h)
(error <- abs(mean(h)-sqrt(pi)))

####################Método 2
library(magrittr)
set.seed(1357)
runif(n = 1000) %>% 
  {exp(-(1 / . - 1) ^ 2) / . ^ 2} %>% 
  {2 * mean(.)}

#################### Método 3
set.seed(1357)
runif(n = 1000) |>
  (function(u) exp(-(1 / u - 1) ^ 2) / u ^ 2)() |>
  mean() |>
  (function(x) x * 2)()  # Multiplicar el resultado por 2 usando una función anónima

#################### Método 4
set.seed(1357)
tibble::tibble(
  u = runif(n = 1000)
) |> 
  dplyr::transmute(h = exp(-(1 / u - 1) ^ 2) / u ^ 2) |>
  dplyr::pull() |> 
  (function(x) mean(x) * 2)()

#################### Método 5
library(data.table)
set.seed(1357)
runif(n = 1000) |>
  (function(u) {
    dt <- data.table(u = u) 
    dt[, h := exp(-(1 / u - 1) ^ 2) / u ^ 2]
    mean(dt$h) * 2
    })()

library("microbenchmark")
microbenchmark(
  expr1 = {
     # Código a evaluar 
    set.seed(1357)
    u <- runif(n = 1000) 
    h <- exp(-(1 / u - 1) ^ 2) / u ^ 2
    rm(u)
    2 * mean(h)
    rm(h)
  },
  expr2 = {
     # Otro código a evaluar
    library(magrittr)
    set.seed(1357)
    runif(n = 1000) %>% 
      {exp(-(1 / . - 1) ^ 2) / . ^ 2} %>% 
      {2 * mean(.)}
  },
  expr3 = {
     # Otro código a evaluar
    set.seed(1357)
    runif(n = 1000) |>
      (function(u) exp(-(1 / u - 1) ^ 2) / u ^ 2)() |>
      mean() |>
      (function(x) x * 2)()  # Multiplicar el resultado por 2 usando una función anónima
  },
  expr4 = {
     # Otro código a evaluar
    set.seed(1357)
    tibble::tibble(
      u = runif(n = 1000)
    ) |> 
      dplyr::transmute(h = exp(-(1 / u - 1) ^ 2) / u ^ 2) |>
      dplyr::pull() |> 
      (function(x) mean(x) * 2)()
  },
  expr5 = {
     # Otro código a evaluar
    library(data.table)
    set.seed(1357)
    runif(n = 1000) |>
      (function(u) {
        dt <- data.table(u = u) 
        dt[, h := exp(-(1 / u - 1) ^ 2) / u ^ 2]
        mean(dt$h) * 2
        })()
  }
)

library(bench)
bench::mark(
  {
     # Otro código a evaluar
    library(magrittr)
    set.seed(1357)
    runif(n = 1000) %>% 
      {exp(-(1 / . - 1) ^ 2) / . ^ 2} %>% 
      {2 * mean(.)}
  },
  {
     # Otro código a evaluar
    set.seed(1357)
    runif(n = 1000) |>
      (function(u) exp(-(1 / u - 1) ^ 2) / u ^ 2)() |>
      mean() |>
      (function(x) x * 2)()  # Multiplicar el resultado por 2 usando una función anónima
  }
)  
integrando <- function(x) {exp(-x^2)}
integrate(integrando, lower = 0, upper = Inf)

k <- 2000
estimacion <- numeric(length = k)
for (i in 1:k) {
  estimacion[i] <- runif(n = 2000) |>
  (function(u) exp(-(1 / u - 1) ^ 2) / u ^ 2)() |>
  mean() |>
  (function(x) x * 2)()
}
mean(estimacion)
paste0(
  "Error de estimación: ", 
  abs(sqrt(pi) - mean(estimacion))
)
rm(estimacion)


#######################################################
# Instalar y cargar los paquetes necesarios
# install.packages("furrr")
library(furrr)

# Configurar el plan de ejecución paralela
plan(multisession)  # Usa múltiples núcleos (ideal para Windows/macOS/Linux)

# Número de simulaciones
k <- 2000

# Función de estimación
simulacion <- function(i) {
  u <- runif(2000)
  mean(exp(-(1 / u - 1)^2) / u^2) * 2
}

# Ejecutar en paralelo con furrr
estimacion <- future_map_dbl(1:k, simulacion)

# Calcular la media
media_final <- mean(estimacion)
print(media_final)

# Calcular el error de estimación
error <- abs(sqrt(pi) - media_final)
mensaje <- paste0("Error de estimación: ", error)
print(mensaje)

############################################################
library(furrr)
plan(multisession)

k <- 2000

simulacion <- function(i) {
  u <- runif(2000)
  mean(exp(-(1 / u - 1)^2) / u^2) * 2
}

# Corrección: añadir seed = TRUE
estimacion <- future_map_dbl(1:k, simulacion, .options = furrr_options(seed = TRUE))

media_final <- mean(estimacion)
print(media_final)

error <- abs(sqrt(pi) - media_final)
mensaje <- paste0("Error de estimación: ", error)
print(mensaje)

est <- replicate(
  n = 100,
  {
    runif(n = 100) |> 
      mean()
  }
)

est <- replicate(n = 100, {
  runif(n = 100) |>
    mean()
}) |>
  mean()

est <- mean(replicate(n = 100, {
  runif(n = 100) |>
    mean()
}))

mean(est)
funcion <- function(x) {
  exp(-x^2)
}
integrate(funcion, lower = 0, upper = Inf)