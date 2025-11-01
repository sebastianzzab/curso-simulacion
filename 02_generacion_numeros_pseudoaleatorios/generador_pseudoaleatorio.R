############################################################
# Función para generar números pseudoaleatorios usando el método congruencial multiplicativo
#############################################################

rmcm <- function(
    n, semilla = 2 ^ 31 -1, a = 7 ^ 5, m = 2^31 - 1
    ) {
  # n: cantidad de números a generar
  # semilla: valor inicial (x0 > 0)
  # a: constante multiplicativa (a > 0)
  # m: módulo (m > x0, m > a)
gç  # Vector para almacenar los números generados
  numeros <- numeric(n)
  x <- semilla
  
  for (i in 1:n) {
    x <- (a * x) %% m
    numeros[i] <- x / m  # Normalizado entre 0 y 1
  }
  
  return(numeros)
}

# Ejemplo de uso
n <- 10            # Cantidad de números
semilla <- 12345   # Semilla inicial
a <- 16807         # Constante multiplicativa (usada en MINSTD)
m <- 2147483647    # Módulo (2^31 - 1)

numeros <- congr_multip(n, semilla, a, m)
print(numeros)

############################################################
# Función para generar números pseudoaleatorios usando el método congruencial mixto
#############################################################
rmcmm <- function(n, semilla, a, c, m) {
  # n: cantidad de números a generar
  # semilla: valor inicial (X0)
  # a: constante multiplicativa
  # c: constante aditiva
  # m: módulo

  # Vector para almacenar los números generados
  numeros <- numeric(n)
  x <- semilla

  for (i in 1:n) {
    x <- (a * x + c) %% m
    numeros[i] <- x / m  # Normalizado entre 0 y 1
  }

  return(numeros)
}

# Ejemplo de uso
n <- 10              # Cantidad de números
semilla <- 123456    # Semilla inicial
a <- 1664525         # Constante multiplicativa (usada en glibc)
c <- 1013904223      # Constante aditiva
m <- 2 ^ 32            # Módulo (para sistemas de 32 o 64 bits)

numeros <- rmcmm(n = 10, semilla, a = 1664525, c, m = 2 ^ 32 - 1)
print(numeros)


############################################################
# Función para generar números pseudoaleatorios usando el método congruencial mixto
############################################################

generador_64bits <- function(n, semilla, a, c, m) {
  numeros <- numeric(n)
  x <- semilla
  for (i in 1:n) {
    x <- (a * x + c) %% m
    numeros[i] <- x / m
  }
  return(numeros)
}

# Parámetros de 64 bits
n <- 10
semilla <- 123456789123456789
a <- 6364136223846793005
c <- 1442695040888963407
m <- 2^64

numeros <- generador_64bits(n, semilla, a, c, m)
print(numeros)

############################################################
# Función para generar números pseudoaleatorios usando el método de cuadrados medios
#########################################################
cuadrados_medios <- function(n, semilla, k = 4) {
  # n: cantidad de números a generar
  # semilla: número inicial (debe tener 'k' cifras)
  # k: cantidad de cifras a mantener en cada paso
  
  if (n <= 0 || k <= 0)
    stop("n y k deben ser positivos")
  if (nchar(semilla) != k)
    stop("La semilla debe tener el número correcto de dígitos")
  
  numeros <- numeric(n)
  x <- as.integer(semilla)
  
  for (i in 1:n) {
    cuadrado <- x^2
    cuadrado_str <- sprintf(paste0("%0", 2 * k, "d"), cuadrado)
    inicio <- floor((nchar(cuadrado_str) - k) / 2) + 1
    x_str <- substr(cuadrado_str, inicio, inicio + k - 1)
    x <- as.integer(x_str)
    numeros[i] <- x / (10^k)  # Normalizado entre 0 y 1
  }
  
  return(numeros)
}

# Ejemplo de uso
semilla <- 1234  # Debe tener 4 dígitos
n <- 10          # Cantidad de números a generar

k <- 4
numeros <- cuadrados_medios(n, semilla)
print(numeros)

############################################################# Prueba de Póker https://github.com/MarceParra/ModelosMatematicosChapter03/blob/main/07PruebaIndependencia_Poker.R
############################################################
# Datos proporcionados
r <- c(0.06141, 0.72484, 0.94107, 0.56766, 0.14411, 0.87648, 0.81792, 0.48999, 0.18590, 0.06060, 
       0.11223, 0.64794, 0.52953, 0.50502, 0.30444, 0.70688, 0.25357, 0.31555, 0.04127, 0.67347, 
       0.28103, 0.99367, 0.44598, 0.73997, 0.27813, 0.62182, 0.82578, 0.85623, 0.51483, 0.09099)

# Función para clasificar los números en manos de póker
categorize_poker <- function(numbers) {
  categories <- c("TD" = 0, "1P" = 0, "2P" = 0, "T" = 0, "TP" = 0, "P" = 0, "Q" = 0)
  for (num in numbers) {
    digits <- substr(num, 3, 7)  # Tomamos los 5 dígitos decimales
    digit_count <- table(strsplit(digits, NULL)[[1]])
    counts <- sort(digit_count, decreasing = TRUE)
    
    if (length(counts) == 5) {
      categories["TD"] <- categories["TD"] + 1
    } else if (length(counts) == 4) {
      categories["1P"] <- categories["1P"] + 1
    } else if (length(counts) == 3) {
      if (max(counts) == 3) {
        categories["T"] <- categories["T"] + 1
      } else {
        categories["2P"] <- categories["2P"] + 1
      }
    } else if (length(counts) == 2) {
      if (max(counts) == 4) {
        categories["P"] <- categories["P"] + 1
      } else {
        categories["TP"] <- categories["TP"] + 1
      }
    } else if (length(counts) == 1) {
      categories["Q"] <- categories["Q"] + 1
    }
  }
  return(categories)
}

# Calcular las frecuencias observadas
observed_frequencies <- categorize_poker(r)

# Frecuencias esperadas según las probabilidades proporcionadas
n <- length(r)
expected_frequencies <- c("TD" = 0.3024 * n, 
                          "1P" = 0.5040 * n, 
                          "2P" = 0.1080 * n, 
                          "TP" = 0.0090 * n, 
                          "T" = 0.0720 * n, 
                          "P" = 0.0045 * n, 
                          "Q" = 0.0001 * n)

# Calcular el estadístico chi-cuadrado
chi_squared <- sum((observed_frequencies - expected_frequencies)^2 / expected_frequencies)

# Grados de libertad
df <- length(expected_frequencies) - 1

# Valor crítico de chi-cuadrado para una confiabilidad del 95% y seis grados de libertad
critical_value <- qchisq(0.95, df)

# Valor p
p_value <- pchisq(chi_squared, df, lower.tail = FALSE)

# Mostrar resultados
print("Frecuencias observadas:")
print(observed_frequencies)

print("Frecuencias esperadas:")
print(expected_frequencies)

print(paste("Chi-cuadrado calculado:", chi_squared))
print(paste("Chi-cuadrado crítico (95% de confianza, 6 grados de libertad):", critical_value))

print(paste("Valor p:", p_value))

# Interpretar el resultado
if (p_value < 0.05) {
  print("Rechazamos la hipótesis nula: Los números no son independientes.")
} else {
  print("No podemos rechazar la hipótesis nula: Los números son independientes.")
}

############################################################# Prueba de Corrida https://github.com/MarceParra/ModelosMatematicosChapter03/blob/main/06PruebaIndependencia_CorridaMedias.R
############################################################

# Datos proporcionados
r <- c(0.809, 0.042, 0.432, 0.538, 0.225, 0.88, 0.688, 0.772, 0.036, 0.854, 0.397, 0.268, 0.821, 0.897, 0.07, 0.721, 0.087, 0.35, 0.779, 0.482, 0.136, 0.855, 0.453, 0.197, 0.444, 0.799, 0.809, 0.691, 0.545, 0.857, 0.692, 0.055, 0.348, 0.373, 0.436, 0.29, 0.015, 0.834, 0.599, 0.724, 0.564, 0.709, 0.946, 0.754, 0.677, 0.128, 0.012, 0.498, 0.6, 0.913)

# Número de datos
n <- length(r)
print(paste("Número de datos:", n))

# Media de referencia
media_ref <- 0.5

# Convertir los datos a 1s y 0s
secuencia <- ifelse(r > media_ref, 1, 0)
secuencia

# Calcular el número de corridas observadas (S)
C0 <- sum(diff(r > media_ref) != 0) + 1
C0

# Contar el número de ceros y unos
n0 <- sum(secuencia == 0)
n1 <- sum(secuencia == 1)

print(paste("Número de ceros (n0):", n0))
print(paste("Número de unos (n1):", n1))

# Calcular la media de la corrida
mediaC0=((2*n0*n1)/n)+0.5
print(paste("uC0:", mediaC0))

# Calcular la varianza de la corrida
varianzaC0=((2*n0*n1)*((2*n0*n1)-n))/((n^2)*(n-1))
print(paste("sigmaC0:", varianzaC0))

#Calcular el valor esperado de Z
valor_observado_Z = (C0-mediaC0)/(sqrt(varianzaC0))
valor_observado_Z

# Calcular el valor crítico
alpha <- 0.05  # Nivel de confianza del 95%
Z_critico_pos <- qnorm(1 - (alpha/2))
Z_critico_neg = (-1)*Z_critico_pos
print(paste("Zcritico negativo:", Z_critico_pos))
print(paste("Zcritico positivo:", Z_critico_neg))

# Imprimir conclusión
if ((valor_observado_Z > -Z_critico)&&(valor_observado_Z < Z_critico)) {
  cat("No se rechaza la hipótesis nula.\n")
} else {
  cat("Se rechaza la hipótesis nula.\n")
}

a <- 441
c <- 13
x0 <- 47
m <- 767

x1 <- (a + c*x0) %% m
x2 <- (a + c*x1) %% m

rmcmm <- function(n, a, c = 0, semilla, m) {
  numeros1 <- vector(length = n)
  numeros2 <- vector(length = n)
  x <- semilla
  for (i in 1:n) {
    x <- (a + c * x) %% m
    numeros1[i] <- x
    numeros2[i] <- x / (m - 1)
  }
  return(list(numeros1, numeros2))
}
