# Metodo de aceptacion y rechazo: Ejercicio propuesto
r_densidad <- function(n) {
  c <- 2.25
  aceptados <- numeric(0)
  intentos_totales <- 0
  
  # Generamos por bloques para aprovechar la vectorización de R
  while(length(aceptados) < n) {
    faltan <- n - length(aceptados)
    # Estimamos cuantos generar para no quedarnos cortos (margen de seguridad de 20%)
    n_sim <- ceiling(faltan * c * 1.2) 
    intentos_totales <- intentos_totales + n_sim
    
    U1 <- runif(n_sim)
    U2 <- runif(n_sim)
    Y <- 2 * U2 # Variable candidata (cambié T por Y)
    
    # Vector lógico de aceptación
    condicion <- (18 * U1) <= (3*Y^2 + 2*Y + 2)
    
    # Guardamos solo los que cumplen
    aceptados <- c(aceptados, Y[condicion])
  }
  
  # Recortamos al tamaño exacto n
  resultado <- aceptados[1:n]
  attr(resultado, "intentos") <- intentos_totales
  return(resultado)
}

# Generar muestra
set.seed(123)
n <- 10000
resultados <- r_densidad(n)
intentos <- attr(resultados, "intentos")

# Eficiencia
cat("Número de valores generados:", n, "\n")
cat("Número de intentos totales:", intentos, "\n")
cat("Proporción de aceptación empírica:", n / intentos, "\n")
cat("Proporción teórica (1/c):", 1/2.25, "\n")
cat("Número medio de intentos por valor generado:", intentos / n, "\n")
cat("Número medio teórico (c):", 2.25, "\n")

# Comparar con densidad teórica
hist(resultados, breaks = 30, freq = FALSE, main = "Distribución simulada vs teórica",
     xlab = "x", ylim = c(0, 1.2))
curve(f, from = 0, to = 2, add = TRUE, col = "blue", lwd = 2)
legend("topleft", legend = c("Simulación", "Teórica"), 
       col = c("black", "blue"), lty = c(1, 1), lwd = c(1, 2))

# Test de bondad de ajuste (Kolmogorov-Smirnov)
ks_test <- ks.test(resultados, function(x) {
  ifelse(x < 0, 0, ifelse(x > 2, 1, (x^3 + x^2 + 2*x)/16))
})
cat("\nTest de Kolmogorov-Smirnov:\n")
cat("Estadístico D =", ks_test$statistic, "\n")
cat("p-valor =", ks_test$p.value, "\n")