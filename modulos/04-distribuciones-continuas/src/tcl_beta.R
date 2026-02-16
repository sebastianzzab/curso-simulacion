# 1. Generar 100,000 observaciones de una distribución Beta con parámetros 0.5 y 1.5
n_observations <- 100000
beta_population <- rbeta(n_observations, shape1 = 0.5, shape2 = 1.5)

# Opcional: Verificar las primeras observaciones y un resumen
# head(beta_population)
# summary(beta_population)
# hist(beta_population, main = "Histograma de la Población Beta", xlab = "Valor", freq = FALSE)
# lines(density(beta_population), col = "blue", lwd = 2)

# 2. Configurar los parámetros para las muestras
n_samples <- 1000 # Número de muestras a extraer
sample_size <- 50 # Tamaño de cada muestra (puedes ajustarlo según sea necesario)
# Es importante que el tamaño de la muestra sea lo suficientemente grande
# para observar el Teorema del Límite Central.

# 3. Almacenar las medias muestrales
sample_means <- numeric(n_samples)

# Extraer muestras y calcular sus medias
for (i in 1:n_samples) {
  # Extraer una muestra aleatoria con reemplazo de la población
  # El 'replace = TRUE' es común en el muestreo para bootstrap o para simular
  # poblaciones infinitas, pero para una población finita grande, 'replace = FALSE'
  # también podría ser adecuado si no se desea repetir las mismas observaciones.
  # Aquí usamos TRUE para mantener la independencia de las muestras.
  current_sample <- sample(beta_population, size = sample_size, replace = TRUE)
  sample_means[i] <- mean(current_sample)
}

# Opcional: Verificar las primeras medias muestrales y un resumen
# head(sample_means)
# summary(sample_means)

# 4. Generar un histograma de las medias muestrales
hist(sample_means,
     main = paste("Histograma de las Medias Muestrales (", n_samples, " muestras de tamaño ", sample_size, ")"),
     xlab = "Media Muestral",
     border = "white",
     col = "skyblue",
     freq = FALSE # Usar densidades en lugar de frecuencias absolutas para comparar con la curva normal
)

# Opcional: Superponer una curva de densidad normal para ilustrar el Teorema del Límite Central
# La media de las medias muestrales debe ser cercana a la media de la población
mean_of_population <- mean(beta_population)
# La desviación estándar de las medias muestrales (error estándar)
# debe ser sd(poblacion) / sqrt(sample_size)
sd_of_sample_means <- sd(beta_population) / sqrt(sample_size)

curve(dnorm(x, mean = mean_of_population, sd = sd_of_sample_means),
      col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Histograma de Medias", "Densidad Normal Teórica"),
       fill = c("skyblue", NA), border = c("white", NA), col = c(NA, "red"), lty = c(NA, 1), lwd = c(NA, 2))

# Nota: Observarás que, gracias al Teorema del Límite Central, la distribución de las medias muestrales
# tiende a ser normal, incluso si la población original (Beta) no lo es.