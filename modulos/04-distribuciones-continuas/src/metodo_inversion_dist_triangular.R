# ===================== CONFIGURACIÓN DE PARÁMETROS =====================
# Parámetros de la distribución triangular según el ejemplo
a <- 2   # Límite inferior (puedes cambiarlo)
b <- 5   # Moda (punto donde cambia la función, puede cambiarse)
c <- 10  # Límite superior (puedes cambiarlo)

# Parámetros de simulación
n <- 100     # Tamaño de cada muestra
N <- 100     # Número de experimentos

# Verificar que a < b < c
if(!(a < b & b < c)) {
  stop("Los parámetros deben cumplir: a < b < c")
}

# ===================== IMPLEMENTACIÓN DEL MÉTODO DE ACEPTACIÓN-RECHAZO =====================
# Función para generar una muestra de tamaño n usando el método del ejemplo
generar_triangular <- function(n, a, b, c) {
  muestras <- numeric(n)
  aceptadas <- 0
  
  while(aceptadas < n) {
    # Paso 1: Generar R1 y R2
    R1 <- runif(1)
    R2 <- runif(1)
    
    # Paso 2: Calcular x
    x <- a + (c - a) * R1
    
    # Paso 3: Determinar f(x) según la región
    if(x < b) {
      # Para a ≤ x ≤ b
      fx <- 2 * R1 / (b - a)
    } else {
      # Para b ≤ x ≤ c
      fx <- 2 * (1 - R1) / (c - b)
    }
    
    # Paso 4: Criterio de aceptación
    # M = 2/(c-a) según el ejemplo
    M <- 2 / (c - a)
    
    if(R2 < fx / M) {
      aceptadas <- aceptadas + 1
      muestras[aceptadas] <- x
    }
  }
  
  return(muestras)
}

# ===================== SIMULACIÓN DE 100 EXPERIMENTOS =====================
cat("Simulando 100 experimentos con 100 muestras cada uno...\n")
resultados <- matrix(NA, nrow = N, ncol = n)

for(i in 1:N) {
  resultados[i, ] <- generar_triangular(n, a, b, c)
  if(i %% 10 == 0) cat("Completado:", i, "de", N, "experimentos\n")
}

cat("¡Simulación completada!\n\n")

# ===================== FUNCIONES AUXILIARES PARA PRUEBAS =====================
# Función de densidad triangular teórica
dtriangular_teorica <- function(x, a, b, c) {
  ifelse(x >= a & x <= b,
         2 * (x - a) / ((c - a) * (b - a)),
         ifelse(x > b & x <= c,
                2 * (c - x) / ((c - a) * (c - b)),
                0))
}

# Función de distribución acumulada triangular teórica
ptriangular_teorica <- function(x, a, b, c) {
  ifelse(x < a, 0,
         ifelse(x <= b,
                (x - a)^2 / ((c - a) * (b - a)),
                ifelse(x <= c,
                       1 - (c - x)^2 / ((c - a) * (c - b)),
                       1)))
}

# ===================== PRUEBAS ESTADÍSTICAS =====================

# 1. PRUEBA DE PROMEDIOS (sobre las 100 medias)
cat("=== PRUEBA DE PROMEDIOS ===\n")
medias_experimentos <- apply(resultados, 1, mean)

# Media teórica de la distribución triangular
media_teorica <- (a + b + c) / 3
media_simulada <- mean(medias_experimentos)
desviacion_medias <- sd(medias_experimentos)

cat("Media teórica:", round(media_teorica, 4), "\n")
cat("Media de las medias muestrales:", round(media_simulada, 4), "\n")
cat("Desviación estándar de las medias:", round(desviacion_medias, 4), "\n")

# Intervalo de confianza del 95% para la media
error_estandar <- desviacion_medias / sqrt(N)
lim_inf <- media_simulada - 1.96 * error_estandar
lim_sup <- media_simulada + 1.96 * error_estandar

cat("Intervalo de confianza al 95% para la media: [", 
    round(lim_inf, 4), ",", round(lim_sup, 4), "]\n")

# Prueba de hipótesis: ¿La media teórica está en el intervalo?
cat("¿La media teórica está en el intervalo?", 
    media_teorica >= lim_inf & media_teorica <= lim_sup, "\n\n")

# 2. PRUEBA DE FRECUENCIAS (Chi-cuadrado) - Primer experimento
cat("=== PRUEBA DE FRECUENCIAS (Chi-cuadrado) ===\n")
muestra_1 <- resultados[1, ]

# Crear intervalos equiprobables
k <- 10  # Número de intervalos
prob_intervalo <- 1/k

# Calcular cuantiles teóricos para los intervalos
cuantiles_teoricos <- qtriangular_teorica(seq(0, 1, length.out = k+1), a, b, c)

# Contar frecuencias observadas
frec_obs <- hist(muestra_1, breaks = cuantiles_teoricos, plot = FALSE)$counts
frec_esp <- rep(n/k, k)

# Calcular estadístico chi-cuadrado
chi2 <- sum((frec_obs - frec_esp)^2 / frec_esp)
gl <- k - 1
p_valor_chi2 <- 1 - pchisq(chi2, df = gl)

cat("Frecuencias observadas:", frec_obs, "\n")
cat("Frecuencias esperadas:", round(frec_esp, 2), "\n")
cat("Estadístico Chi-cuadrado:", round(chi2, 4), "\n")
cat("Grados de libertad:", gl, "\n")
cat("p-valor:", round(p_valor_chi2, 4), "\n")
cat("¿Se acepta uniformidad (α=0.05)?", p_valor_chi2 > 0.05, "\n\n")

# 3. PRUEBA DE KOLMOGOROV-SMIRNOV - Primer experimento
cat("=== PRUEBA DE KOLMOGOROV-SMIRNOV ===\n")
# Función para la prueba KS manual
ks_test_manual <- function(muestra, dist_func, ...) {
  n <- length(muestra)
  muestra_ord <- sort(muestra)
  
  # Distribución empírica
  Fn <- (1:n) / n
  
  # Distribución teórica
  Ft <- dist_func(muestra_ord, ...)
  
  # Estadístico D
  D <- max(abs(Fn - Ft))
  
  # p-valor aproximado (prueba de Kolmogorov-Smirnov)
  p_valor <- exp(-2 * n * D^2)
  
  return(list(D = D, p_valor = p_valor))
}

ks_result <- ks_test_manual(muestra_1, ptriangular_teorica, a, b, c)
cat("Estadístico D:", round(ks_result$D, 4), "\n")
cat("p-valor (aproximado):", round(ks_result$p_valor, 4), "\n")
cat("¿Se ajusta a la distribución (α=0.05)?", ks_result$p_valor > 0.05, "\n\n")

# También usando la función ks.test de R (más precisa)
ks_test_R <- ks.test(muestra_1, ptriangular_teorica, a, b, c)
cat("Usando ks.test de R:\n")
cat("Estadístico D:", round(ks_test_R$statistic, 4), "\n")
cat("p-valor:", round(ks_test_R$p.value, 4), "\n")
cat("¿Se ajusta a la distribución (α=0.05)?", ks_test_R$p.value > 0.05, "\n\n")

# 4. PRUEBA DE BOX-PIERCE (para independencia) - Primer experimento
cat("=== PRUEBA DE BOX-PIERCE ===\n")
# Calcular autocorrelaciones
lag_max <- 20
acf_vals <- acf(muestra_1, lag.max = lag_max, plot = FALSE)$acf[-1]

# Estadístico Q de Box-Pierce
Q <- n * sum(acf_vals^2)
gl_box <- lag_max
p_valor_box <- 1 - pchisq(Q, df = gl_box)

cat("Estadístico Q de Box-Pierce:", round(Q, 4), "\n")
cat("Grados de libertad:", gl_box, "\n")
cat("p-valor:", round(p_valor_box, 4), "\n")
cat("¿Hay independencia (α=0.05)?", p_valor_box > 0.05, "\n\n")

# ===================== GRÁFICOS =====================
# Configurar ventana gráfica
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

# 1. Histograma vs densidad teórica (primer experimento)
hist(muestra_1, breaks = 20, freq = FALSE, 
     main = "Histograma vs Densidad Teórica",
     xlab = "x", ylab = "Densidad", col = "lightblue")
curve(dtriangular_teorica(x, a, b, c), from = a, to = c, 
      add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Densidad simulada", "Densidad teórica"),
       fill = c("lightblue", "red"), cex = 0.8)

# 2. Gráfico de autocorrelación
acf(muestra_1, main = "Gráfico de Autocorrelación", 
    lag.max = lag_max, ylab = "Autocorrelación")

# 3. Gráfico retardado (lag plot)
lag.plot(muestra_1, lag = 1, main = "Gráfico Retardado (lag=1)",
         xlab = "x_t", ylab = "x_{t+1}", 
         diag = TRUE, diag.col = "red")

# 4. QQ plot teórico
qq_teorico <- function(muestra, dist_func, ...) {
  n <- length(muestra)
  teorico <- dist_func((1:n - 0.5)/n, ...)
  empirico <- sort(muestra)
  plot(teorico, empirico, main = "QQ Plot vs Teórico",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
  abline(0, 1, col = "red")
}

# Función cuantil triangular inversa
qtriangular_teorica <- function(p, a, b, c) {
  ifelse(p < (b - a)/(c - a),
         a + sqrt(p * (c - a) * (b - a)),
         c - sqrt((1 - p) * (c - a) * (c - b)))
}

qq_teorico(muestra_1, qtriangular_teorica, a, b, c)

# 5. Distribución de medias de los experimentos
hist(medias_experimentos, breaks = 15, freq = FALSE,
     main = "Distribución de Medias Muestrales",
     xlab = "Media muestral", ylab = "Densidad", col = "lightgreen")
abline(v = media_teorica, col = "red", lwd = 2)
abline(v = media_simulada, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Media teórica", "Media simulada"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2, cex = 0.8)

# 6. Comparación de funciones de distribución acumulada
plot(ecdf(muestra_1), main = "FDA Empírica vs Teórica",
     xlab = "x", ylab = "Probabilidad acumulada")
curve(ptriangular_teorica(x, a, b, c), from = a, to = c, 
      add = TRUE, col = "red", lwd = 2)
legend("bottomright", legend = c("Empírica", "Teórica"),
       col = c("black", "red"), lty = c(1, 1), lwd = 2, cex = 0.8)

# ===================== RESUMEN ESTADÍSTICO =====================
cat("=== RESUMEN ESTADÍSTICO ===\n")
cat("Parámetros usados: a =", a, ", b =", b, ", c =", c, "\n")
cat("Media teórica:", round(media_teorica, 4), "\n")
cat("Varianza teórica:", round(((a^2 + b^2 + c^2) - (a*b + a*c + b*c))/18, 4), "\n")
cat("\nResultados de las pruebas para el primer experimento:\n")
cat("1. Prueba de promedios: Media =", round(mean(muestra_1), 4), "\n")
cat("2. Prueba Chi-cuadrado: p-valor =", round(p_valor_chi2, 4), 
    ifelse(p_valor_chi2 > 0.05, "(Acepta)", "(Rechaza)"), "\n")
cat("3. Prueba KS: p-valor =", round(ks_test_R$p.value, 4),
    ifelse(ks_test_R$p.value > 0.05, "(Acepta)", "(Rechaza)"), "\n")
cat("4. Prueba Box-Pierce: p-valor =", round(p_valor_box, 4),
    ifelse(p_valor_box > 0.05, "(Independencia)", "(Dependencia)"), "\n")

# Eficiencia del método de aceptación-rechazo
cat("\n=== EFICIENCIA DEL MÉTODO ===\n")
# Simular para estimar eficiencia
set.seed(123)
prueba_eficiencia <- generar_triangular(1000, a, b, c)
# La eficiencia teórica es 1/M = (c-a)/2
eficiencia_teorica <- (c - a) / 2
cat("Eficiencia teórica:", round(eficiencia_teorica, 4), "\n")
cat("(Proporción esperada de aceptación:", round(1/eficiencia_teorica, 4), ")\n")

# # ===================== CONFIGURACIÓN =====================
# # Parámetros de la distribución triangular
# a <- 0    # Límite inferior
# b <- 10   # Límite superior
# c <- 7    # Moda (debe estar entre a y b)
# 
# # Parámetros de simulación
# n <- 100  # Números a generar por simulación
# N <- 100  # Número de simulaciones
# 
# # Función de densidad triangular
# dtriangular <- function(x, a, b, c) {
#   ifelse(x >= a & x <= c,
#          2*(x - a)/((b - a)*(c - a)),
#          ifelse(x > c & x <= b,
#                 2*(b - x)/((b - a)*(b - c)),
#                 0))
# }
# 
# # ===================== MÉTODO DE ACEPTACIÓN-RECHAZO =====================
# simular_triangular <- function(n, a, b, c) {
#   # Envolvente: distribución uniforme
#   M <- 2/(b - a)  # Máximo de f(x)
#   valores <- numeric(n)
#   aceptados <- 0
#   
#   while(aceptados < n) {
#     # Generar candidato de la uniforme
#     x_candidato <- runif(1, a, b)
#     u <- runif(1)
#     
#     # Criterio de aceptación
#     if(u <= dtriangular(x_candidato, a, b, c)/M) {
#       aceptados <- aceptados + 1
#       valores[aceptados] <- x_candidato
#     }
#   }
#   return(valores)
# }
# 
# # ===================== SIMULACIÓN PRINCIPAL =====================
# resultados <- matrix(NA, nrow = N, ncol = n)
# for(i in 1:N) {
#   resultados[i,] <- simular_triangular(n, a, b, c)
# }
# 
# # ===================== PRUEBAS ESTADÍSTICAS =====================
# 
# # 1. PRUEBA DE PROMEDIOS
# medias <- apply(resultados, 1, mean)
# media_teorica <- (a + b + c)/3
# cat("=== PRUEBA DE PROMEDIOS ===\n")
# cat("Media teórica:", media_teorica, "\n")
# cat("Media de las medias muestrales:", mean(medias), "\n")
# cat("Desviación estándar de las medias:", sd(medias), "\n")
# 
# # Intervalo de confianza del 95%
# lim_inf <- media_teorica - 1.96*sd(medias)/sqrt(N)
# lim_sup <- media_teorica + 1.96*sd(medias)/sqrt(N)
# cat("Intervalo de confianza al 95%: [", lim_inf, ",", lim_sup, "]\n")
# cat("¿La media está en el intervalo?", media_teorica >= lim_inf & media_teorica <= lim_sup, "\n\n")
# 
# # 2. PRUEBA DE FRECUENCIAS (Chi-cuadrado)
# cat("=== PRUEBA DE FRECUENCIAS (Chi-cuadrado) ===\n")
# # Usar la primera simulación como ejemplo
# datos <- resultados[1,]
# k <- 10  # Número de intervalos
# intervalos <- seq(a, b, length.out = k+1)
# frec_obs <- hist(datos, breaks = intervalos, plot = FALSE)$counts
# 
# # Probabilidades teóricas para cada intervalo
# prob_teorica <- numeric(k)
# for(i in 1:k) {
#   prob_teorica[i] <- ptriangular(intervalos[i+1], a, b, c) - 
#     ptriangular(intervalos[i], a, b, c)
# }
# 
# frec_esp <- prob_teorica * n
# chi2 <- sum((frec_obs - frec_esp)^2 / frec_esp)
# p_valor <- 1 - pchisq(chi2, df = k-1)
# cat("Estadístico Chi-cuadrado:", chi2, "\n")
# cat("p-valor:", p_valor, "\n")
# cat("¿Distribución adecuada (α=0.05)?", p_valor > 0.05, "\n\n")
# 
# # 3. PRUEBA KOLMOGOROV-SMIRNOV
# cat("=== PRUEBA KOLMOGOROV-SMIRNOV ===\n")
# ks_test <- ks.test(datos, function(x) ptriangular(x, a, b, c))
# cat("Estadístico D:", ks_test$statistic, "\n")
# cat("p-valor:", ks_test$p.value, "\n")
# cat("¿Distribución adecuada (α=0.05)?", ks_test$p.value > 0.05, "\n\n")
# 
# # 4. PRUEBA DE BOX (Box-Pierce para autocorrelación)
# cat("=== PRUEBA DE BOX-PIERCE ===\n")
# # Usar la primera simulación
# serie <- datos
# lag_max <- 20
# box_test <- Box.test(serie, lag = lag_max, type = "Box-Pierce")
# cat("Estadístico Q:", box_test$statistic, "\n")
# cat("p-valor:", box_test$p.value, "\n")
# cat("¿Independencia (α=0.05)?", box_test$p.value > 0.05, "\n\n")
# 
# # ===================== GRÁFICOS =====================
# par(mfrow = c(2, 2))
# 
# # Histograma vs densidad teórica
# hist(datos, breaks = 20, freq = FALSE, main = "Histograma vs Densidad Teórica",
#      xlab = "Valor", ylab = "Densidad", col = "lightblue")
# curve(dtriangular(x, a, b, c), from = a, to = b, add = TRUE, col = "red", lwd = 2)
# 
# # Gráfico de autocorrelación
# acf(datos, main = "Gráfico de Autocorrelación", lag.max = 20)
# 
# # Gráfico retardado (lag plot)
# lag.plot(datos, lag = 1, main = "Gráfico Retardado (lag=1)", 
#          diag = FALSE, do.lines = FALSE)
# 
# # QQ plot teórico
# plot(sort(datos), ptriangular(sort(datos), a, b, c),
#      main = "QQ Plot vs Distribución Teórica",
#      xlab = "Cuantiles muestrales", ylab = "Cuantiles teóricos")
# abline(0, 1, col = "red")
# 
# # ===================== FUNCIÓN DE DISTRIBUCIÓN TRIANGULAR =====================
# ptriangular <- function(x, a, b, c) {
#   ifelse(x < a, 0,
#          ifelse(x <= c,
#                 (x - a)^2/((b - a)*(c - a)),
#                 ifelse(x <= b,
#                        1 - (b - x)^2/((b - a)*(b - c)),
#                        1)))
# }