# Función para generar números pseudoaleatorios usando el método congruencial mixto
congr_multip <- function(n, semilla, a, c, m) {
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

# Ejercicios Sheldon Ross

ejercicio_1 <- congr_multip(n =10, semilla=5, a=3, c=0,m=150)
print(ejercicio_1) #0.1 0.3 0.9 0.7 0.1 0.3 0.9 0.7 0.1 0.3

ejercicio_2 <- congr_multip(n =10, semilla=3, a=5, c=7,m=200)
print(ejercicio_2) #0.110 0.585 0.960 0.835 0.210 0.085 0.460 0.335 0.710 0.585


#ejercicio_3 <- replicate(
#  n=100,{
#    
#  }
#  u <- runif(100)
#  h <- exp(e^u)
#  (mean(h))
#)  

#ejercicio_3 <- function(x) ifelse((x> 0) & (x < 1), exp(exp(x)),0)
#curve(ejercicio_3, 0,1)
