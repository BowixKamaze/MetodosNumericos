
#Funci?n a aplicar el algoritmo:
PolinomioS <- function(x) {
  f <- exp(x) + 2 ^ (-x) + 2 * cos(x) - 6
  return(f)
}

#Algoritmo de Secante
Raiz_Secante <- function(p0,p1,TOL,N,f) {
  #Paso 1
  i <- 2
  q0 <- f(p0)
  q1 <- f(p1)
  #Paso 2
  while (i<=N) {
    #Paso 3
    p <- p1-(q1*(p1-p0))/(q1-q0)
    #Paso 4
    if (abs(p-p1) < TOL) {
      return(p)
    }
    #Paso 5
    i <- i+1
    #Paso 6
    p0 <- p1
    q0 <- q1
    p1 <- p
    q1 <- f(p)
  }
  return(paste('El m?todo fall? luego de ' & N & ' iteraciones.'))
}
