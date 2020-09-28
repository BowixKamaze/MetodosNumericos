#x <- c(8.1, 8.3, 8.6, 8.7)
#y <- c(16.9446, 17.56492, 18.50515, 18.82091)

#x<-c(2.80,3.25,3.70,4.15,4.60)
#y<-c(7.1027,7.6343,8.0486,8.3721,8.2635)

x1<-c(1,1.3,1.6,1.9,2.2)
y1<-c(0.7651977,0.6200860,0.4554022,0.2818186,0.1103623)

polinomioNeville <- function(x, y, x0) {
  
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- ((x0 - x[j-i+1]) * q[j,i-1] - (x0 - x[j]) * q[j-1,i-1]) / (x[j] - x[j-i+1])
    }
  }
  
  res <- list('Valor Aproximado'=q[n,n], 'Tabla de iteraciones de Neville'=q)
  return(res)
}

polinomioNeville(x1,y1,1.5)
