rm(list= ls())
graphics.off()
x<-c(5.20,5.63,6.05,6.48,6.90)
a<-c(10.7171,12.2979,13.6469,14.7015,15.4607)
# length(X)  # para ver cuantos datos hay
#NOTA : n siempre será la cantidad de datos -1 
# si tengo 8 datos >> n= 7
# --------------------------------------------
TrazadorCubicoNatural<-function(n,x,a){
  
h<-c()
alfa<-c()
b<-c()
c<-c()
d<-c()
l<-c()
z<-c()
u<-c()

  #PASO 1
  for (i in 1:n){
    
    h[i] =  x[i+1] - x[i]
  }

  #PASO 2  
    for(i in 2:n){
      
    alfa[i] = (3/h[i]) * (a[i+1]- a[i]) - (3/h[i-1])* (a[i]-a[i-1])
  }
  
  # PASO 3
  l[1] = 1
  u[1] = 0
  z[1] = 0
  
  # PASO 4
  
  for(i in 2:n){
    
    l[i] = 2*(x[i+1]- x[i-1]) - h[i-1]* u[i-1]
    u[i] = h[i] / l[i]
    z[i] = (alfa[i]- h[i-1]*z[i-1])/ l[i]
  }
  # PASO 5
  l[n+1] = 1
  z[n+1] = 0
  c[n+1] = 0
    
  # PASO 6
  for(j in n:1){
      
      c[j] = z[j]- u[j]*c[j+1]
      b[j] = ((a[j+1] - a [j]) / h[j]) -  h[j]*(c[j+1]+ 2*c[j])/3
      d[j] = (c[j+1]- c[j]) / (3*h[j]) 
  }
  
  #PASO 7
  b[n+1] = 0
  c[n+1] = 0
  d[n+1] = 0
final<-cbind(a,b,c,d)
  return(final)
}


polinomio<-function(n,x,a){
  ecuacion<-list()
  matriz=TrazadorCubicoNatural(n,x,a)
  for(i in 1:n){
    
    ecuacion[[i]] =  paste(matriz[i,1],"+",matriz[i,2],"*(x-",x[i],")","+",matriz[i,3],"*(x-",x[i],")^2","+",matriz[i,4],"*(x-",x[i],")^3")
  }
  return(ecuacion)
}
resultado_trazador=TrazadorCubicoNatural(4,x,a)  
prueba=polinomio(4,x,a)
resultado_trazador
prueba

# NOTA  : PARA GRAFICAR PUEDO UTILIZAR  :
# Plot(x,a , type"o")   << Grafica puntos con una linea atravesada por ellos.
