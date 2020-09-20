rm(list= ls())
graphics.off()
x<-c(1,2,3,6,9,12) # vector original
precios<-c(99.8792,99.7503,99.6209,99.1908,98.7826,98.3724)
#tasas<-c(1.4517,1.5018,1.5221,1.6316,1.6432,1.6545)
variablesinterpoladas=c(4,5,7,8,10,11)
#vc=variablesinterpoladas

TrazadorCubicoNatural<-function(x,a,vc){
  #vc son los valores que quiero interpolar (calcular)
  # x es el valor de los vectores (original) con los que armo la función de interpolación
  # a es la imagen de las x 
  
  n = length(x) - 1
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
  ############### ULTIMO AGREGADO 5/07/2020
  b[n+1] = 0
  c[n+1] = 0
  d[n+1] = 0
  ############### HASTA ACA LO QUE AGREGUE EL 5/07/2020
  #PASO 7
  final<-cbind(a,b,c,d)
  
  #######################
  matriz=final
  ecuacion<-list()
  
  for(i in 1:n){
    
    ecuacion[[i]] =  paste(matriz[i,1]," + ",matriz[i,2],"*(x-",x[i],")"," + ",matriz[i,3],"*(x-",x[i],")^2"," + ",matriz[i,4],"*(x-",x[i],")^3"," Para x [",x[i],",",x[i+1],"]",sep='')
  }
  
  
  
  #######################################################################################################################3
#x<-c(1,2,3,6,9,12) # vector original
m= length(x)
  R=c()
  for(k in 1:length(vc)){
    for(q in 1:(m-1)){
      
      if(q<(m-1)){  
          if( vc[k] >= x[q] &  vc[k]<x[q+1]){
            
            
            R[k] = matriz[q,1] + matriz[q,2] *(vc[k]- x[q] ) + matriz[q,3] *(vc[k]- x[q] )^2 + matriz[q,4] *(vc[k]- x[q] )^3
            
          }
        }
      else if( vc[k] >= x[q] &  vc[k]<= x[q+1]){
            
            R[k] = matriz[q,1] + matriz[q,2] *(vc[k]- x[q] ) + matriz[q,3] *(vc[k]- x[q] )^2 + matriz[q,4] *(vc[k]- x[q] )^3
          }
    }}

    
  
  
  refinal = list('Funciones'=ecuacion,'Valores interpolados'=R)  
  # R son las variables interpoladas que se calcula con Slpnes
  # ecuacion es la expresión en ecuación
  return(refinal)
} 

l=TrazadorCubicoNatural(x,precios,variablesinterpoladas)
l
