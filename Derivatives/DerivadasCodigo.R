rm(list=ls())  # removes all objects from the current workspace (R memory)
graphics.off()
x=c(1.62,1.87,2.12,2.37,2.62,2.87,3.12) # datos
fx=c(3.9612,4.1186,4.0617,3.8176,3.4211,2.9134,2.3404) # imagenes de los datos 

###### Comprobar que el h es igual para todas ####
distancia=c()
for(k in 1:(length(x)-1)){
  
  distancia[k] = -x[k] + x[k+1]
  
}

#distancia
h = distancia[1]

### En todas las fórmulas se supone que el vector de las imágenes fx estan ordenados concordando con las x y en donde el paso "h" es igual para todas. ####
##### 2 puntos #####
diferencia_progresiva<-function(x,fx,h){  # Diferencia Progresiva con h >0

fprima_x0=c()
p = c()
  #fprima_x0= (f(x0+h)- f(x0))/h
  for(i in 1:length(x)-1){
  fprima_x0[i] = (fx[i+1]-fx[i])/h 
  p[i] = x[i]
  }
  
tabla=cbind(p,fprima_x0)
  return(tabla)
  
  # Cota :  - h/2 * f II(e) 
}
diferencia_regresiva<-function(x,fx,h){
 
   #fprima_x0= (f(x0-h)- f(x0))/-h
  
  fprima_x0=c()
  p = c()
  for(i in 2:length(x)){
    fprima_x0[i-1] = (fx[i-1]-fx[i])/-h 
    p[i-1] = x[i]
  }
  
  tabla=cbind(p,fprima_x0)
  return(tabla)
  
}
##### 3 puntos #####
diferencia_3puntosextremaizq<-function(x,fx,h){
  derivada_x0=c()
  p = c()
  # Como los argumentos son equiespaciados 
  # h= x1- x0
  # h= (x2-x0)/2
  # derivada_x0 = (1/(2*h))*(-3*f(x0)+4*f(x0+h)-f(x0+2*h)) 
  # COTA : + ((h^2)/3) * f III (e)
  for(k in 1:(length(x)-2)){
  derivada_x0[k] = (1/(2*h))*(-3*fx[k]+4*fx[k+1]-fx[k+2])
  p[k] = x[k]
  }
  tabla = cbind(p,derivada_x0)
  return(tabla)  
  
}
diferencia_3puntosextremaderecha<-function(x,fx,h){
  derivada_x0=c()
  p = c()
  # Como los argumentos son equiespaciados 
  # h= x1- x0
  # h= (x2-x0)/2
  #derivada_x0 = (1/(2*h))*(f(x0-2*h)-4*f(x0-h)+3*f(x0)) 
  
  # COTA : + ((h^2)/3) * f III (e)
  for(k in 3:length(x)){
    derivada_x0[k-2] = (1/(2*h))*(fx[k-2]-4*fx[k-1]+3*fx[k]) 
    p[k-2] = x[k]
  }
  tabla = cbind(p,derivada_x0)
  return(tabla)  
  
}
diferencia_3puntosmedio<-function(x,fx,h){
  derivada_x0=c()
  p = c()
  # Como los argumentos son equiespaciados 
  # h= x1- x0
  # h= (x2-x0)/2
  # derivada_x0 = (1/(2*h))* (-f(x0-h)+f(x0+h))
  # COTA : + ((h^2)/3) * f III (e)
  for(k in 2:(length(x)-1)){
    derivada_x0[k-1] = (1/(2*h))*(-fx[k-1]+fx[k+1])
    p[k-1] = x[k]
  }
  tabla = cbind(p,derivada_x0)
  return(tabla)  
  
}

#####  5 puntos #####
diferencia_5puntosextremaizq<-function(x,fx,h){
  derivada_x0=c()
  p = c()
# derivada_x0=(1/(12*h))*(-25*f(x0)+48*f(x0+h)-36*f(x0+2*h)+16*f(x0+3*h)-3*f(x0+4*h))

  for(k in 1:(length(x)-4)){
    derivada_x0[k] = (1/(12*h))*(-25*fx[k]+48*fx[k+1]-36*fx[k+2]+16*fx[k+3]-3*fx[k+4])
    p[k] = x[k]
  }
  tabla = cbind(p,derivada_x0)
  return(tabla)  
  
}
diferencia_5puntosextremaderecha<-function(x,fx,h){
  derivada_x0=c()
  p = c()
#   derivada_x0=(1/(12*h))*(25*f(x0)-48*f(x0-h)+36*f(x0-2*h)-16*f(x0-3*h)+3*f(x0-4*h))
  
  for(k in 5:(length(x))){
    derivada_x0[k-4] = (1/(12*h))*(25*fx[k]-48*fx[k-1]+36*fx[k-2]-16*fx[k-3]+3*fx[k-4])
    p[k-4] = x[k]
  }
  tabla = cbind(p,derivada_x0)
  return(tabla)  
  
}
diferencia_5puntosmedio<-function(x,fx,h){  # esta es la mejor por que el error es más chico
  derivada_x0=c()
  p = c()
  # (1/(12*h)) * (f(x0-2*h)-8*f(x0-h)+8*f(x0+h)-f(x0+2*h))  
  
  for(k in 3:(length(x)-2)){
    derivada_x0[k-2] = (1/(12*h)) * (fx[k-2]-8*fx[k-1]+8*fx[k+1]-fx[k+2])  
    p[k-2] = x[k]
  }
  tabla = cbind(p,derivada_x0)
  return(tabla)  
  
}

###### Segunda derivada con 3 puntos #####
SegundaDerivada_3puntos<-function(x,fx,h){
  segundaderivada_x0=c()
  p = c()
#   segunda_derivada_x0=(1/(h^2))*(f(x0-h)-2*f(x0)+f(x0+h))
  for(k in 2:(length(x)-1)){
    segundaderivada_x0[k-1] = (1/(h^2))*(fx[k-1]-2*fx[k]+fx[k+1])
    p[k-1] = x[k]
  }
  tabla = cbind(p,segundaderivada_x0)
  return(tabla)  
  
}


## Tablas a ejecutar que hice para comprobarlas (prueba exitosa)  , # En todos los casos "p" hace referencia al punto "x" en cuestión.
diferencia_progresiva(x,fx,h)
diferencia_regresiva(x,fx,h)
diferencia_3puntosextremaizq(x,fx,h)
diferencia_3puntosextremaderecha(x,fx,h)
diferencia_3puntosmedio(x,fx,h)
diferencia_5puntosextremaizq(x,fx,h)
diferencia_5puntosextremaderecha(x,fx,h)
diferencia_5puntosmedio(x,fx,h)
SegundaDerivada_3puntos(x,fx,h)




