rm(list=ls())  # removes all objects from the current workspace (R memory)
graphics.off()
# El método de Euler lo que hace es construir una red de puntos o varios puntos de red y generar aproximaciones en dichos puntos
# lo que se hace de alguna manera es discretizar la ecuacion diferencial.
# Entonces lo que se hace es, se selecciona un nro entero "n" positivo y se construye los puntos de red de manera tal que, lo que se hace
# es generar un set de puntos x-espaciados, desde el valor "a" hasta el valor "b".

# Tmaño de paso : h = (b-a)/N
# Los "puntos de red" son llamdos "mesh points"

# Una vez constuidos los puntos de red, el método lo que haces es aplicar el teorema de taylor para cada i=0,1,... N-1;


#Algoritmo 5.1 Euler
f<-function(t,y){
 fx = y - (t^2) + 1
  #fx= (t*exp(3*t)) - 2*y
    return(fx)
}
#######################################
#Solución exacta 

fexacta<-function(t){
  f = (t+1)^2 - 0.5*exp(t)
  return(f)
}

###############################################################
#funci?n para el m?todo  
met_euler<-function(a,b,N,alpha){   # y(a) = α ;  b = es el valor extremo de t
  
  resultado<-matrix(NA,N+1,2) # resultado<-matrix(rep(NA,2*(N+1)),ncol=2) 
  
  #PASO 1
  
  h=(b-a)/N
  t=a
  w=alpha
  resultado[1,1]=t
  resultado[1,2]=w
  
  #PASO 2
  
  for(i in 1:N){
    w=w+h*f(t,w)
    t=a+i*h  
    resultado[1+i,2]=w
    resultado[1+i,1]=t
  }
  #si quiero la tabla de pasos y resultados le saco el # a lo de abajo 
  rdo = resultado  
  
  #rdo<-c(t,w)  # si quisiera imprimir solo el último valor.
  print(rdo)
  
}
# NOTA , SI NO TENGO EL N , PUEDO DESPEJARLO DE :
#  h=(b-a)/N   <---- sIEMPRE Y CUANDO TENGA "h"

#met_euler(0,1,10,0) 
#met_euler(0,1,100,0)

#t=met_euler(0,2,10,0.5)[,1]  # selecciona toda la primer columna
w= met_euler(0,2,10,0.5)[,2] # selecciona toda la segunda columna

#met_euler(0,2,10,0.5)
b= 2
a=0
N=10
h = (b-a)/N
t = seq(0,2,h)

plot(t,w, type="b",ylim= c(0,6), ylab="y(t)", xlab= "t", xaxs = "i", yaxs="i")
lines(seq(0,2,h),fexacta(seq(0,2,0.2)),col="red")
legend('topleft', legend=c("Exacta","Euler N=10"),col=c("red","black"),lty=c(1,1),pch=c(NA,1))
title("Solución del problema: y'= y-t^2+1")

#funcion_exacta=fexacta(seq(0,2,0.2))
#error=abs(funcion_exacta-w)

#(tabla<-cbind(t,w,funcion_exacta,error))
