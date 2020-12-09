f1<- function(x){x^2} # Funcion que se desea integrar en el intervalo [0,1]
n<-10000 # Tamaño de la muestra
a=10
b=15
U<- a +(b-a) *runif(n)  # Generación de números uniformes entre a y b



Altura.Promedio<- (1/n) * sum(f1(U)) 
Ancho.Base<- b-a
Integral.MC<- Altura.Promedio*Ancho.Base

sf.MC<- sqrt((1/(n-1)) * sum(( f1(U) * (b-a) - Integral.MC)^2))  #Estimación de Sigma(f)
Error.MC<- sf.MC/sqrt(n)  # Error en la integral MC
