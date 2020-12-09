f1<- function(x){x^2} # Funcion que se desea integrar en el intervalo [0,1]
n<-100 # Tamaño de la muestra
U<- runif(n)  # Generación de números uniformes

Integral.MC<- (1/n) * sum(f1(U)) # Estimación de la integral
sf.MC<- sqrt((1/(n-1)) * sum((f1(U)- Integral.MC)^2))  #Estimación de Sigma(f)
Error.MC<- sf.MC/sqrt(n)  # Error en la integral MC
Integral.MC 
sf.MC
Error.MC
 