
f1<-function(x){
  
  
   (1/sqrt(2*pi*10^2))*exp(-(x-5)^2/(2*10^2))  # Funcion de densidad normal con u=5 y sigma=10
  
}

n<-100
a=10
b=20
U<- a+(b-a) * runif(n)

Altura.Promedio<- (1/n) * sum(f1(U))
Altura.Promedio
Ancho.Base<- b-a
Ancho.Base

Integral.MC<- Altura.Promedio*Ancho.Base
Integral.MC


sf.MC<- sqrt((1/(n-1))*sum((f1(U)*(b-a)-Integral.MC)^2))
Error.MC<- sf.MC/sqrt(n)
Error.MC

pnorm(20,5,10)-pnorm(10,5,10)  # Calculo con paquete de R con la función de Dist.

# Notar que el intervalo  Integral.MC +/-  2* Error.MC contiene al valor calculado directamente con R

Integral.MC+ 2* Error.MC   
Integral.MC- 2* Error.MC
