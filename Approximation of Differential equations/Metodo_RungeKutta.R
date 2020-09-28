#Algoritmo 5.2 Runge-Kutta
rm(list=ls())
#Ejemplo de funci?n
f<-function(t,y){
  f=y-t^2+1
  return(f)
}

fexacta<-function(t){
  f = (t+1)^2 - 0.5*exp(t)
  return(f)
}

#funci?n para el m?todo  
met_RK<-function(a,b,N,alpha){  # y(a) = α ;  b = es el valor extremo de t
  resultado<-matrix(NA,N+1,2) 
  
  #PASO 1
  h=(b-a)/N
  t=a
  w=alpha
  resultado[1,1]=0
  resultado[1,2]=w
  
#PASO 2
for (i in 1:N) {
    #PASO 3    
    k1=h*f(t,w)
    k2=h*f(t+h/2,w+k1/2)
    k3=h*f(t+h/2,w+k2/2)
    k4=h*f(t+h,w+k3)
    #PASO 4
    w=w+(k1+2*k2+2*k3+k4)/6
    t=a+i*h  
    resultado[1+i,1]=t
    resultado[1+i,2]=w
    
  }
  
  #rdo<-c(t,w)   para ver el último valor.
  return(resultado)
  
}

# met_RK(0,2,10,0.5)

t    = met_RK(0,2,10,0.5)[,1]
Wi = met_RK(0,2,10,0.5)[,2]
Yi= fexacta(seq(0,2,0.2))
Error = abs(Wi-Yi)
#(tabla<-cbind(t,Yi,Wi,Error))

plot(t,Wi, type="b",ylim= c(0,6), ylab="y(t)", xlab= "t", xaxs = "i", yaxs="i",col="green")
lines(seq(0,2,0.2),Yi,col="red")
legend('topleft', legend=c("Exacta","Runge-Kutta"),col=c("red","green"),lty=c(1,1),pch=c(NA,1))




