rm(list=ls())
graphics.off()

f<-function(x){
  
return( f= x - ((x^3) + (4*x^2) - 10)/(3*x^2 + 8*x) )
}

PuntoFijo<-function(p0,tol,N,f){
  # PASO 1 
  i=1
  #PASO 2
  while(i<=N){
      #PASO 3
    p=f(p0)
      #PASO 4
    if (abs(p-p0) < tol) {
        
      return(p)
    }
      #PASO 5
    i = i+1
      
      #PASO 6
    p0=p
  }
  
#PASO 7
    return(paste("El método fracasó después de: ",  N , " Iteraciones."))
}

(PuntoFijo(0.5,10^-5,100,f))


