f<-function(r){
  
 f<-  1.65 * ((1 + r) ^ -(1)) + 1.65 * ((1 + r) ^ -(2)) + 1.65 * ((1 + r) ^ -(3)) + 1.65 * ((1 + r) ^ -(4)) + 1.65 * ((1 + r) ^ -(5)) + 1.65 * ((1 + r) ^ -(6)) + 1.65 * ((1 + r) ^ -(7)) +
      1.65 * ((1 + r) ^ -(8)) + 1.65 * ((1 + r) ^ -(9)) + 1.65 * ((1 + r) ^ -(10))+1.65 * ((1 + r) ^ -(11))+1.65 * ((1 + r) ^ -(12))+1.65 * ((1 + r) ^ -(13))+1.65 * ((1 + r) ^ -(14))+
      1.65 * ((1 + r) ^ -(15))+1.65 * ((1 + r) ^ -(16))+1.65 * ((1 + r) ^ -(17))+1.65 * ((1 + r) ^ -(18))+1.65 * ((1 + r) ^ -(19)) + 101.65 * ((1 + r) ^ -(20/5))
return(f)
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


ValoresPuntoFijo<-function(p0,tol,n){
  
  
  
}