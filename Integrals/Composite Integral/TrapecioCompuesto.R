rm(list= ls())
graphics.off()


f<-function(x){ 
  fx = x * log(x)      
  
  return(fx)
  
}



####################################################################################
reglaCompuestaTrapecio<-function(a,b,n){
  
# NOTA  : PARA ESTE MÉTODO "n" PUEDE SER PAR O IMPAR.
  
#PASO 1
  h= (b-a) /n
  
#PASO 2
  XI0 = f(a) + f(b)
  XI = 0
  
#PASO 3 
  for(i in 1:(n-1)){
    
#PASO 4
    X = a+i*h
    
#PASO 5
      
    XI = XI + f(X)
      
   
  }
#PASO 6
  
  XIR = h* (XI0 +2*XI)/2
  
#PASO 7
  
  return(XIR)
  
#cOTA : -(b-a)/12 * h^2 * fII(u)  
}

reglaCompuestaTrapecio(1,2,4)  
