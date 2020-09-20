f<-function(x){ #
  
  fx = (x^2) * cos(x)     
  
  return(fx)
  
  
}



####################################################################################
reglaCompuestaPuntoMedio<-function(a,b,n){
  
# NOTA  : PARA ESTE MÉTODO "n" DEBE SER PAR.
  
#PASO 1
  h= (b-a)/(n+2)  # 
  
#PASO 2
  XI2 = 0
  
#PASO 3 
  for(i in 0:n){
    
      #PASO 4
      X = a+(i+1)*h
      
      #PASO 5
      if(i%%2==0){   # Si es par entra.
      
      XI2 = XI2 + f(X)
      }
                     # si no es par , no hace nada.
  }
#PASO 6
  
  XR = 2*h* (XI2)
  
#PASO 7
  
  return(XR)
  
}

reglaCompuestaPuntoMedio(0,pi,6)  
