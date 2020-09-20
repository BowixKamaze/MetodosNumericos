f<-function(x){
  
  fr = ( (exp((-0.5)*((x-50)/10)^2))/ (10*sqrt(2*pi))) 
  
  return(fr)
  
}



####################################################################################
reglaCompuestaSimpson<-function(a,b,n){

# NOTA  : ACORDARSE QUE PARA ESTE MÉTODO "n" DEBE SER PAR .
  
#PASO 1
h= (b-a) /n

#PASO 2
XI0 = f(a) + f(b)
XI1 = 0
XI2 = 0

#PASO 3 
for(i in 1:(n-1)){
  
    #PASO 4
    X = a+i*h
    
    #PASO 5
    if(i%%2==0){   # Caso cuando es par 
      
      XI2 = XI2 + f(X)
      
    }else {       # Caso cuando es impar
    
      XI1= XI1 + f(X)
      
    }
}
#PASO 6
  
  XI = h* (XI0 +2*XI2 + 4*XI1)/3
  
#PASO 7
  
  return(XI)
  
 # COTA  :  -(b-a)/180  *h^4 * fIV(u)
}

reglaCompuestaSimpson(0,4,8)   
