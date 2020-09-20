rm(list=ls())
graphics.off()

funcion<-function(x){
  f<-   sqrt(x) - cos(x) 
  return(f)
  
}

D(expression(sqrt(x) - cos(x) ),"x")

fprima<-function(x){
  
  fp= 0.5 * x^-0.5 + sin(x)
  return(fp)
}

MetodoNewton<-function(po,tol,n,f,fprima){
 #PASO 1 
  i=1
  # PASO 2
  while(i<=n){
     #PASO 3
      p=po - (f(po)/fprima(po))
      #PASO 4
      if(abs(p-po)<tol){
        return(p)
        
      }
      #PASO 5
      i=i+1
      #PASO 6
      po=p
    
    
  }
#PASO 7
return(paste("El método fracasó después de ", n , " Iteraciones, el resultado es: ", p))
}
plot(seq(0,1,0.2),funcion(seq(0,1,0.2)))

(MetodoNewton(0.62,10^-6,5,funcion,fprima))

