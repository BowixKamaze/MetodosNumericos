rm(list=ls())
graphics.off()
funcion<-function(x){
  f<-   sqrt(x) - cos(x) 
  return(f)
  
}

Biseccion<-function(a,b,tol,n,f){
  
  
#PASO 1
i=1
FA=f(a)

#PASO 2
while(i<=n){
  
  #PASO 3
  p=a+ (b-a)/2
  FP=f(p)
  
  #PASO 4
  if(FP==0 | (b-a)/2 < tol){
    return(p)
    
    
  }
  #PASO 5
  i= i+1
  
  #PASO 6
  if (FA*FP > 0){
    a=p
    FA=FP
  }else{
    b=p
  }
    
  }
#PASO 7
  
  return(paste("El método fracasó despúes de: " , n , " Iteraciones"))

}

plot(seq(0,2,0.25),funcion(seq(0,2,0.25)))
a = 0.5
b = 1
tol = 10^-6
n = 100
(Biseccion(a,b,tol,n,funcion))
