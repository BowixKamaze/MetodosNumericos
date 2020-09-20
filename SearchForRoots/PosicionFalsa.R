rm(list=ls())
graphics.off()

g<-function(r){
  g<- 3.65 * ((1 + r) ^ -(1/9)) + 20.31 * ((1 + r) ^ -(3/5)) + 2.92 * ((1 + r) ^ -(10/9)) + 19.58 * ((1 + r) ^ -(8/5)) + 2.19 * ((1 + r) ^ -(19/9)) + 18.85 * ((1 + r) ^ -(13/5)) + 1.46 * ((1 + r) ^ -(28/9)) + 18.12 * ((1 + r) ^ -(18/5)) + 0.73 * ((1 + r) ^ -(37/9)) + 17.43 * ((1 + r) ^ -(23/5))
  return(g)
}

f<-function(r){
  
  van<- (-39.50 + g(r))
  return(van)
}

# -----------------------------------------

FalsaPosicion<-function(po,p1,tol,n){
  
  #PASO 1
  i=2
  qo=f(po)
  q1=f(p1)
  
  #PASO 2
  while(i<=n){
    
    #PASO 3
     p= p1 - q1*(p1-po)/(q1-qo)
    # PASO 4
     if(abs(p-p1)<tol ){
       return(p)
       
       
     }
     #PASO 5
     i = i+1
     q=f(p)
     # PASO 6
     if(q*q1 < 0){
       po= p1
       qo=q1
       
     }
     # PASO 7
     p1 =p
     q1=q
  }
#PASO 8
  return(paste("El método falló depués de: " , n , " iteraciones."))
  
}

plot(seq(0,1,0.2),f(seq(0,1,0.2)))

# # LOS PARÁMETROS DEL MODELO SON :  (0.6,0.7,10^-6,100)

(FalsaPosicion(0.6,0.7,10^-6,100))
