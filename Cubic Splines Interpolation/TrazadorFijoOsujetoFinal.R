rm(list= ls())
graphics.off()
x<-c(0,1,2,3)
a<-c(1,exp(1),exp(2),exp(3))
FPO<- 1
FPN<- exp(3)
vc = 1.5  


# -------------------------------------------------
TrazadorCubicoSujeto<-function(x,a,FPO,FPN,vc){
  #vc son los valores que quiero interpolar (calcular)
  # x es el valor de los vectores (original) con los que armo la función de interpolación
  # a es la imagen de las x 
  # FPO condición del punto inicial valuado en la primera derivada
  # FPN condición del punto n (el último punto) valuado en la primera derivada
  n = length(x) - 1
  h<-c()
  alfa<-c()
  b<-c()
  c<-c()
  d<-c()
  l<-c()
  z<-c()
  u<-c()
  
  #PASO 1
  for(i in 1:n){
    h[i]= x[i+1]-x[i]
  }
  #PASO 2
  alfa[1]  = (3*(a[2]-a[1])/h[1]) - 3*FPO
  alfa[n+1]= (3*FPN) - 3 *(a[n+1]-a[n])/h[n]
  #PASO 3
  for(i in 2:n){ 
    
    alfa[i]= (3/h[i])*(a[i+1]-a[i]) - (3/h[i-1])*(a[i]-a[i-1])
  }
  #PASO 4
  l[1]=2*h[1]
  u[1]=0.5
  z[1]=alfa[1]/l[1]
  #PASO 5
  for(i in 2:n){
    
    l[i] = 2*(x[i+1]-x[i-1]) - h[i-1]*u[i-1]
    u[i] = h[i]/l[i]
    z[i] = (alfa[i]-h[i-1]*z[i-1])/l[i]
  }
  #PASO 6
  l[n+1] = h[n] * (2-u[n])
  z[n+1] = (alfa[n+1] - h[n]*z[n])/l[n+1]
  c[n+1]=z[n+1]
  #PASO 7
  for(j in n:1 ){
    
    c[j] = z[j] - u[j]*c[j+1]
    b[j] = ((a[j+1]-a[j])/ h[j]) - h[j]*(c[j+1] + 2*c[j])/3
    d[j] = (c[j+1]-c[j])/(3*h[j])
  }
  #PASO 8
  b[n+1] = 0
  c[n+1] = 0
  d[n+1] = 0
  final=cbind(a,b,c,d)
  
  #######################
  
  matriz=final
  ecuacion<-list()
  
  for(i in 1:n){
    
    ecuacion[[i]] =  paste(matriz[i,1]," + ",matriz[i,2],"*(x-",x[i],")"," + ",matriz[i,3],"*(x-",x[i],")^2"," + ",matriz[i,4],"*(x-",x[i],")^3"," Para x [",x[i],",",x[i+1],"]",sep='')
  }
 #######################################################################################################################
  m= length(x)
  R=c()
  for(k in 1:length(vc)){
    for(q in 1:(m-1)){
      
      if(q<(m-1)){  
        if( vc[k] >= x[q] &  vc[k]<x[q+1]){
          
          
          R[k] = matriz[q,1] + matriz[q,2] *(vc[k]- x[q] ) + matriz[q,3] *(vc[k]- x[q] )^2 + matriz[q,4] *(vc[k]- x[q] )^3
          
        }
      }
      else if( vc[k] >= x[q] &  vc[k]<= x[q+1]){
        
        R[k] = matriz[q,1] + matriz[q,2] *(vc[k]- x[q] ) + matriz[q,3] *(vc[k]- x[q] )^2 + matriz[q,4] *(vc[k]- x[q] )^3
      }
    }}
  
  refinal = list('Funciones'=ecuacion,'Valores Interpolados'=R) 
  # R son las variables interpoladas que se calcula con Slpnes
  # ecuacion es la expresión en ecuación
  return(refinal)
  
}



TrazadorCubicoSujeto(x,a,FPO,FPN,vc)
