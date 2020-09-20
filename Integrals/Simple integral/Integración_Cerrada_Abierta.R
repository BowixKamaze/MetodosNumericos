########### FORMULAS NEWTON COTES CERRADAS ############

# REGLA DEL TRAPECIO   n=1
f<-function(a,b){   # nOTA  "h" es la diferencia entre puntos :   h = x1-x0
  n=1
  h=(b-a)/n
  x0=a
  x1=b
  f<- (h/2)*(f(x0)+f(x1))    # x0 = a  , x1 =b
  
  # COTA :   -(h^3/12)*f II (e)
  return(f)
}


# REGLA DE SIMPSON  n=2
f<-function(a,b){
  n=2
  x0=a
  x1=x0+h
  x2=b
  h= (b-a)/n
  f<- (h/3)*(f(x0)+4*f(x1)+f(x2))
  #COTA :    -(h^5/90)*f IV (e)
  return(f)
    
}

# REGLA DE TRES OCTAVOS DE SIMPSON (n=3) 
f<-function(a,b,n){
  n=3
  h=(b-a)/n
  x0=a
  x1=x0+h
  x2=x0+2*h
  x3=b
  f<- (3*h/8)*(f(x0)+3*f(x1)+3*f(x2)+f(x3))
  #COTA:  -((3*h^5)/80) * f IV (e)
  return(f)
}

# REGLA DE NC CERRADA CON n=4
f<-function(a,b,n){
  h=(b-a)/n
  x0=a
  x1=x0+h
  x2=x0+2*h
  x3=x0+3*h
  x4=b
  
  f<- (2*h/45)*(7*f(x0)+32*f(x1)+12*f(x2)+32*f(x3)+7*f(x4))
  # COTA: -((8*h^7)/945)* f VI (e)
  return(f)
}

########### FORMULAS NEWTON COTES ABIERTAS ############

# REGLA DEL PUNTO MEDIO (n=0)
f<-function(a,n){
  h=(b-a)/(n+2)
  x0 = a+h
  f<- 2*h*f(x0)
  
  # COTA : +(h^3/3)* fII(e)
  return(f)
}

# REGLA NC ABIERTA CON n=1
f<-function(a,b,h){
  h=(b-a)/(n+2)
  x0=a+h
  x1=b-h
  h=(b-a)/(n+2)
  f<- (3*h/2)* (f(x0)+f(x1))
  #COTA : +((3*h^3)/4)* fII(e)
    return(f)
}

#REGLA DE NC ABIERTA CON n=2
f<-function(a,b,n){
  h=(b-a)/(n+2)
  x0=a+h
  x1=x0+h
  x2=b-h
  f<- (4*h/3)(2*f(x0)-f(x1)+2*f(x2))
  #COTA:  +((14*h^5)/45)* fIV(e)
    return(f)
}

#REGLA DE NC ABIERTA CON n=3
f<-function(a,b,n){
  h=(b-a)/(n+2)
  x0= a+h
  x1=x0 +h
  x2= x0+2*h
  x3=b-h
  f= (5*h/24)*(11*fx(x0)+fx(x1)+fx(x2)+11*fx(x3))
  #COTA :  +(95*h^5/144)* fIV(e) 
    return(f)
}
