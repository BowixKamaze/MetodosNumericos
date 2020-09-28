rm(list=ls())
graphics.off()
x<-c(2.80,3.25,3.70,4.15,4.60)
#vector con los fi correspondientes con cada xi
fx<-c(7.1027,7.6343,8.0486,8.3721,8.6235)

Dif_Dividida_Newton<-function(x,fx){
n<-as.numeric(length(fx))  # asigno a n según la cantidad de puntos que tengo (en este caso pongo los valores de esos puntos,el tamaño es igual) .
nombres<-c("fx",paste("D",1:(n-1),sep="") )  # creo un vector con nombres que voy a utilizar para ponerle nombre a las columnas.
h<-rep(NA,n*n)  # creo vector con n*n valores "NA"  .
dim(h)<-c(n,n)  # asigno dimensión a la matriz creada.
h[,1]<-fx  # le asigno valor a la primera columna para todas las filas.

dimnames(h)<-list(0:(n-1),nombres)  # acá usso los nombres creados anteriormente.

for(j in 2:n){  # Algoritmo construido en base a los valores de los puntos.
  
  for(i in j:n){
    h[i,j]<-(h[i,j-1]-h[i-1,j-1])/(x[i]-x[abs(i-j)+1])
  }
  
}

valor_r=c()
valor_r[1] = paste(h[1,1])
for(k in 1:length(fx)){
  for(l in 1:length(fx)){
    if(k==l & k>1){
        valor=paste(round(h[k,k],6))  # NOTA: ESTÁ REDONDEADO A 4 DECIMALES
        for(p in 1:(k-1)){
          valor = paste(valor,"(x-",x[p],")",sep="")   # Nota: puedo redondear  "x[p]"
        }
    valor_r[l] = valor 
    }
  }
}
polinomio_interpolado=paste(valor_r[1])
for(w in 2:length(fx)){
  
  polinomio_interpolado=paste(polinomio_interpolado," + ",valor_r[w],sep="")
  
}


#h
#polinomio_interpolado
tabla<-as.matrix(data.frame(x=x,h))
result=list('Tabla'=tabla,'Polinomio Interpolante Dif Div Newton'=polinomio_interpolado)

return(result)
}
Dif_Dividida_Newton(x,fx)
