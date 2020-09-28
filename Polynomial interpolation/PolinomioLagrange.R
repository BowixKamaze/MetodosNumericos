rm(list= ls())
graphics.off()
rm(list= ls())
x<-c(1,2,3,6,9,12) # vector original
Fe<-c(1.4517,1.5018,1.5221,1.6316,1.6432,1.6545)


# LO que hace el algoritmo es crear un polinomio de grado "n-1" (en donde n es la cantidad de datos) y también interpola el valor.

PolinomioLagrange<-function(w,x,Fe){  # Fe es la "imagen" o el valor que toman los puntos
  n = length(x)                        # w es el valor a interpolar
  # VALOR DE LAS FUNCIONES              # x es el vector de valores
  
  xm<-matrix(NA,n,n-1)
  xf = x # x fijo necesario
  for(h in 1:n){
    
    x = x[-c(h)]    # Elimina el elemento h del vector x
    for(f in 1:(n-1)){
      xm[h,f]= x[f]  # le va agregando los elementos por fila
    }
    x = xf  # que el vector vuelva a ser el fijo y vuelva al loop
  }
  
  # -------------------------------------------------------
  result<-c()  
  nume = 1
  denom= 1
  #---------------------------------------------------------
  txt0 =""
  expre=c()
  for(j in 1:n) {
    for(i in 1:(n-1)){
      nume = nume * (w-xm[j,i])
      denom = denom * (x[j]-xm[j,i])
      result[j] = nume/denom
      txt0 =  paste(txt0, "(x-",xm[j,i],")",sep="")
    }
    
    if(j<n){  
      expre[j] = paste(round(Fe[j]/denom,6), "*",sep="", txt0, "+")
      txt0 =""
    }
    else{
      expre[j] = paste(round(Fe[j]/denom,6), "*",sep="", txt0)
      txt0 =""
      
    }
    nume= 1
    denom= 1
  }
  
  ### último que agregué  22/07/2020
  polinomio_interpolado=paste(expre[1])
  for(w in 2:length(Fe)){
    
    polinomio_interpolado=paste(polinomio_interpolado,expre[w])
    
  }
  
  
  # -------------------------------------------------------
  finalresult=0
  for(j in 1:n){
    finalresult= finalresult + Fe[j]*result[j]  # Suma total
  }
  
  resultado = list('Polinomio Interpolado Lagrange'=polinomio_interpolado,'Valor interpolado'=finalresult)  
  
  return(resultado)
}
MultiplePolinomioLagrange<-function(v,PolinomioLagrange,Fe){
  
  valor=c()  
  for(k in 1:length(v)){
    
    valor[k]=PolinomioLagrange(v[k],x,Fe)[2]
  }
  return(valor)
  
} 

PolinomioLagrange(5,x,Fe)

