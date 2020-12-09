# ------  Genera un número aleatorio con distribución de Poisson
# ------ con λ = 50
#N<- rpois(n=1 , lambda=50)   # Genera un solo numero de poisson

#-------  Genera N números aleatorios con distribución Gamma.
#-------  Si shape= a, scale=a, entonces la densidad es:
# ------  f(x)= 1(s^a Gamma(a))^ x^(a-1) e^-(x/s)
#xi<- rgamma(n=N, shape= 10 , scale=5)
#S<- sum(xi)
# ----------------------------------------------------------------

## nro siniestro como variable poisson
# Ingreso el tamaño de la muestra
M<-10000

# creo la matriz donde se almacenará la salida
Resultado<-matrix(NA,nrow=M, ncol=2)

for(m in 1:M){
  
  # Genero un número aleatorio con distribución
  # de Poissson con λ=50
  N<-rpois(n=1, lambda=50)
  
  # Grabo el resultado en la primera columna de la salida
  Resultado[m,1]<-N
  
  # Genero N números aleatorios con distribucuón Gamma
  # Si shape=a, scale=s, entonces la densidad es:
  # f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s)
  Xi<- rgamma(n=N, shape=10, scale=5)  # Devuelve un vector de valores de la distribución Gamma aleatorios
  
  # Sumo los valores "N" de "Xi"
  S<- sum(Xi)
  
  #Grabo el resultado en la segunda columna de la salida
  Resultado[m,2]<-S
}
  
N.E<- mean(Resultado[,1])  # Media de la primera columna (Donde están las variables Poisson)
N.var<- var(Resultado[,1])  # Varianza de la primera columna

S.E<- mean(Resultado[,2]) 
S.de<- sd(Resultado[,2])

hist(Resultado[,2])
