library(boot) # Contiene el método simplex
library(linprog) # Contiene el método solveLP (usada en la última pregunta)
library(graphics) # Contiene el método plot

# Autor: Georvic Tur
# Carnet: 12-11402
# Email: alexanderstower@gmail.com


#Modelo Forma Normal Estándar
b <- c(950000, 600000, 300000, 230000)
Ae <- cbind(c(0.45, 0.18, 0.3, 0.03), c(0.38, 0.22, 0.3, 0.07), c(0.35, 0.26, 0.2, 0.14), c(-1,0,0,0), c(0, -1, 0, 0), c(0, 0, -1, 0), c(0, 0, 0, -1))
coe <- c(35, 31, 29, 0, 0, 0, 0)


# Solución óptima
x <- simplex(a=coe,A3=Ae, b3=b, maxi=FALSE)

x1 = x[["soln"]]['x1'] 
x2 = x[["soln"]]['x2']
x3 = x[["soln"]]['x3']

sprintf("La solución óptima al modelo planteado es: %s ($)", x[["value"]])
sprintf("Barriles de Crudo Ligero: %s", x1)
sprintf("Barriles de Crudo Mediano: %s", x2)
sprintf("Barriles de Crudo Pesado: %s", x3)


# Cantidad de derivados en la condición óptima

y1 = 0.45*x1 + 0.38*x2 + 0.35*x3
y2 = 0.18*x1 + 0.22*x2 + 0.26*x3
y3 = 0.3*x1 + 0.3*x2 + 0.2*x3
y4 = 0.03*x1 + 0.07*x2 + 0.13*x3

sprintf("La cantidad de barriles de derivados producidos en el óptimo es: ")
sprintf("Barriles de Gasolina: %s", y1)
sprintf("Barriles de Turbosina: %s", y2)
sprintf("Barriles de Querosene: %s", y3)
sprintf("Barriles de Bitumen: %s", y4)


# Sensibilidad del Problema

#Planteamiento del Problema dual
Aed <- rbind(c(0.45, 0.18, 0.3, 0.03), c(0.38, 0.22, 0.3, 0.07), c(0.35, 0.26, 0.2, 0.14), c(-1,0,0,0), c(0, -1, 0, 0), c(0, 0, -1, 0), c(0, 0, 0, -1))
Aed
coed <- (10^4)*c(95, 60, 30, 23)
bd <- c(35, 31, 29, 0, 0, 0, 0)


sprintf("Resolviendo Problema Dual")
xd <- simplex(a=coed,A1=Aed, b1=bd, maxi=TRUE) # X dual

xd[["soln"]] # x1 es el que afecta más (gasolina)
sprintf("Derivado cuya variación afecta más a la función objetivo: ")
sprintf("Gasolina: %s ($/barrilGasolina)", xd[["soln"]][1])
sprintf("Turbosina: %s ($/barrilTurbosina)", xd[["soln"]][2])
sprintf("Querosene: %s ($/barrilQuerosene)", xd[["soln"]][3])
sprintf("Bitumen: %s ($/barrilBitumen)", xd[["soln"]][4])
sprintf("Por tanto, el que afecta más es la gasolina.")
# Intervalo de factibilidad

x[['A']]
sprintf("Columnas que forman la base en la última tabla: %s %s %s %s",x[["basic"]][1], x[["basic"]][2], x[["basic"]][3], x[["basic"]][4])


B_2 = cbind(Ae[,6], Ae[,7],Ae[,1],Ae[,3])
sprintf("Última tabla:")
B_2
 
sprintf("Inversa de la última tabla:")
B_2_I # Este es la inversa que se puede reutilizar


sprintf("Calculando máximo valor posible de la gasolina.")
i <- 950000
while(i<=1200000){
  b_new <- c(i, 600000, 300000, 230000)
  x_solu <- B_2_I %*% (b_new)
  if (any(x_solu < 0)){ #Hay negativo?
    break
  }
  i <- i+1
}
sprintf("La cota superior es: %s", i) #Cota superior 1050001
 
sprintf("Calculando mínimo valor posible de la gasolina.")
j <- 950000
while(400000<=j){
  b2_new <- c(j, 600000, 300000, 230000)
  x_solu2 <- B_2_I %*% (b2_new)
  if (any(x_solu2 < 0)){ # Hay negativo?
    break
  }
  j <- j-1
}
sprintf("La cota inferior es: %s", j) #cota inferior 807692


sprintf("Calculando costo mínimo en función de la cantidad de gasolina entregada")
costo = NULL
dom = NULL
n<-1
k <- 400000 # Se usa el rango obtenido arriba, pues es el intervalo factible
while(k<1200000){
  xxx = solveLP(cvec=coe2, bvec=c(k, 600000, 300000, 230000), Amat=Ae2, maximum=FALSE, const.dir=c(">=",">=",">=",">="),solve.dual=TRUE,verbose=1)
  costo[n] = xxx[["opt"]]
  dom[n] = k
  k <- k+1000                                
  n <- n+1
}


sprintf("Graficando Costo Mínimo versus Barriles de Gasolina Producidos")
plot(x=dom, costo, type="l",xlab='Número de barriles de gasolina', ylab='Costo de producción mínimo', main='Variación de costo según cantidad de gasolina')



