
# Bondad de Ajuste  ----------------------------------------------------------


x <- seq(1:50)

curve( dchisq(x, df=5), col='black', lwd = 2,
       from = 0, to = 50,
       ylab = 'p(x)',
       main = 'Distribución Ji-Cuadrado')
curve( dchisq(x, df=1), col='gray', add=TRUE)
curve( dchisq(x, df=10), col='black', add=TRUE, lty = 2)
curve( dchisq(x, df=20), col='black', add=TRUE, lty = 3 )

legend('topright', 
      expression(gl==1, gl==5, gl==10, gl==20),
      lwd = c(1, 2, 1, 1),
      lty = c(1,1, 2, 3),
      col = c('grey','black', 'black', 'black'))

# Armar tabla
# Data wrangling para calcular el estadístico
# Test

curve( dchisq(x, df=5), col='black', lwd = 2,
       from = 0, to = 20,
       ylab = 'p(x)',
       main = 'Distribución Ji-Cuadrado')


  #Graficando la región crítica
  
  #Selección de grados de libertad
  df <- 6
  #Selección de error tipo I
  alpha <- 0.10
  #Defino valor del eje de las abcisas (será un valor del estadístico) a partir del cual rechazo H0 
  rc <- qchisq(1 - alpha, df)
  
  #Lo que haremos para graficar la región crítica será construir un polígono con la forma de la distribución
  
  #Defino el eje x (técnicamente debería seguir hasta infinito, pero a fines del gráfico se corta antes)
  x <- seq(0,30,0.01)
  #Defino la región crítica en el eje
  z <- seq(rc,30,0.01)
  #Defino el "lado" suoerior del polígono, que será una curva con la forma de la distribución en el segmento que quiero
  p <- dchisq(z, df)
  #redefino los vectores de lados para la función polígono 
  z <- c(z,30,rc)
  p <- c(p,min(p),min(p))
  
  #Grafico distribución Ji-Cuadrado
  plot(x,dchisq(x, df),type="l",
       ylab="p(x)",xlab="x", 
       ylim=c(0.005,0.15), xlim = c(0,25),
       main = "Región Crítica del test Ji-Cuadrado") 
  #ylim sólo está para que quede el eje x bien cruzado con el cero
  #Agrego al gráfico el polígono
  polygon(z,p,col="gray45")
  #Agrego una línea que define la región crítica
  segments(x0=rc, y0=0, x1=30, y1=0, lwd=3)
  #Agrego leyenda
  legend('center',
         expression(gl==6, alpha==0.10),
         bty = "n")


