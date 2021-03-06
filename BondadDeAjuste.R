
# Bondad de Ajuste  ----------------------------------------------------------


x <- seq(1:50)

curve( dchisq(x, df=5), col='black', lwd = 2,
       from = 0, to = 50,
       ylab = 'p(x)',
       main = 'Distribuci�n Ji-Cuadrado')
curve( dchisq(x, df=1), col='gray', add=TRUE)
curve( dchisq(x, df=10), col='black', add=TRUE, lty = 2)
curve( dchisq(x, df=20), col='black', add=TRUE, lty = 3 )

legend('topright', 
      expression(gl==1, gl==5, gl==10, gl==20),
      lwd = c(1, 2, 1, 1),
      lty = c(1,1, 2, 3),
      col = c('grey','black', 'black', 'black'))

# Armar tabla
# Data wrangling para calcular el estad�stico
# Test

curve( dchisq(x, df=5), col='black', lwd = 2,
       from = 0, to = 20,
       ylab = 'p(x)',
       main = 'Distribuci�n Ji-Cuadrado')


  #Graficando la regi�n cr�tica
  
  #Selecci�n de grados de libertad
  df <- 6
  #Selecci�n de error tipo I
  alpha <- 0.10
  #Defino valor del eje de las abcisas (ser� un valor del estad�stico) a partir del cual rechazo H0 
  rc <- qchisq(1 - alpha, df)
  
  #Lo que haremos para graficar la regi�n cr�tica ser� construir un pol�gono con la forma de la distribuci�n
  
  #Defino el eje x (t�cnicamente deber�a seguir hasta infinito, pero a fines del gr�fico se corta antes)
  x <- seq(0,30,0.01)
  #Defino la regi�n cr�tica en el eje
  z <- seq(rc,30,0.01)
  #Defino el "lado" suoerior del pol�gono, que ser� una curva con la forma de la distribuci�n en el segmento que quiero
  p <- dchisq(z, df)
  #redefino los vectores de lados para la funci�n pol�gono 
  z <- c(z,30,rc)
  p <- c(p,min(p),min(p))
  
  #Grafico distribuci�n Ji-Cuadrado
  plot(x,dchisq(x, df),type="l",
       ylab="p(x)",xlab="x", 
       ylim=c(0.005,0.15), xlim = c(0,25),
       main = "Regi�n Cr�tica del test Ji-Cuadrado") 
  #ylim s�lo est� para que quede el eje x bien cruzado con el cero
  #Agrego al gr�fico el pol�gono
  polygon(z,p,col="gray45")
  #Agrego una l�nea que define la regi�n cr�tica
  segments(x0=rc, y0=0, x1=30, y1=0, lwd=3)
  #Agrego leyenda
  legend('center',
         expression(gl==6, alpha==0.10),
         bty = "n")


