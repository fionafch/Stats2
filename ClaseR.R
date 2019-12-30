
## Estadistica II
## Curso: Silvia Vietri
## Aplicaciones en R
## Fiona Franco Churruarin

setwd("E:/R")

# Paquetes a utilizar -------------------------------------------------------------------
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages('GGally')
#install.packages("scatterplot3d")
#install.packages("car")

library("dplyr")
library("ggplot2")
library("readxl")
library('GGally')
library("scatterplot3d")
library("car")

options(scipen = 999) # desactivar notacion cientifica
rm(list=ls()) # borrar elementos del global environment

# Introduccion y nociones ------------------------------------------------------------

# Operaciones

1+1; 1-3; 1/4; 3*5; sqrt(16); abs(-31)

# Creacion de distintos tipos de elementos 

x <- 2
x

y <- c(1,2,3)
y

z <- 1:10
z

class(x); class(y); class(z)

z <- seq(1:100)
z[34]

z <- 3*seq(from=1, to=1000, by=1)+11
#*# Cuanto vale el elemento 333 de z? 
z[333]

# Algebra vectorial y matricial

vector <- c("sdjf", hola)
a <- c(2,-5,3)
b <- c(-1, 5, -3)
2*a
a+b
a*a       #producto elemento por elmento 
sum(a*a)  #producto interno
a/a; a/b  #cociente elemento por elemento

A <- matrix(c(2, 3, -4,
              6, 2,  5,
              0, 1,  4),
              nrow = 3, ncol= 3)
A
A %*% A   # producto matricial
det(A)
solve(A)  # inversa
solve(A) %*% A
diag(10)  
A %*% diag(3)

# Logica

1 == 1; 2 > 1; 3 >= 1; 3 >= 3; 3 >= 21

sqrt(2) == 2^.5

solve(A) %*% A == diag(3)
round(solve(A) %*% A, 1) == diag(3)

TRUE & FALSE; TRUE & TRUE; FALSE|TRUE; FALSE|FALSE

rm(list=ls())

# Datos y estadisticas descriptivas -------------------------------------------------------------------

# Carga de datos, desde Excel

DatosClase <- read_excel("E:/R/DatosClase.xlsx")

class(DatosClase)

# Explorando los datos

View(DatosClase)

names(DatosClase)

head(DatosClase)
tail(DatosClase)

pairs(DatosClase)
ggpairs(DatosClase)

plot(DatosClase$x1, DatosClase$y)

plot(DatosClase$x1, DatosClase$y,
     main = "Relacion entre (x1, y)",
     xlab = "x_1",
     ylab = "y",
     type = "p",
     col  = "blue2",
     pch  = 19,
     cex  = .5)

#*# Ejercicio: plotear x1 y x2 haciendo que cada par (x1, x2) sea un punto verde

mean(DatosClase$y); mean(DatosClase$x1)

# Como tomo promedios de todos mis datos juntos?

mean(DatosClase)

colMeans(DatosClase)
rowMeans(DatosClase)


var(DatosClase$y); var(DatosClase$x1)
sd(DatosClase$y); sd(DatosClase$x1)

summary(DatosClase) # Extremos, cuartiles y media

var(DatosClase) # Matriz de varianzas y covarianzas

cor(DatosClase) # Matriz de correlacion

cor(DatosClase)[1,2] == var(DatosClase)[1,2]/(sd(DatosClase$y)*sd(DatosClase$x1))


# Regresion ---------------------------------------------------------------

# Regresion simple

rls_x1 <- lm(y ~ x1, data = DatosClase)

summary(rls_x1)

class(rls_x1)

plot(DatosClase$x1, DatosClase$y,
     main = "Relacion entre (x1, y)",
     xlab = "x_1",
     ylab = "y",
     type = "p",
     col  = "blue2",
     pch  = 19,
     cex  = .5)
abline(rls_x1)

ggplot(DatosClase, aes(y=y, x=x1)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  ggtitle("Relacion entre (x1, y)")

#*# Ejercicio: replicar esto para la relacion entre (x2, y), (x3, y), (x1, 2), (x1, x3), (x2, x3)

# Evaluacion del modelo

summary(rls_x1)

# Muestra coeficientes, errores estandar, test de significatividad individual, test de significatividad global y R2.
# podemos comprobar esto:

# estadistico t para b1

summary(rls_x1)$coef

class(summary(rls_x1)$coef)

b1  <- summary(rls_x1)$coef[2,1]
ee1 <- summary(rls_x1)$coef[2,2]

t_b1_rlsx1 <-  b1 / ee1
t_b1_rlsx1

dt(t_b1_rlsx1, 28)*2

# del p-valor puedo recuperar el estadistico

p2 <- summary(rls_x1)$coef[1,4]
p2
qt(p2/2, 28)


# Regresion multiple

source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')

# Que explica que cambie tanto el resultado cuando agrego una variable explicativa?

rlm_x1x3 <- lm(y ~ x1 + x3, data = DatosClase)

DatosConNombre  <- DatosClase
names(DatosConNombre)  <- c("wage", "educ", "edad", "exper")
View(DatosConNombre)

rlm_x1x3nombre <- lm(wage ~ educ + exper, data = DatosConNombre)

summary(rlm_x1x3)

## puede ser tener una sola variable sea restrictivo y no me permita ver el efecto 
#  de x1 en y, sino que este "contaminado" por factores no observables

rls_x3 <- lm(y ~ x3, data = DatosClase)

summary(rls_x3)

ggplot(DatosClase, aes(y=y, x=x3)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Relacion entre (x3, y)")

# Relacion entre las variables explicativas

rls_x1x3 <- lm(x1 ~ x3, data = DatosClase)

summary(rls_x1x3)

ggplot(DatosClase, aes(y=x1, x=x3)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Relacion entre (x3, x1)")


# que conclusiones podemos extraer?

summary(rls_x1)
summary(rls_x3)
summary(rlm_x1x3)

plot3d <- scatterplot3d(DatosClase$x1, DatosClase$x3, DatosClase$y,
                        main  = "Relacion entre (x1, x3, y)",
                        xlab  = "x1", ylab = "x3", zlab = "y",
                        pch   = 16,
                        color = "steelblue",
                        type  = "h",
                        box   = FALSE)
addgrids3d(DatosClase$x1, DatosClase$x3, DatosClase$y, 
           grid = c("xy", "xz", "yz"))
plot3d$plane3d(rlm_x1x3)


plot3d <- scatterplot3d(DatosClase$x1, DatosClase$x3, DatosClase$y,
                        main  = "Relacion entre (x1, x3, y)",
                        xlab  = "x1", ylab = "x3", zlab = "y",
                        pch   = 16,
                        color = "steelblue",
                        type  = "h",
                        angle = 90)
plot3d$plane3d(rlm_x1x3)


plot3d <- scatterplot3d(DatosClase$x1, DatosClase$x3, DatosClase$y,
                        main  = "Relacion entre (x1, x3, y)",
                        xlab  = "x1", ylab = "x3", zlab = "y",
                        pch   = 16,
                        color = "steelblue",
                        type  = "h",
                        angle = 180)
plot3d$plane3d(rlm_x1x3)


# Seleccion de modelo

anova(rls_x1, rlm_x1x3)

plot(rls_x1)
plot(rls_x1x3)

# Analisis de residuos

fitval <- fitted.values(rlm_x1x3)
sresid <- rstandard(rlm_x1x3)

plot(x = fitval, y = sresid,
     xlab = "valores ajustados",
     ylab = "residuos estandarizados")
abline(h=0)

qqnorm(sresid); qqline(sresid)

durbinWatsonTest(rlm_x1x3)

# Esto es similar a bondad de ajuste

ks.test(sresid, "pnorm",
        mean = 0, sd = 1,
        alternative = "two.sided")


# Aplicacion del modelo


#si bien el modelo con dos variables era mejor, a fines de mostrar como plotear voy a usar rls_x1

newdata <- data.frame(x1=45, x3=48)
newdata

predict(rls_x1)
conf.int <- predict.lm(rls_x1,
                       interval = "confidence",
                       level = .95)
conf.int
class(conf.int)

predict.lm(rls_x1, newdata,
        interval = "confidence",
        level = .95)


pred.int <- predict.lm(rls_x1,
                    interval = "prediction",
                    level = .95)
pred.int

predict.lm(rls_x1, newdata,
        interval = "prediction",
        level = .95)

mydata <- cbind(DatosClase, pred.int, conf.int)
class(mydata)
names(mydata) 
names(mydata) <- c("y", "x1",  "x2",  "x3",  "p.fit", "p.lwr", "p.upr", "c.fit", "c.lwr", "c.upr")

rlsx1plot <- ggplot(mydata, aes(y=y,x=x1)) +
                    geom_point() +
                    stat_smooth(method=lm, se=FALSE) +
                    ggtitle("Relacion entre (x1, y)")
  
rlsx1plot

rlsx1plot + 
  geom_line(aes(y=p.lwr), color = "red", linetype = "dashed") + 
  geom_line(aes(y=p.upr), color = "red", linetype = "dashed") 

rlsx1plot + 
  geom_line(aes(y=c.lwr), color = "green", linetype = "dashed") + 
  geom_line(aes(y=c.upr), color = "green", linetype = "dashed") 

rlsx1plot + 
  geom_line(aes(y=p.lwr), color = "red", linetype = "dashed")    + 
  geom_line(aes(y=p.upr), color = "red", linetype = "dashed")    +
  geom_line(aes(y=c.lwr), color = "green", linetype = "twodash") + 
  geom_line(aes(y=c.upr), color = "green", linetype = "twodash")  


#*# Probar lo mismo pero usar la funcion predict en vez de predict.lm y ver como cambian los graficos. 
#   Por que ocurre esto?

# intervalo de confianza para los parametros

confint(rlm_x1x3, level = .95)


# multicolinealidad perfecta

DatosClase <- DatosClase %>%
  mutate(x4 = 3 * x1 + 1/2 * x2 + 113) %>%
  mutate(x5 = 2 * x3)

DatosClase

summary(lm(y ~ x4, data = DatosClase))
summary(lm(y ~ x1+x4, data = DatosClase))
summary(lm(y ~ x1+x3+x4, data = DatosClase))
summary(lm(y ~ x1+x2+x3+x4, data = DatosClase))
summary(lm(y ~ x1+x2+x3+x4+x5, data = DatosClase))

# Simulacion --------------------------------------------------------------


# Distribuciones muestrales: Ji-Cuadrado

set.seed(285) # para replicabilidad

n <- 10
Z <- rnorm(n, 0, 1)
summary(Z)

print(Z)

x <- seq(from = -4, to = 4, by= 0.01)

hist(Z, freq = FALSE, 
     breaks  = 10,
     xlim    = c(-4,4),
     main    = "Realizaciones de v.a. con distribución normal estándar")

curve(dnorm(x, 0, 1),
      from = -4, to = 4,
      col  = "blue2",
      lwd  = 3,
      add = TRUE)

print(Z)
print(Z^2)
print(sum(Z^2))

SumZsq <- function(n, m){
  # n es la cantidad de N(0,1) que sumo
  # m es el numero de repeticiones que quiero
  v <- vector()
  for(i in 1:m) {
    z <- rnorm(n, 0, 1)
    v[i] <- sum(z^2)
  }
  return(v)
}

SumZsq(n=10, m=40)

n <- 10 
m <- 100
hist(SumZsq(n, m), freq = FALSE, breaks=30,
     main = "Suma de cuadrados de realizaciones de N(0,1) \n y distribución Ji-Cuadrado",
     xlim = c(0,40), ylim = c(0,.15))
curve(dchisq(x, df=n),
      from = 0, to = 40,
      col  = "blue2",
      lwd  = 2,
      add  = TRUE)
legend('topright',legend =  bquote(atop(paste("gl=",.(n)), paste("reps=",.(m)))))


#cambiando m

par(mfrow=c(1,2))
for (m in c(30, 500)){
  hist(SumZsq(n, m), freq = FALSE, breaks=20,
       xlim = c(0,50), ylim = c(0,.15))
  curve(dchisq(x, df=n),
        from = 0, to = 50,
        col  = "blue2",
        lwd  = 2,
        add  = TRUE) 
  legend('topright',legend =  bquote(atop(paste("gl=",.(n)), paste("reps=",.(m)))))
}

# cambiando n

par(mfrow=c(2,2))
for (n in c(5, 10, 25, 50)){
  set.seed(10)
  hist(SumZsq(n, 30), freq = FALSE, breaks=20,
       main = bquote(paste("gl=",.(n), ", reps=",.(m))),
       xlim = c(0,80), ylim = c(0,.15))
  curve(dchisq(x, df=n),
        from = 0, to = 80,
        col  = "blue2",
        lwd  = 2,
        add  = TRUE) 
}

# Estimadores sesgados, insesgados, consistentes e inconsistentes

rm(list=ls())


# Los estimadores son funciones delos datos muestrales. 
# Al tomar muestras aleatorias, estos también v.a.
# Siendo X una v.a., puedo programar funciones en R que se comporten como estimadores.

#La función mean es la media muestral definida como de costumbre



# defino el desvío estándar muestral (varianza sobre n)
# la función var o tomar sqrt(var) está pensada en términos poblacionales
sdxbar <- function(x, sigmasqr, n){
  sqrt (var(x)/n)
}
# meanxtilde define un estimador sesgado pero consistente de la media poblacional
meanxtilde <- function(x, n){
  sum(x) / (1+n)
}
#desvío estándar de xtilde
sdxtilde <- function(x, n, sigmasqr){
  sqrt ((n * var(x) / ((n + 1) ^ 2)))
}
#poblacion vs estimador insesgado vs estimador sesgado, ambos consistentes

n <- 50
mu = 1000
sigmasqr = 100
sigma = sqrt(sigmasqr)

#tomo una m.a. de una normal con media 1000 y desvío 10 (poner sqrt(sigmasqr))
n<-300
sample <- rnorm(n, mu, sigma)

par(mfrow=c(1,1))
hist(sample, breaks = 20, probability = TRUE,
     main = "Distribucion de X y estimadores de X",
     font.main = 2, las = 1, xlim = c(950, 1050), ylim = c(0,0.07))

x <- seq(mu-5*sigma, mu+5*sigma, by = 0.1)

curve(dnorm(x, mean = mu, sd = sigma),
      from = mu-5*sigma, to = mu+5*sigma,
      col = "blue4", lwd=3,
      add = TRUE) #dist poblacional

curve(dnorm(x, mean = mean(sample), sd = sdxbar(sample, n, sigmasqr)),
      from = mu-5*sigma, to = mu+5*sigma,
      col = "turquoise3", lwd=3, lty="twodash",
      add = TRUE) #dist muestral del insesgado

curve(dnorm(x, mean = meanxtilde(sample, n), sd = sdxtilde(sample, n, sigmasqr)),
      from = mu-5*sigma, to = mu+5*sigma,
      col = "red", lwd=3, lty="dashed",
      add = TRUE) #dist muestral del sesgado

legend("topright", 
       legend = c("Dist. poblacional", 
                  "Dist. xbar",
                  "Dist. xtilde", 
                  'n=500' ), 
       col = c('blue2', 'red', 'orange', 'white'),
       lty = 1)

mu; sigma
mean(sample) ; sd(sample) ; sdxbar(sample, n, sigmasqr)
meanxtilde(sample, n) ; sdxtilde(sample, n, sigmasqr) 

PlotEstimatorDist <- function(mu, sigma, n, ylim){
  hist(sample, breaks = 20, probability = TRUE, 
       main = "Distribucion de X y estimadores de X",
       font.main = 2, las = 1, xlim = c(mu-5*sigma, mu+5*sigma), ylim = c(0, ylim))
  x <- seq(mu-5*sigma, mu+5*sigma, by = 0.1)
  curve(dnorm(x, mean = mu, sd = sigma),
        from = mu-5*sigma, to = mu+5*sigma,
        col = "blue4", lwd=3,
        add = TRUE) #dist poblacional
  curve(dnorm(x, mean = mean(sample), sd = sdxbar(sample, n, sigmasqr)),
        from = mu-5*sigma, to = mu+5*sigma,
        col = "turquoise", lwd=3, lty="twodash",
        add = TRUE) #dist muestral del insesgado
  curve(dnorm(x, mean = meanxtilde(sample, n), sd = sdxtilde(sample, n, sigmasqr)),
        from = mu-5*sigma, to = mu+5*sigma,
        col = "red3", lwd=3, lty="dashed",
        add = TRUE) #dist muestral del sesgado
}

true.mu = 1000
true.sigmasqr = 100
true.sigma = sqrt(true.sigmasqr)

par(mfrow=c(2,2))
for(n in c(20, 50, 250, 500)){
  set.seed(285)
  sample <- rnorm(n, true.mu, true.sigma)
  PlotEstimatorDist(true.mu, true.sigma, n, ylim=0.07)
  print(paste("n=", n))
  print(paste("xbar:   ", "mean=", mean(sample), sd(sample), sdxbar(sample, n, true.sigmasqr)))
  print(paste("xtilde: ", "mean=", meanxtilde(sample, n), sdxtilde(sample, n, true.sigmasqr)))
}

# Redefino xtilde

# meanxtilde define un estimador sesgado pero consistente de la media poblacional
meanxtilde <- function(x, n){
  sum(x) / (1+n) + 50
}

true.mu = 1000
true.sigmasqr = 100
true.sigma = sqrt(true.sigmasqr)

par(mfrow=c(2,2))
for(n in c(20, 50, 250, 500)){
  set.seed(285)
  sample <- rnorm(n, true.mu, true.sigma)
  PlotEstimatorDist(true.mu, true.sigma, n, ylim=0.07)
  print(paste("n=", n))
  print(paste("xbar:   ", "mean=", mean(sample), sd(sample), sdxbar(sample, n, true.sigmasqr)))
  print(paste("xtilde: ", "mean=", meanxtilde(sample, n), sdxtilde(sample, n, true.sigmasqr)))
}

