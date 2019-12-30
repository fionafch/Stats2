
# Simulacion de distribuciones muestrales ---------------------------------

# * Ji - Cuadrado ===================
# 

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


# * T de Student ==================

draw.T <- function(n){
  # n es grados de libertad
  Z <- rnorm(1, 0, 1)
  J <- rchisq(1, n)
  T <- Z / sqrt(J/n)
  return(T)
}

vec.T <- function(n, m){
  # n: grados de libertad
  # m: el numero de repeticiones que quiero
  v <- vector()
  for(i in 1:m){
    v[i] <- draw.T(n)
  }
  return(v)
}

n <- 10 
m <- 100



hist(vec.T(n, m), freq = FALSE, breaks=30,
     main = "Suma de cuadrados de realizaciones de N(0,1) \n y distribución Ji-Cuadrado",
     xlim = c(-10,10), ylim = c(0,.5))
curve(dchisq(x, df=n),
      from = -10, to = 10,
      col  = "blue2",
      lwd  = 2,
      add  = TRUE)
legend('topright',legend =  bquote(atop(paste("gl=",.(n)), paste("reps=",.(m)))))

Plot1Simul.T <- function(n,m){
  hist(vec.T(n, m), freq = FALSE, breaks=30,
       main = bquote(paste("gl=",.(n), ", reps=",.(m))),
       xlab = "",
       xlim = c(-10,10), ylim = c(0,.5))
  curve(dt(x, df=n),
        from = -10, to = 10,
        col  = "blue2",
        lwd  = 2,
        add  = TRUE)
}


Plot1Simul.T(n=100,m=1000)

# cambiando m

PlotSimulReps.T <- function(n, m.opt, p.rows, p.cols){
  # n: grados de libertad 
  # m.opt: vector de numero de distintas repeticiones
  # p.row: numero de filas en el plot dividido
  # p.col: numero de columnas en el plot dividido
  # restriccion: length(m.opt) = p.row * p.col
  if(length(n.opt)<9){p.cols.def<-2} else {p.cols.def<-3}
  if(missing(p.rows) | missing(p.cols)){
    p.cols = p.cols.def
    p.rows = ceiling(length(m.opt)/p.cols.def)}
  par(mfrow=c(p.rows,p.cols), mar=c(2,2,2,2))
  if(missing(p.rows) | missing(p.cols)){
   p.cols = 2
   p.rows = ceiling(length(m.opt)/2)}
  par(mfrow=c(p.rows,p.cols))
  for (m in m.opt){
    Plot1Simul.T(n,m)
  }
}

PlotSimulReps.T(n=8, m.opt = c(20,50,100,500,1000))
  
# cambiando n

PlotSimulDF.T <- function(m, n.opt, p.rows, p.cols){
  # n: grados de libertad 
  # m.opt: vector de numero de distintas repeticiones
  # p.row: numero de filas en el plot dividido
  # p.col: numero de columnas en el plot dividido
  # restriccion: length(m.opt) = p.row * p.col
  if(length(n.opt)<9){p.cols.def<-2} else {p.cols.def<-3}
  if(missing(p.rows) | missing(p.cols)){
    p.cols <- p.cols.def
    p.rows <- ceiling(length(n.opt)/p.cols.def)}
  par(mfrow = c(p.rows,p.cols), mar = c(2,2,2,2))
  for (n in n.opt){
    Plot1Simul.T(n,m)
  }
}

PlotSimulDF.T(m=200, n.opt = c(1,2,3,4,5,6,8))

# * F de Snedecor ================= 

#draw.F

draw.F <- function(n1, n2){
  J1 <- rchisq(1, n1)
  J2 <- rchisq(1, n2)
  FSned <- (J1 / n1) / (J2 / n2)
  return(FSned)
}

draw.F(2,5)

vec.F <- function(m, n1, n2){
  v <- vector()
  for(i in 1:m){
    v[i] <- draw.F(n1,n2)
  }
  return(v)
}

vec.F(100,5,15)
  
Plot1Simul.F <- function(m, n1, n2){
  hist(vec.F(m, n1, n2), freq = FALSE, breaks=30,
       main = bquote(paste("gl=",.(n1)," ",.(n2), ", reps=",.(m))),
       xlab = "",
       xlim = c(0,10), ylim = c(0,1))
  curve(df(x, df1=n1, df2=n2),
        from = 0, to = 10,
        col  = "blue2",
        lwd  = 2,
        add  = TRUE)
}

par(mfrow=c(1,1))
Plot1Simul.F(m=10,n1=5,n2=10)

PlotSimulReps.F <- function(n1, n2, m.opt, p.rows, p.cols){
  # n: grados de libertad 
  # m.opt: vector de numero de distintas repeticiones
  # p.row: numero de filas en el plot dividido
  # p.col: numero de columnas en el plot dividido
  # restriccion: length(m.opt) = p.row * p.col
  if(length(m.opt)<9){p.cols.def<-2} else {p.cols.def<-3}
  if(missing(p.rows) | missing(p.cols)){
    p.cols = p.cols.def
    p.rows = ceiling(length(m.opt)/p.cols.def)}
  par(mfrow=c(p.rows,p.cols), mar=c(2,2,2,2))
  if(missing(p.rows) | missing(p.cols)){
    p.cols = 2
    p.rows = ceiling(length(m.opt)/2)}
  par(mfrow=c(p.rows,p.cols))
  for (m in m.opt){
    Plot1Simul.F(m, n1, n2)
  }
}

PlotSimulReps.F(n1=6,n2=14, m.opt=c(10,20,30,40,50,60,70,80,90,100))

#Plot1Simul.F
#PlotSimulReps.F
#PlotSimulDF.F
