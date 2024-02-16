# Paquetes

install.packages("nnet")
library("nnet")

# Red neuronal

iteraciones <- 150
tamano_muestral <- 200

genera_y <- function(x){
  cos(x) + rnorm(length(x),0,0.5)
}

X <- seq(0, 3*pi, length.out = tamano_muestral)
Y <- genera_y(X)

plot(Y~X)
lines(cos(X)~X, col=2, lwd=2)

red_neuronal <- nnet(X, Y, size = 6, linout = TRUE, trace = FALSE)

YY <- predict(red_neuronal)

lines(YY~X, col=4, lwd=2)

plot(Y~X)

for (i in seq_len(iteraciones)) {
  Y <- genera_y(X)
  red_neuronal <- nnet(X, Y, size = 6, linout = TRUE, trace = FALSE)
  YY <- predict(red_neuronal)
  lines(YY~X, col=4, lwd=2)
}
lines(cos(X)~X, col=2, lwd=2)

# Regresión lineal --------------------------------------------------------


iteraciones <- 100
tamano_muestral <- 30
beta_0 <- 1
beta_1 <- -0.3


x <- seq(-3, 3, length.out = tamano_muestral)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

y <- genera_y(x, beta_0, beta_1)

modelo_lineal <- lm(y~x)

plot(x, y)
abline(beta_0, beta_1, col = 2, lwd = 2)
lines(x, modelo_lineal$fitted.values, col = 4, lwd = 2)


plot(x, modelo_lineal$fitted.values, type = "l")

for(i in seq_len(iteraciones)){
  y <- genera_y(x, beta_0, beta_1)
  
  modelo_lineal <- lm(y~x)
  
  lines(x, modelo_lineal$fitted.values)
  
}
abline(beta_0, beta_1, col = 2, lwd = 2)
