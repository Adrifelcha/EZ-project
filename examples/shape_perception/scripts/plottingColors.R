# Custom function to select colors
myCol <- function(r,g,b,sub,alpha=1){
  rgb(r[sub]/255,g[sub]/255,b[sub]/255,alpha)
}

r <- matrix(NA, ncol=5,nrow=9)
g <- matrix(NA, ncol=5,nrow=9)
b <- matrix(NA, ncol=5,nrow=9)

# Paleta violeta
r[,1] <- round(seq(124,234, length.out=9),0)
g[,1] <- round(seq(33,46, length.out=9),0)
b[,1] <- round(seq(135,255, length.out=9),0)

# Paleta verde
r[,2] <- round(seq(30,90, length.out=9),0)
g[,2] <- round(seq(116,255, length.out=9),0)
b[,2] <- round(seq(54,93, length.out=9),0)

# Paleta azul
r[,3] <- round(seq(44,108, length.out=9),0)
g[,3] <- round(seq(87,173, length.out=9),0)
b[,3] <- round(seq(142,255, length.out=9),0)

# Paleta amarillo
r[,4] <- round(seq(123,255, length.out=9),0)
g[,4] <- round(seq(104,210, length.out=9),0)
b[,4] <- round(seq(34,49, length.out=9),0)

# Paleta rojo
r[,5] <- round(seq(99,255, length.out=9),0)
g[,5] <- round(seq(25,40, length.out=9),0)
b[,5] <- round(seq(25,40, length.out=9),0)