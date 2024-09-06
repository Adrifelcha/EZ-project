n <- 500
x <- rnorm(n,2.25,1)
y <- x+rnorm(n,0,0.5)

edges <- range(x)
bins <- seq(edges[1],edges[2],length.out=10)
plot(x,y, xlim=c(edges[1],edges[2]), ylim=c(edges[1],edges[2]), ann=F, axes=F)
abline(v=bins)

