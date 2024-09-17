

load("../../simulations/params_from_uniforms/sim_P20T20D100_MetaRegEZBHDDM_genUnif.RData")

x <- output$trueValues[,"betaweight"]
y <- output$estimates[,"betaweight"]

edges <- range(x)
bins <- seq(edges[1],edges[2],length.out=15)
edges.plot <- range(c(x,y))
plot.border <- sd(c(x,y))*0.05
plot.range <- c(edges.plot[1]-plot.border,edges.plot[2]+plot.border)
plot(x,y, xlim=plot.range, ylim=plot.range, ann=F, axes=F, col="white")
abline(0,1,col="gray70", lwd=2, lty=2)
heights <- c()
mids <- c()
for(b in 2:length(bins)){
  X.inBin <- x[x<=bins[b]&x>=bins[b-1]]
  Y.inBin <- y[x<=bins[b]&x>=bins[b-1]]
  count <- length(X.inBin)
  whiskers <- quantile(Y.inBin,probs = c(0.025,0.975))
  prop  <- count/n
  lines(c(bins[b-1],bins[b]),rep(whiskers[1],2))
  lines(c(bins[b-1],bins[b]),rep(whiskers[2],2))
  polygon(c(bins[b-1],bins[b],bins[b],bins[b-1]),
          c(whiskers[2],whiskers[2],whiskers[1],whiskers[1]),
          col=rgb(0.2,0.8,0.6,prop), border = NA)
  heights <- rbind(heights,whiskers)
  mids <- append(mids,median(c(bins[b],bins[b-1])))
}
points(mids, heights[,1], pch=16, cex=0.5)
points(mids, heights[,2], pch=16, cex=0.5)
lines(mids,heights[,1])
lines(mids,heights[,2], )
points(x,y, cex=0.8, pch=16, col=rgb(0.2,0.8,0.6,0.3))
axis.labels <- seq(plot.range[1],plot.range[2],length.out=7)
axis(1, axis.labels, round(axis.labels,1))
axis(2, axis.labels, round(axis.labels,1), las=2)