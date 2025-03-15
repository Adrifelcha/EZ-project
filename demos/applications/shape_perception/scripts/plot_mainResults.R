###########################################################################
#
###########################################################################
load("../results/data_toJAGS.RData")
load("../results/samples.RData")
source("./loadSamples.R")

png(file = "../../../figures/mlr_QualandChangeTypeEffcts.png", width = 7, height = 5, units="in",res=300) # Width and height of the plot in 
par(mar=c(3.5,1.5,1.5,1), bg=NA)
layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE))

curve.gamma1 <- myCol(r[,1],g[,1],b[,1],5,0.5)
line.gamma1  <- myCol(r[,1],g[,1],b[,1],2,1)
curve.gamma2 <- myCol(r[,3],g[,3],b[,3],5,0.5)
line.gamma2  <- myCol(r[,3],g[,3],b[,3],2,1)
curve.gamma3 <- myCol(r[,4],g[,4],b[,4],5,0.5)
line.gamma3  <- myCol(r[,4],g[,4],b[,4],2,1)

max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))
hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
     ann=F, axes = F, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
lines(density(gamma[,1]), lwd=4, col=line.gamma1)
mtext("Change quality", cex=0.9, f=2, side=3, line=0.1)
mtext("Posterior Density",side=2,line=0, cex=0.8)
mtext(expression(paste(gamma[1])),side=1,line=1.9, cex=1)
abline(v=0,lty=2,col="gray50")
axis(1,seq(-1.5,1.5,length.out=10),round(seq(-1.5,1.5,length.out=10),1), line=-0.4, cex.axis=0.8)
legend(-0.2,1.6, c("B = 1  Quantitative", "B = 0  Qualitative"), bty = "n", bg = "white", cex=0.8)

hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
     ann=F, axes = F, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
lines(density(gamma[,2]), lwd=4, col=line.gamma2)
mtext("Change type", cex=0.9, f=2, side=3, line=0.1)
mtext(expression(paste(gamma[2])),side=1,line=1.9, cex=1)
abline(v=0,lty=2,col="gray50")
legend(-1.8,1.6, c("C = 1  Concavity", "C = 0  Convexity"), bty = "n", bg = "white", cex=0.8)
axis(1,seq(-1.5,1.5,length.out=10),round(seq(-1.5,1.5,length.out=10),1), line=-0.4, cex.axis=0.8)

hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
     ann=F, axes = F, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
lines(density(gamma[,3]), lwd=4, col=line.gamma3)
mtext("Change quality x type", cex=0.9, f=2, side=3, line=0.1)
mtext(expression(paste(gamma[3])),side=1,line=1.9, cex=1)
abline(v=0,lty=2,col="gray50")
par(mar=c(2,14,0.5,14))
axis(1,seq(-1.5,1.5,length.out=10),round(seq(-1.5,1.5,length.out=10),1), line=-2, cex.axis=0.8)

data <- driftPred
binWidth = 0.1

binStarts <- c(0.9,1.1,1.9,2.1)
binMids <- binStarts + binWidth / 2

DOYrange <- range(data)
## Get the histogram obects
histList <- apply( data, 2, function(x, hCol) hist(x, plot = FALSE))

means <- apply(data, 2, mean)
CI <- apply(data, 2, quantile, prob=c(0.025,0.975))
shadeCI <- list()
for(i in 1:ncol(CI)){
  keep <- (data[,i] > CI[1,i]) & (data[,i] < CI[2,i])
  nBreaks <- sum(histList[[i]]$breaks >= CI[1,i] & histList[[i]]$breaks <= CI[2,i])
  partial <- hist(data[keep,i], breaks = nBreaks, plot = FALSE)
  shadeCI <- append(shadeCI, list(partial))
}


## Plotting
xlim <- c(0.5,2.5)
ylim <- c(-1.2,1.9)
xlabs <- c("Change in Convexity", "Change in Concavity")
fillCol <- c("#42B6EC", "#0D41D5", "#42B6EC", "#0D41D5")
CIcolor <- rep("#CAC7DA",4)

plot(c(0, 5), DOYrange, type = "n", xlim=xlim, ylim=ylim,
     ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
axis(1, 1:2, xlabs, cex.axis = 1.2, col = NA, line=-0.5, f=1)
#mtext(side = 1, outer = F, line = 2.5, "Change type", cex = 1.5, f=2)
y.seq = format(round(seq(ylim[1],ylim[2],length.out=9),digits = 1), nsmall = 1)
axis(2, cex.axis = 0.95, las = 1, line = -.7, col = "white", tck = 0,
     at = y.seq, labels = y.seq, las=2)
mtext(side = 2, outer = F, line = 2, expression(paste(nu^pred)), cex = 1.1)
box(bty = "L", col = axisCol)

biggestDensity <- max(unlist(lapply(histList, function(h){max(h[[4]])})))
xscale <- binWidth * .9 / biggestDensity

## Plot the histograms
for (i in 1:4) {
  X <- binStarts[i]
  VerticalHist(x = X, xscale = xscale, xwidth = binWidth, 
               hist= histList[[i]], fillCol = "black")
  VerticalHist(x = X, xscale = xscale*0.8, xwidth = binWidth, 
               hist= shadeCI[[i]], fillCol = fillCol[i])
  points(X, means[i], pch=18, cex=1.2)
  lines(x=c(X-0.05, X+0.05), y=c(means[i],means[i]), lwd=2)
}
lines(binStarts[c(1,3)], means[c(1,3)], lwd=1, lty=2)
lines(binStarts[c(2,4)], means[c(2,4)], lwd=1, lty=2)
legend(0.5,2.05, c("Qualitative change", "Quantitative change"), col=c("#42B6EC", "#0D41D5"), pch=15, bty = "n", cex=1.2)
#text(0.9,1.9,"Change quality", cex=1.5, f=2)
dev.off()