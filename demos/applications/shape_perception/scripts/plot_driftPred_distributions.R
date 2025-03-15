##########################################################################
#
##########################################################################
source("../../../code/functions/plot_VerticalHist.R")
driftPred <- drift_pred[,1:4]
colnames(driftPred) <- c("QualConvex","QuantConvex","QualConcav","QuantConcav")


load("../results/data_toJAGS.RData")
load("../results/samples.RData")
source("./loadSamples.R")


binWidth = 0.1
binStarts <- c(0.9,1.1,1.9,2.1)
binMids <- binStarts + binWidth / 2

means <- apply(driftPred, 2, mean)
CI <- apply(driftPred, 2, quantile, prob=c(0.025,0.975))

DOYrange <- range(driftPred)
## Get the histogram obects
histList <- apply(driftPred, 2, function(x, hCol) hist(x, plot = FALSE))

## Plotting
axisCol <- "gray30"
xlim <- c(0.5,2.5)
ylim <- c(-1.2,2.1)
xlabs <- c("Change in Convexity", "Change in Concavity")
fillCol <- c("#42B6EC", "#0D41D5", "#42B6EC", "#0D41D5")
CIcolor <- rep("#CAC7DA",4)

png(file = "../../../figures/mlr_driftPredDistributions.png", width = 7, height = 5, units="in",res=300) # Width and height of the plot in 
plot(c(0, 5), DOYrange, type = "n", xlim=xlim, ylim=ylim,
     ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
axis(1, 1:2, xlabs, cex.axis = 1.2, col = NA, line=-0.5, f=1)
mtext(side = 1, outer = F, line = 2.5, "Change type", cex = 1.5, f=2)
y.seq = format(round(seq(ylim[1],ylim[2],length.out=9),digits = 1), nsmall = 1)
axis(2, cex.axis = 0.95, las = 1, line = -.7, col = "white", tck = 0,
     at = y.seq, labels = y.seq, las=2)
mtext(side = 2, outer = F, line = 2, expression(paste(nu^pred)), cex = 1.2)
box(bty = "L", col = axisCol)

biggestDensity <- max(unlist(lapply(histList, function(h){max(h[[4]])})))
xscale <- binWidth * .9 / biggestDensity

## Plot the histograms
for (i in 1:4) {
  X <- binStarts[i]
  VerticalHist(x = X, xscale = xscale, 
               xwidth = binWidth, 
               hist= histList[[i]], 
               fillCol = fillCol[i])
  points(X, means[i], pch=18, cex=1.2)
  lines(x=c(X-0.05, X+0.05), y=c(means[i],means[i]), lwd=2)
}
lines(binStarts[c(1,3)], means[c(1,3)], lwd=1, lty=2)
lines(binStarts[c(2,4)], means[c(2,4)], lwd=1, lty=2)
legend(0.5,1.9, c("Qualitative change", "Quantitative change"), col=c("#42B6EC", "#0D41D5"), pch=15, bty = "n", cex=1.2)
text(0.9,1.9,"Change quality", cex=1.5, f=2)

dev.off()