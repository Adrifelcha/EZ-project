###########################################################################################
# Load results and define plotting variables
############################################################################################
# Load packages
library(here)
# Load custom plotting function
source(here("src", "plot_VerticalHist.R")) 
# Load results
results_at <- here("output", "RData-results", "demo_shape_results.RData")
if(file.exists(results_at)){
  load(results_at)
} else {
  source(here("demos", "applications", "shape_perception", "src", "run_hypoTesting-example.R"))
}

driftPred <- drift_pred[,1:4]
colnames(driftPred) <- c("QualConvex","QuantConvex","QualConcav","QuantConcav")

axisCol <- "black"

pdf(file = here("output", "figures", "poster_shapeExample.pdf"), width = 4, height = 3.2) # Width and height of the plot in 
par(bg=NA, mai=c(0.3,0.05,0,0.05), oma= c(0,1,0.2,0.1))
layout(matrix(c(1,2,3,4,4,4), nrow = 2, ncol = 3, byrow = TRUE), heights = c(1,1.5), widths = c(1,1,1))

curve.gamma1 <- "#B499E0"
line.gamma1  <- "#5D1FBE"
curve.gamma2 <- "#A196DC"
line.gamma2  <- "#5645B4"
curve.gamma3 <- "#DC96D4"
line.gamma3  <- "#A11A91"
cutLine.color <- "#D629CD"

x_lim <- c(-1.8,1.8)
x_lab <- round(seq(-1.5,1.5,length.out=5),1)
max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))
hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
     ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
lines(density(gamma[,1]), lwd=2, col=line.gamma1)
mtext("Posterior Density",side=2,line=0.3, cex=0.7, f=2)
mtext(expression(paste(gamma[1])),side=1,line=1.2, cex=0.8)
abline(v=0,lty=2,col=cutLine.color, lwd=2)
axis(1,x_lim, c("",""), tck=-0, line=-0.2)
axis(1,x_lab, rep("", length(x_lab)), tck=-0.05, line=-0.2)
axis(1,x_lab, x_lab, cex.axis=0.6, lwd=0, line=-1)
text(0.9,1.5, "Effect of a", cex=0.7, f=2)
text(0.9,1.35, "Quantitative", cex=0.7, f=2)
text(0.9,1.2, "change", cex=0.7, f=2)

hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
     ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
lines(density(gamma[,2]), lwd=2, col=line.gamma2)
mtext(expression(paste(gamma[2])),side=1,line=1.2, cex=0.8)
abline(v=0,lty=2,col=cutLine.color, lwd=2)
axis(1,x_lim, c("",""), tck=-0, line=-0.2)
axis(1,x_lab, rep("", length(x_lab)), tck=-0.05, line=-0.2)
axis(1,x_lab, x_lab, cex.axis=0.6, lwd=0, line=-1)
text(-0.8,1.5, "Effect of a", cex=0.7, f=2)
text(-0.8,1.35, "change in", cex=0.7, f=2)
text(-0.8,1.2, "Concavity", cex=0.7, f=2)

hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
     ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
lines(density(gamma[,3]), lwd=2, col=line.gamma3)
mtext(expression(paste(gamma[3])),side=1,line=1.2, cex=0.8)
abline(v=0,lty=2,col=cutLine.color, lwd=2)
axis(1,x_lim, c("",""), tck=-0, line=-0.2)
axis(1,x_lab, rep("", length(x_lab)), tck=-0.05, line=-0.2)
axis(1,x_lab, x_lab, cex.axis=0.6, lwd=0, line=-1)
text(-1,1.5, "Interaction", cex=0.7, f=2)
text(-1,1.35, "effect", cex=0.7, f=2)

par(mar=c(2,6,1,6))
data <- drift_pred
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
ylim <- c(-1.2,2)
xlabs <- c("Change in Convexity", "Change in Concavity")
fillCol <- c("#42B6EC", "#0D41D5", "#42B6EC", "#0D41D5")
CIcolor <- rep("#CAC7DA",4)

plot(c(0, 5), DOYrange, type = "n", xlim=xlim, ylim=ylim,
     ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
axis(1, 1:2, xlabs, cex.axis = 0.7, col = NA, line=-0.7, f=2)
axis(1, 1:2, c("",""), cex.axis = 0.7, tck=-0.04)

y.seq = format(round(seq(-1.2,2,length.out=7),digits = 1), nsmall = 1)
axis(2,ylim, c("",""), tck=-0, line=0)
axis(2,y.seq, rep("", length(y.seq)), tck=-0.05, line=0)
axis(2,y.seq, y.seq, cex.axis=0.6, lwd=0, line=-0.2, las=2)

mtext(side = 2, outer = F, line = 2, expression(paste(nu^pred)), cex = 1)
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
lines(c(0.55,0.65),c(1.8,1.8), lwd=3, col="#42B6EC")
text(1.025,1.8,"Qualitative change", cex=0.8, f=2)
lines(c(0.55,0.65),c(1.6,1.6), lwd=3, col="#0D41D5")
text(1.05,1.6,"Quantitative change", cex=0.8, f=2)
dev.off()