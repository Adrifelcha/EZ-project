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

###########################################################################################
# Plotting file description:
# This R script generates figures used in the paper
############################################################################################
# Custom plotting functions and variables

driftPred <- drift_pred[,1:4]
colnames(driftPred) <- c("QualConvex","QuantConvex","QualConcav","QuantConcav")

axisCol <- "black"
curve.gamma1 <- "#FDBB86"
line.gamma1  <- "#D57004"
curve.gamma2 <- "#C4FFCA"
line.gamma2  <- "#2D9033"
curve.gamma3 <- "#81A0D7"
line.gamma3  <- "#103C8A"
cutLine.color <- "#DF1919"

x_lim <- c(-1.8,1.8)
x_lab <- round(seq(-1.5,1.5,length.out=5),1)
max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))

###################################################################
# Figure 1: Gamma posteriors and drift rate predictions
###################################################################
plot_gamma_posteriors_and_drift <- function() {
  # Set up 2x2 layout
  par(mfrow = c(2,2), mai=c(0.4,0.4,0,0), oma= c(0,0,0.2,0), bg=NA)
  
  ###################################################################
  # Top-left panel: Gamma 1 : Main effect of a Quantitative change
  ###################################################################
  hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
       ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
  lines(density(gamma[,1]), lwd=2, col=line.gamma1)
  mtext("Posterior density",side=2,line=0.3, cex=0.8, f=2)
  mtext(expression(paste(gamma[1])),side=1,line=1, cex=0.8)
  abline(v=0,lty=2,col=cutLine.color, lwd=2)
  axis(1,x_lim, c("",""), tck=-0, line=-0.2)
  axis(1,x_lab, rep("", length(x_lab)), tck=-0.05, line=-0.2)
  axis(1,x_lab, x_lab, cex.axis=0.6, lwd=0, line=-1)
  text(0.9,1.5, "Effect of a", cex=0.8, f=2)
  text(0.9,1.3, "Quantitative", cex=0.8, f=2)
  text(0.9,1.1, "change", cex=0.8, f=2)
  
  ###################################################################
  # Top-right panel: Gamma 2 : Main effect of a change in Concavity
  ###################################################################
  hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
       ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
  lines(density(gamma[,2]), lwd=2, col=line.gamma2)
  mtext("Posterior density",side=2,line=0.3, cex=0.8, f=2)
  mtext(expression(paste(gamma[2])),side=1,line=1, cex=0.8)
  abline(v=0,lty=2,col=cutLine.color, lwd=2)
  axis(1,x_lim, c("",""), tck=-0, line=-0.2)
  axis(1,x_lab, rep("", length(x_lab)), tck=-0.05, line=-0.2)
  axis(1,x_lab, x_lab, cex.axis=0.6, lwd=0, line=-1)
  text(-0.8,1.5, "Effect of a", cex=0.8, f=2)
  text(-0.8,1.3, "change in", cex=0.8, f=2)
  text(-0.8,1.1, "Concavity", cex=0.8, f=2)
  
  ###################################################################
  # Bottom-left panel: Gamma 3 : Interaction effect
  ###################################################################
  hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
       ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
  lines(density(gamma[,3]), lwd=2, col=line.gamma3)
  mtext("Posterior density",side=2,line=0.3, cex=0.8, f=2)
  mtext(expression(bold(paste(gamma[3]))),side=1,line=1, cex=0.8)
  abline(v=0,lty=2,col=cutLine.color, lwd=2)
  axis(1,x_lim, c("",""), tck=-0, line=-0.2)
  axis(1,x_lab, rep("", length(x_lab)), tck=-0.05, line=-0.2)
  axis(1,x_lab, x_lab, cex.axis=0.6, lwd=0, line=-1)
  text(-1,1.5, "Interaction", cex=0.8, f=2)
  text(-1.025,1.3, "effect", cex=0.8, f=2)
  
  ###################################################################
  # Bottom-right panel: Posterior drift rate distributions
  ###################################################################
  data <- drift_pred
  binWidth = 0.1
  
  binStarts <- c(0.9,1.1,1.9,2.1)
  binMids <- binStarts + binWidth / 2
  
  DOYrange <- range(data)
  ## Get the histogram obects
  histList <- apply(data, 2, function(x, hCol) hist(x, plot = FALSE))
  
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
  xlabs <- c("Convexity", "Concavity")
  fillCol <- c("#42B6EC", "#0D41D5", "#42B6EC", "#0D41D5")
  CIcolor <- rep("#CAC7DA",4)
  
  plot(c(0, 5), DOYrange, type = "n", xlim=xlim, ylim=ylim,
       ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
  axis(1, 1.5, "Change type", cex.axis = 0.8, col = NA, line=0, f=2)
  axis(1, 1:2, xlabs, cex.axis = 0.7, col = NA, line=-1, f=1)
  axis(1, 1:2, c("",""), cex.axis = 0.7, tck=-0.04)
  y.seq = format(round(seq(-1.2,2,length.out=7),digits = 1), nsmall = 1)
  axis(2,ylim, c("",""), tck=-0, line=0)
  axis(2,y.seq, rep("", length(y.seq)), tck=-0.04, line=0)
  axis(2,y.seq, y.seq, cex.axis=0.6, lwd=0, line=-0.6, las=2)
  
  mtext(side = 2, outer = F, line = 1.3, expression(paste(nu^pred)), cex = 0.8)
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
    points(X, means[i], pch=18, cex=0.5)
    lines(x=c(X-0.05, X+0.05), y=c(means[i],means[i]), lwd=1)
  }
  lines(binStarts[c(1,3)], means[c(1,3)], lwd=1, lty=2)
  lines(binStarts[c(2,4)], means[c(2,4)], lwd=1, lty=2)
  text(1.1,1.85,"Change quality", cex=0.8, f=2)
  lines(c(0.59,0.64),c(1.55,1.55), lwd=3, col="#42B6EC")
  text(0.975,1.55,"Qualitative", cex=0.7, f=1)
  lines(c(0.59,0.64),c(1.25,1.25), lwd=3, col="#0D41D5")
  text(1.01,1.25,"Quantitative", cex=0.7, f=1)
}

###################################################################
# Generate all figures in PNG, PDF, and EPS formats
###################################################################

# Figure 1: Gamma posteriors and drift rate predictions
# PNG version
png(file = here("output", "figures", "paper_shapeExample_gammas.png"), width = 4, height = 3.2, units="in", res=300)
plot_gamma_posteriors_and_drift()
dev.off()

# PDF version
pdf(file = here("output", "figures", "paper_shapeExample_gammas.pdf"), width = 4, height = 3.2)
plot_gamma_posteriors_and_drift()
dev.off()

# EPS version
setEPS()
postscript(file = here("output", "figures", "paper_shapeExample_gammas.eps"), width = 4, height = 3.2)
plot_gamma_posteriors_and_drift()
dev.off()