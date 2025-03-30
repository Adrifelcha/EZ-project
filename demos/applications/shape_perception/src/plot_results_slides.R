###########################################################################################
# Load results and necessary packages
############################################################################################
# Load packages
library(here)
# Load custom functions
source(here("src", "plot_VerticalHist.R"))
# Load results
results_at <- here("output", "RData-results", "demo_shape_results.RData")
if(file.exists(results_at)){
  load(results_at)
} else {
  source(here("demos", "applications", "shape_perception", "src", "run_hypoTesting-example.R"))
}


###########################################################################################
# Prepare plotting variables and auxiliary functions
############################################################################################
# Custom function to select colors
myCol <- function(r,g,b,sub,alpha=1){
       rgb(r[sub]/255,g[sub]/255,b[sub]/255,alpha)
}

### Plotting functions
curve.gamma1 <- "#B2E5C7"
line.gamma1  <- "#419E67"
curve.gamma2 <- "#B6E5DB"
line.gamma2  <- "#2DAD92"
curve.gamma3 <- "#BED4E4"
line.gamma3  <- "#2975AC"
curve.gamma4 <- "#E4D4BE"
line.gamma4  <- "#AC7529"
curve.mu <- "#E4B6BE"
line.mu  <- "#AC2975"

binWidth = 0.1
binStarts <- c(0.9,1.1,1.9,2.1)
binMids <- binStarts + binWidth / 2

axisCol <- "gray30"
xlim <- c(0.5,2.5)
ylim <- c(-1.2,2.1)
xlabs <- c("Change in Convexity", "Change in Concavity")
fillCol <- c("#42B6EC", "#0D41D5", "#42B6EC", "#0D41D5")
CIcolor <- rep("#CAC7DA",4)

#### Process results
driftPred <- drift_pred[,1:4]
colnames(driftPred) <- c("QualConvex","QuantConvex","QualConcav","QuantConcav")
means <- apply(driftPred, 2, mean)
CI <- apply(driftPred, 2, quantile, prob=c(0.025,0.975))
DOYrange <- range(driftPred)
histList <- apply(driftPred, 2, function(x, hCol) hist(x, plot = FALSE))

shadeCI <- list()
for(i in 1:ncol(CI)){
  keep <- (driftPred[,i] > CI[1,i]) & (driftPred[,i] < CI[2,i])
  nBreaks <- sum(histList[[i]]$breaks >= CI[1,i] & histList[[i]]$breaks <= CI[2,i])
  partial <- hist(driftPred[keep,i], breaks = nBreaks, plot = FALSE)
  shadeCI <- append(shadeCI, list(partial))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 1: Main results (Gamma 1, 2, 3, and drift predictions)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_main_results <- function(){
     par(mar=c(3.5,1.5,1.5,1), bg=NA)
     layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE))

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
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 2: All posterior distributions (Mu, and Gamma 1 to 4)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_all_posteriors <- function(){
     layout(mat = matrix(c(1,1,2,2,3,3, 0,4,4,5,5,0), nrow = 2, byrow = TRUE))     
     par(pty="m", mai=c(0.45,0.5,0.25,0), oma= c(0,0,1,1.5), bg=NA)

     max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))
     hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,1]), lwd=4, col=line.gamma1)
     mtext("Quantitative change", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Density",side=2,line=2.15, cex=0.8)
     mtext("Gamma 1",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.55))
     lines(density(gamma[,2]), lwd=4, col=line.gamma2)
     mtext("Change in Concavity", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Gamma 2",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,3]), lwd=4, col=line.gamma3)
     mtext("Quantitative change in Concavity", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Interaction effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Gamma 3",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     max.Y <- c(max(density(gamma[,4])$y,density(mu)$y))+0.1
     hist(gamma[,4], freq = FALSE, breaks = 50, col=curve.gamma4, border = curve.gamma4, 
          ann=F, axes = T, ylim=c(0, max.Y))
     lines(density(gamma[,4]), lwd=4, col=line.gamma4)
     mtext("Change NOT occuring", cex=0.75, f=2, side=3, line=0.5)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=-0.6, col="gray30")
     mtext("Density",side=2,line=2.15, cex=0.8)
     mtext("Gamma 4",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(mu, freq = FALSE, breaks = 50, col=curve.mu, border = curve.mu, ann=F, 
          axes = T, ylim=c(0, max.Y))
     lines(density(mu), lwd=4, col=line.mu)
     mtext("Qualitative change in Convexity", cex=0.75, f=2, side=3, line=0.5)
     mtext("(Intercept; Baseline drift)", cex=0.75, f=2, side=3, line=-0.6, col="gray30")
     mtext("Mu",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 3: Main posterior distributions (Gamma 1, 2, 3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_mainGamma_posteriors <- function(){
     par(pty="m", mfrow=c(1,3), mai=c(0.45,0.5,0.25,0), oma= c(0,0,1,1.5), bg=NA)

     max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))
     hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,1]), lwd=4, col=line.gamma1)
     mtext("Quantitative change", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Density",side=2,line=2.15, cex=0.8)
     mtext("Gamma 1",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.55))
     lines(density(gamma[,2]), lwd=4, col=line.gamma2)
     mtext("Change in Concavity", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Gamma 2",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,3]), lwd=4, col=line.gamma3)
     mtext("Quantitative change in Concavity", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Interaction effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Gamma 3",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 4: Drift rate predictions per condition
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_drift_prediction <- function(){
     plot(c(0, 5), DOYrange, type = "n", xlim=xlim, ylim=ylim, ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
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
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 5 to 7: Posterior predictions of accuracy rate, mean RT, and RT variance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_posterior_predictions <- function(){

}

###########################################################################################
# Store plots to output folder
###########################################################################################

# Main results
png(file = here("output", "figures", "slides_shapeExample_mainResults.png"), width = 10, height = 6, units="in", res=300)
plot_main_results()
dev.off()
pdf(file = here("output", "figures", "slides_shapeExample_mainResults.pdf"), width = 10, height = 6)
plot_main_results()
dev.off()

# All posterior distributions
png(file = here("output", "figures", "slides_shapeExample_allPosteriors.png"), width = 10, height = 6, units="in", res=300)
plot_all_posteriors()
dev.off()
pdf(file = here("output", "figures", "slides_shapeExample_allPosteriors.pdf"), width = 10, height = 6)
plot_all_posteriors()
dev.off()

# Main posterior distributions (Gamma 1, 2, 3)
png(file = here("output", "figures", "slides_shapeExample_mainGammaPosteriors.png"), width = 7, height = 3, units="in", res=300)
plot_mainGamma_posteriors()
dev.off()
pdf(file = here("output", "figures", "slides_shapeExample_mainGammaPosteriors.pdf"), width = 7, height = 3)
plot_mainGamma_posteriors()
dev.off()

# Drift rate predictions per condition
png(file = here("output", "figures", "slides_shapeExample_driftPredictions.png"), width = 7, height = 3, units="in", res=300)
plot_drift_prediction()
dev.off()
pdf(file = here("output", "figures", "slides_shapeExample_driftPredictions.pdf"), width = 7, height = 3)
plot_drift_prediction()
dev.off()

