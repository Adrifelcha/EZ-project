###########################################################################################
# Individual plots showing relevant results
############################################################################################
load("../results/samples.RData")
source("./loadSamples.R")
source("./getPostPredictives.R")

makeLines <- function(posterior_object){
  x <- posterior_object
  line_acc <- c(x[1:16], NA,x[17:32])
  line_spd <- c(x[33:48],NA,x[49:64])
  full_line <- c(line_acc, NA, line_spd)
  return(full_line)
}

###################################################################
# Figure 1: Beta posteriors
###################################################################
pdf(file = "../../../figures/metaEx_poster.pdf", width = 4, height = 3.2)
par(mai=c(0.3,0.1,0,0.05), oma= c(0,1,0.2,0.1), bg=NA)
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE),heights = c(1, 1.5))
line.color    <- "#245BC3"
density.color <- "#B5BFDA"
cutLine.color <- "#0D3D6D"
x_lim <- c(-0.1,2)
x_lab <- seq(0,2,length.out=6)
topY <- max(density(beta4)[2]$y)*1.05
hist(beta3, freq = FALSE, breaks = 50, col=density.color, border = density.color, 
     ann=F, axes = F, xaxs = "i", yaxs = "i", ylim=c(0,topY), xlim=x_lim)
lines(density(beta3), lwd=2, col=line.color)
axis(1,x_lim, c("",""), tck=-0)
axis(1,x_lab, rep("", length(x_lab)), tck=-0.05)
axis(1,x_lab, c("", x_lab[-1]), cex.axis=0.5, lwd=0, line=-1)
axis(1,0, 0, cex.axis=0.5, lwd=0, line=-1, col.axis = cutLine.color)
mtext("Posterior density",side=2,line=0.6,cex=0.7, f=2)
text(1.4,2, "Effect of instruction", cex=0.7, f=2)
text(1.4,1.75, "on the slope", cex=0.7, f=2)
mtext(expression(paste(beta[3])),side=1,line=0.8,cex=0.7, f=2)
abline(v=0,lty=2, col=cutLine.color, lwd=2)

hist(beta4, freq = FALSE, breaks = 50, col=density.color, border = density.color, 
     ann=F, axes = F,xaxs = "i", yaxs = "i", xlim=x_lim, ylim = c(0,topY))
lines(density(beta4), lwd=2, col=line.color)
axis(1,x_lim, c("",""), tck=-0)
axis(1,x_lab, rep("", length(x_lab)), tck=-0.05)
axis(1,x_lab, c("", x_lab[-1]), cex.axis=0.5, lwd=0, line=-1)
axis(1,0, 0, cex.axis=0.5, lwd=0, line=-1, col.axis = cutLine.color)
text(1.5,2, "Main effect", cex=0.7, f=2)
text(1.5,1.75, "of instruction", cex=0.7, f=2)
mtext(expression(paste(beta[4])),side=1,line=0.8,cex=0.7)
abline(v=0,lty=2, col=cutLine.color, lwd=2)

###################################################################
# Figure 2: Predicted drift vs Recovered drift
###################################################################
acc.bckg <- "#DECFF6"
spd.bckg <- "#D2D4FF"
predicted.color  <- "#150D84"
recovered.color <- "#155CB3"
#### Get mean posterior estimates and percentiles for plotting
# Drift recovered
means <- apply(drift, 2, mean)
percentiles <- apply(drift,2, quantile, probs=c(0.025,0.975))
lower_percentiles <- percentiles[1,]  #  2.5%
upper_percentiles <- percentiles[2,]  # 97.5%
# Drift predictions (i.e., fitted values)
preds <- apply(drift_pred,2,mean)
means <- makeLines(means)
fit_line <- makeLines(preds)
# Lower and upper boundaries of the error bars
errors_acc <- cbind(c(lower_percentiles[1:16], NA, lower_percentiles[17:32]),
                    c(upper_percentiles[1:16], NA, upper_percentiles[17:32]))
errors_spd <- cbind(c(lower_percentiles[33:48],NA, lower_percentiles[49:64]),
                    c(upper_percentiles[33:48],NA, upper_percentiles[49:64]))
errors <- rbind(errors_acc,c(NA,NA),errors_spd)
#### Make plot
plot(full_x,fit_line, type="l", lwd=4, col=predicted.color, ann=F, axes=F,
     ylab="Drift rate", ylim=c(0,6),xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
# Gray background
polygon(c(-3.5,3.4,3.4,-3.5),c(0,0,6.5,6.5),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(0,0,6.5,6.5),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(0,0,6.5,6.5),col = "gray75", border = "gray75", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(0,0,6.5,6.5),col = "gray75", border = "gray75", lwd = 3, lty = "solid")
lines(full_x,fit_line,lwd=2,col=predicted.color)
arrows(full_x,errors[,1], full_x, errors[,2], length=0.03, angle=90, code=3, col=recovered.color, lwd=0.5)
points(full_x, means, col=recovered.color, pch=16, cex=0.4)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), lwd=0, line=-0.9, cex.axis=0.6)
axis(1,c(-2,0,2,5,7,9), rep("", length(c(-2,0,2,5,7,9))), tck=-0.03)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(1,c(0,6.5), c("",""), tck=-0)
axis(2,0:6,0:6, las=2, lwd=0, line=-0.6, cex.axis=0.5)
axis(2,0:6, rep("", length(0:6)), tck=-0.025, las=2)
text(-2,0.35,"Accuracy condition", cex=0.6)
text(9,0.35,"Speed condition", cex=0.6)
mtext("Drift rate", 2, line=0.8, cex=0.7, f=2)
mtext("Stimulus configuration", 1, line=0.85, cex=0.7, f=2)
#text(3.5,6.1,"Predicted and recovered drift rate per condition", f=2, cex=.85)
lines(c(-3.2,-2.7),c(5.7,5.7), lwd=2, col=predicted.color)
text(-1.35,5.7,"Predicted drift", cex=0.7, f=2)
lines(c(-3.2,-2.7),c(5.25,5.25), lwd=1, col=recovered.color)
text(-1.275,5.25,"Recovered drift", cex=0.7, f=2)
dev.off()

