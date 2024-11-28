###########################################################################################
# Individual plots showing relevant results
############################################################################################
load("../results/samples.RData")
source("./loadSamples.R")
source("./getPostPredictives.R")
acc.bckg <- "#F4E4FC"
spd.bckg <- "#E4EFE1"

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
pdf(file = "../../../figures/metaEx_betas_paper.pdf", width = 2, height = 1) 
par(mfrow = c(1,2), mai=c(0,0.05,0,0.05), oma= c(1.1,0.3,1,0), bg=NA)
line.color    <- "#DC9303"
density.color <- "#F3EAD8"
topY <- max(density(beta4)[2]$y)*1.05
hist(beta3, freq = FALSE, breaks = 50, col=density.color, border = density.color, ann=F, axes = F, xaxs = "i", yaxs = "i", ylim=c(0,topY))
lines(density(beta3), lwd=2, col=line.color)
axis(1,seq(0,2,0.4), rep("", length(seq(0,2,0.4))), tck=-0.05)
axis(1,seq(0,2,0.4), c("", seq(0.4,2,0.4)), cex.axis=0.3, lwd=0, line=-1.3)
axis(1,0, 0, cex.axis=0.3, lwd=0, line=-1.3, col.axis = "red")
mtext("Posterior density",side=2,line=-0,cex=0.35, f=2)
mtext("Effect on slope", f=1, cex=0.35, line=0.05)
mtext(expression(paste(beta[3])),side=1,line=0.2,cex=0.45)
abline(v=0,lty=2, col="red", lwd=0.5)

hist(beta4, freq = FALSE, breaks = 50, col=density.color, border = density.color, ann=F, axes = F,xaxs = "i", yaxs = "i", xlim=c(0,1.6), ylim = c(0,topY))
lines(density(beta4), lwd=2, col=line.color)
axis(1,seq(0,1.6,0.4), rep("", length(seq(0,1.6,0.4))), tck=-0.05)
axis(1,seq(0,1.6,0.4), c("", seq(0.4,1.6,0.4)), cex.axis=0.3, lwd=0, line=-1.3)
axis(1,0, 0, cex.axis=0.3, lwd=0, line=-1.3, col.axis = "red")
mtext("Main effect", f=1, cex=0.35, line=0.05)
mtext(expression(paste(beta[4])),side=1,line=0.2,cex=0.45)
abline(v=0,lty=2, col="red")
mtext("Effect of instruction on the drift rate",3,outer=TRUE, f=2, line=.4, cex=0.45)
dev.off()

###################################################################
# Figure 2: Predicted drift vs Recovered drift
###################################################################
predicted.color  <- "#FF8B0F"
recovered.color <- "#83460A"
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
pdf(file = "../../../figures/metaEx_driftPreds_paper.pdf", width = 2.5, height = 1.25) # Width and height of the plot in inches
par(pty="m", mfrow = c(1,1), mai=c(0,0,0,0), oma= c(1,1.2,0.5,0.1), bg=NA)
plot(full_x,fit_line, type="l", lwd=4, col=predicted.color, ann=F, axes=F,
     ylab="Drift rate", ylim=c(0,6),xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
# Gray background
polygon(c(-3.5,3.4,3.4,-3.5),c(0,0,6,6),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(0,0,6,6),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
lines(full_x,fit_line,lwd=2,col=predicted.color)
arrows(full_x,errors[,1], full_x, errors[,2], length=0.01, angle=90, code=3, col=recovered.color, lwd=0.5)
points(full_x, means, col=recovered.color, pch=16, cex=0.35)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), lwd=0, line=-1.2, cex.axis=0.4)
axis(1,c(-2,0,2,5,7,9), rep("", length(c(-2,0,2,5,7,9))), tck=-0.05)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,0:6,0:6, las=2, lwd=0, line=-0.6, cex.axis=0.4)
axis(2,0:6, rep("", length(0:6)), tck=-0.05, las=2)
text(-2.3,0.35,"Accuracy", cex=0.4)
text(9.3,0.35,"Speed", cex=0.4)
mtext("Drift rate", 2, line=0.55, cex=0.4, f=2)
mtext("Stimulus configuration", 1, line=2)
mtext("Predicted and recovered drift rate per condition",3,outer=TRUE, f=2, line=0, cex=.5)
dev.off()

###################################################################
# Figure 3: Posterior predictive checks
###################################################################
## Get mean posteriors and key percentiles
# Accuracy rate
pp.accRate <- apply(pp_accRate, 2, mean)
pp.perc.accRate <- apply(pp_accRate,2, quantile, probs=c(0.025,0.975))
## Concatenate per instruction condition, with a "jump" between pixel condition
est.accRate.Full   <- makeLines(pp.accRate)
accRate <- makeLines(df$acc_rate)
## Arrange lower and upper percentiles such that we can use polygon() to draw them
err.accRate.acc  <- c(pp.perc.accRate[1,1:16],  pp.perc.accRate[2,16:1],
                      pp.perc.accRate[1,17:32], pp.perc.accRate[2,32:17])
err.accRate.spd  <- c(pp.perc.accRate[1,33:48], pp.perc.accRate[2,48:33],
                      pp.perc.accRate[1,49:64], pp.perc.accRate[2,64:49])
data.color  <- "black"
point.size <- 0.9
error.color <- "#E69FF8"
pred.color <- "#BE07C1"
pdf(file = "../../../figures/metaEx_PostPredChecks.pdf", width = 2.5, height = 2.5) # Width and height of the plot in inches
par(pty="m", mfrow = c(3,1), mai=c(0.01,0,0.01,0), oma= c(1.5,2,1,0), bg=NA)
plot(full_x,est.accRate.Full, col="white", ann=F, axes=F, 
     ylim=c(0.51,1.04),xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
polygon(c(-3.5,3.4,3.4,-3.5),c(0,0,6,6),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(0,0,6,6),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(full_x[1:16],full_x[16:1]),err.accRate.acc[1:32],col = error.color, border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[18:33],full_x[33:18]),err.accRate.acc[33:64],col = error.color, border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[35:50],full_x[50:35]),err.accRate.spd[1:32],col = error.color, border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[52:67],full_x[67:52]),err.accRate.spd[33:64],col = error.color, border = NA, lwd = 3, lty = "solid")
lines(full_x,est.accRate.Full,col=pred.color, lwd=1.5)
points(full_x,accRate,pch=8,col=data.color, cex=0.2)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(0.55,1,length.out=5),round(seq(0.55,1,length.out=5),1), las=2, lwd=0, line=-0.5, cex.axis=0.55)
axis(2,seq(0.55,1,length.out=5), rep("", length(seq(0.55,1,length.out=5))), tck=-0.05, las=2)
text(-2.3, 0.55, "Accuracy", cex=0.65)
text(9.3, 0.55, "Speed", cex=0.65)
mtext("Accuracy rate", 2, line=1.2, cex=0.45, f=2)

## Get mean posteriors and key percentiles
# Mean RT-correct
pp.rtMean <- apply(pp_meanRT, 2, mean)
pp.perc.rtMean <- apply(pp_meanRT,2, quantile, probs=c(0.025,0.975))
## Concatenate per instruction condition, with a "jump" between pixel condition
est.rtMean.Full <- makeLines(pp.rtMean)
meanRT  <- makeLines(df$mean_rt)
## Arrange lower and upper percentiles such that we can use polygon() to draw them
# Mean RT-correct
err.rtMean.acc  <- c(pp.perc.rtMean[1,1:16],  pp.perc.rtMean[2,16:1],
                     pp.perc.rtMean[1,17:32], pp.perc.rtMean[2,32:17])
err.rtMean.spd  <- c(pp.perc.rtMean[1,33:48], pp.perc.rtMean[2,48:33], 
                     pp.perc.rtMean[1,49:64], pp.perc.rtMean[2,64:49])
error.color <- "#6DFA9C"
pred.color <- "#00A437"
point.size <- 0.9
plot(full_x,est.rtMean.Full, col="white", ann=F, axes=F, ylim=c(0.16,1.04), 
     xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
polygon(c(-3.5,3.4,3.4,-3.5),c(0,0,6,6),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(0,0,6,6),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(full_x[1:16],full_x[16:1]),err.rtMean.acc[1:32],col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[18:33],full_x[33:18]),err.rtMean.acc[33:64],col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[35:50],full_x[50:35]),err.rtMean.spd[1:32],col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[52:67],full_x[67:52]),err.rtMean.spd[33:64],col = error.color,
        border = NA, lwd = 3, lty = "solid")
lines(full_x,est.rtMean.Full,col=pred.color, lwd=1.5)
points(full_x,meanRT,pch=8,col=data.color, cex=0.2)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(0.2,1,length.out=5),round(seq(0.2,1,length.out=5),1), las=2, lwd=0, line=-0.5, cex.axis=0.55)
axis(2,seq(0.2,1,length.out=5), rep("", length(seq(0.2,1,length.out=5))), tck=-0.05, las=2)
mtext("Mean RT (secs)", 2, line=1.2, cex=0.45, f=2)

# RT-correct Variance
pp.rtVar <- apply(pp_varRT, 2, mean)
pp.perc.rtVar <- apply(pp_varRT,2, quantile, probs=c(0.025,0.975))
## Concatenate per instruction condition, with a "jump" between pixel condition
est.rtVar.Full  <- makeLines(pp.rtVar)
varRT   <- makeLines(df$variance_rt)
## Arrange lower and upper percentiles such that we can use polygon() to draw them
# RT-correct Variance
err.rtVar.acc  <- c(pp.perc.rtVar[1,1:16],  pp.perc.rtVar[2,16:1],
                    pp.perc.rtVar[1,17:32], pp.perc.rtVar[2,32:17])
err.rtVar.spd  <- c(pp.perc.rtVar[1,33:48], pp.perc.rtVar[2,48:33],
                    pp.perc.rtVar[1,49:64], pp.perc.rtVar[2,64:49])
error.color <- "#8CE0FF"
pred.color <- "#095B79"

plot(full_x,log(est.rtVar.Full), col="white", ann=F, axes=F, ylim=c(-7.5,-0.4),
     xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
polygon(c(-3.5,3.4,3.4,-3.5),c(-7.1,-7.1,-0.8,-0.8),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(-7.1,-7.1,-0.8,-0.8),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(-7.1,-7.1,-0.8,-0.8),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(-7.1,-7.1,-0.8,-0.8),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(full_x[1:16],full_x[16:1]),log(err.rtVar.acc[1:32]),col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[18:33],full_x[33:18]),log(err.rtVar.acc[33:64]),col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[35:50],full_x[50:35]),log(err.rtVar.spd[1:32]),col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[52:67],full_x[67:52]),log(err.rtVar.spd[33:64]),col = error.color,
        border = NA, lwd = 3, lty = "solid")
lines(full_x,log(est.rtVar.Full),col=pred.color, lwd=1.5)
points(full_x,log(varRT),pch=8,col=data.color, cex=0.2)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), lwd=0, line=-0.8, cex.axis=0.6)
axis(1,c(-2,0,2,5,7,9), rep("", length(c(-2,0,2,5,7,9))), tck=-0.05)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(-7.1,-0.8,length.out=5),round(seq(-7.1,-0.8,length.out=5),1), las=2, lwd=0, line=-0.5, cex.axis=0.55)
axis(2,seq(-7.1,-0.8,length.out=5), rep("", length(seq(-7.1,-0.8,length.out=5))), tck=-0.05, las=2)
mtext("log(RT variance)", 2, line=1.2, cex=0.45, f=2)
mtext("Posterior predictive checks",3,outer=TRUE, f=2, line=-0.1, cex=0.6)
dev.off()
