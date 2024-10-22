###########################################################################################
# Individual plots showing relevant results
############################################################################################
load("../results/samples.RData")
source("./loadSamples.R")
source("./getPostPredictives.R")
acc.bckg <- "#F4E4FC"
spd.bckg <- "#E4EFE1"


###################################################################
# Figure 1: Beta posteriors
###################################################################
png(file = "../../../figures/percEx_betas.png", width = 2, height = 1.5, units="in",res=300) # Width and height of the plot in inches
par(mfrow = c(1,2), mai=c(0.6,0.15,0.5,0.1), oma= c(0,0,0,0), bg=NA)
line.color    <- "#DC9303"
density.color <- "#F3EAD8"
topY <- max(density(beta4)[2]$y)*1.05
hist(beta3, freq = FALSE, breaks = 50, col=density.color, border = density.color, ann=F, axes = F, xaxs = "i", yaxs = "i", ylim=c(0,topY))
lines(density(beta3), lwd=2, col=line.color)
axis(1,seq(0,2,0.4), rep("", length(seq(0,2,0.4))), tck=-0.05)
axis(1,seq(0,2,0.4), seq(0,2,0.4), cex.axis=0.5, lwd=0, line=-1)
mtext("Posterior density",side=2,line=-0,cex=0.6, f=2)
mtext(expression(paste(beta[3])),side=1,line=0.9,cex=0.65)
mtext("Effect on slope", f=2, cex=0.65, line=0.3)
abline(v=0,lty=2)
mtext("Effect of instruction on the drift rate",3,outer=TRUE, f=2, line=-1.5, cex=0.7)

hist(beta4, freq = FALSE, breaks = 50, col=density.color, border = density.color, ann=F, axes = F,xaxs = "i", yaxs = "i", xlim=c(0,1.6), ylim = c(0,topY))
lines(density(beta4), lwd=2, col=line.color,cex.axis=0.8)
mtext(expression(paste(beta[4])),side=1,line=0.9,cex=0.65)
axis(1,seq(0,1.6,0.4), rep("", length(seq(0,1.6,0.4))), tck=-0.05)
axis(1,seq(0,1.6,0.4), seq(0,1.6,0.4), cex.axis=0.5, lwd=0, line=-1)
mtext("Main effect", f=2, cex=0.65, line=0.3)
abline(v=0,lty=2)
dev.off()

###################################################################
# Figure 2: Predicted drift vs Recovered drift
###################################################################
predicted.color  <- "#FF8B0F"
recovered.color <- "#83460A"
point.size <- 1
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
png(file = "../../figures/percEx_driftPreds.png", width = 10, height = 6, units="in",res=300) # Width and height of the plot in inches
par(pty="m", mfrow = c(1,1), mai=c(0.65,0.6,0.55,0.1), oma= c(0,0,0,0), bg=NA)
plot(full_x,fit_line, type="l", lwd=4, col=predicted.color, ann=F, axes=F,
     ylab="Drift rate", ylim=c(0,6),xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
# Gray background
polygon(c(-3.5,3.4,3.4,-3.5),c(0,0,6,6),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(0,0,6,6),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
lines(full_x,fit_line,lwd=4,col=predicted.color)
arrows(full_x,errors[,1], full_x, errors[,2], length=0.05, angle=90, code=3, col=recovered.color)
points(full_x, means, col=recovered.color, pch=16, cex=point.size)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), line = 0)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,0:6,0:6, las=2)
mtext("Drift rate", 2, line=2)
text(-2,0.35,"Accuracy", cex=1.5)
mtext("Stimulus configuration", 1, line=2)
text(9,0.35,"Speed", cex=1.5)
mtext("Predicted and recovered drift rate per condition",3,outer=TRUE, f=2, line=-2, cex=1.5)
dev.off()

###################################################################
# Figure 3: Posterior predictive check on the Accuracy rate
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
png(file = "../../figures/percEx_accPostPred.png", width = 10, height = 6, units="in",res=300) # Width and height of the plot in inches
par(pty="m", mfrow = c(1,1), mai=c(0.9,0.6,0.5,0.1), oma= c(0,0,0,0), bg=NA)
plot(full_x,est.accRate.Full, col="white", ann=F, axes=F, 
     ylim=c(0.45,1.05),xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
polygon(c(-3.5,3.4,3.4,-3.5),c(0,0,6,6),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(0,0,6,6),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(full_x[1:16],full_x[16:1]),err.accRate.acc[1:32],col = error.color, border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[18:33],full_x[33:18]),err.accRate.acc[33:64],col = error.color, border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[35:50],full_x[50:35]),err.accRate.spd[1:32],col = error.color, border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[52:67],full_x[67:52]),err.accRate.spd[33:64],col = error.color, border = NA, lwd = 3, lty = "solid")
lines(full_x,est.accRate.Full,col=pred.color, lwd=3)
points(full_x,accRate,pch=8,col=data.color, cex=0.7)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), line = 0)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(0,1,0.1),seq(0,1,0.1), las=2)
text(2.5, 0.47, "Accuracy")
text(4.2, 0.47, "Speed")
mtext("Stimulus configuration", 1, line=3)
abline(h=0.5,lty=2, col="gray50")
text(9,0.52,"50% accuracy",col="gray50", cex=0.7)
mtext("Accuracy rate per condition",3,outer=TRUE, f=2, line=-2, cex=1.5)
dev.off()

###################################################################
# Figure 4: Posterior predictive check on the Mean RT
###################################################################
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
png(file = "../../figures/percEx_meanRTpostPred.png", width = 10, height = 6, units="in",res=300) # Width and height of the plot in inches
par(pty="m", mfrow = c(1,1), mai=c(0.9,0.6,0.5,0.1), oma= c(0,0,0,0), bg=NA)
plot(full_x,est.rtMean.Full, col="white", ann=F, axes=F, ylim=c(0.2,1), 
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
lines(full_x,est.rtMean.Full,col=pred.color, lwd=3)
points(full_x,meanRT,pch=8,col=data.color, cex=0.7)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), line = 0)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(0,1,0.1),seq(0,1,0.1), las=2)
mtext("Mean RT (secs)", 2, line=2.2)
mtext("Stimulus configuration", 1, line=3)
text(2.5, 0.95, "Accuracy")
text(4.2, 0.95, "Speed")
mtext("Mean RT per condition",3,outer=TRUE, f=2, line=-2, cex=1.8)
dev.off()

###################################################################
# Figure 4b: Posterior predictive check on the Mean RT (log scale)
###################################################################
png(file = "../../figures/percEx_meanRTpostPred_log.png", width = 10, height = 6, units="in",res=300) # Width and height of the plot in inches
par(pty="m", mfrow = c(1,1), mai=c(0.9,0.6,0.5,0.1), oma= c(0,0,0,0), bg=NA)
est.rtMean.log <- log(est.rtMean.Full)
logmeanRT <- log(meanRT)
plot(full_x,est.rtMean.log, col="white", ann=F, axes=F, ylim=c(-1.25,0), 
     xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
polygon(c(-3.5,3.4,3.4,-3.5),c(0,0,6,6),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(0,0,6,6),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(-1.25,-1.25,0,0),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(-1.25,-1.25,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(full_x[1:16],full_x[16:1]),log(err.rtMean.acc[1:32]),col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[18:33],full_x[33:18]),log(err.rtMean.acc[33:64]),col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[35:50],full_x[50:35]),log(err.rtMean.spd[1:32]),col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[52:67],full_x[67:52]),log(err.rtMean.spd[33:64]),col = error.color,
        border = NA, lwd = 3, lty = "solid")
lines(full_x,est.rtMean.log,col=pred.color, lwd=3)
points(full_x,logmeanRT,pch=8,col=data.color, cex=0.7)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), line = 0)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(-1.25,0,0.2),round(seq(-1.25,0,0.2),1), las=2)
mtext("log(MeanRT)", 2, line=2.2)
mtext("Stimulus configuration", 1, line=3)
text(2.5, 0.95, "Accuracy")
text(4.2, 0.95, "Speed")
mtext("log Mean RT per condition",3,outer=TRUE, f=2, line=-2, cex=1.8)
dev.off()



###################################################################
# Figure 5: Posterior predictive check on the Variance RT
###################################################################
## Get mean posteriors and key percentiles
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
png(file = "../../figures/percEx_varRTpostPred.png", width = 10, height = 6, units="in",res=300) # Width and height of the plot in inches
par(pty="m", mfrow = c(1,1), mai=c(0.9,0.6,0.5,0.1), oma= c(0,0,0,0), bg=NA)
plot(full_x,est.rtVar.Full, col="white", ann=F, axes=F, ylim=c(0,0.4),
     xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
polygon(c(-3.5,3.4,3.4,-3.5),c(0,0,6,6),col = acc.bckg, border = acc.bckg, lwd = 1, lty = "solid")
polygon(c(3.4,10.3,10.3,3.4),c(0,0,6,6),col = spd.bckg, border = spd.bckg, lwd = 1, lty = "solid")
polygon(c(-0.04,0.04,0.04,-0.04),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(6.76,6.84,6.84,6.76),c(0,0,6,6),col = "gray85", border = "gray85", lwd = 3, lty = "solid")
polygon(c(full_x[1:16],full_x[16:1]),err.rtVar.acc[1:32],col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[18:33],full_x[33:18]),err.rtVar.acc[33:64],col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[35:50],full_x[50:35]),err.rtVar.spd[1:32],col = error.color,
        border = NA, lwd = 3, lty = "solid")
polygon(c(full_x[52:67],full_x[67:52]),err.rtVar.spd[33:64],col = error.color,
        border = NA, lwd = 3, lty = "solid")
lines(full_x,est.rtVar.Full,col=pred.color, lwd=3)
points(full_x,varRT,pch=8,col=data.color, cex=0.7)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), line = 0)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(0,1,0.1),seq(0,1,0.1), las=2)
mtext("RT variance", 2, line=2.2)
text(2.5, 0.37, "Accuracy")
text(4.2, 0.37, "Speed")
mtext("Stimulus configuration", 1, line=3)
mtext("RT Variance per condition",3,outer=TRUE, f=2, line=-2, cex=1.8)
dev.off()

#######################################################################
# Figure 5b: Posterior predictive check on the Variance RT (log scale)
#######################################################################
png(file = "../../figures/percEx_varRTpostPred_log.png", width = 10, height = 6, units="in",res=300) # Width and height of the plot in inches
par(pty="m", mfrow = c(1,1), mai=c(0.9,0.6,0.5,0.1), oma= c(0,0,0,0), bg=NA)
plot(full_x,log(est.rtVar.Full), col="white", ann=F, axes=F, ylim=c(-7.1,-0.8),
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
lines(full_x,log(est.rtVar.Full),col=pred.color, lwd=3)
points(full_x,log(varRT),pch=8,col=data.color, cex=0.7)
axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), line = 0)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(-7.1,-0.8,0.6),seq(-7.1,-0.8,0.6), las=2)
mtext("RT variance - log scale", 2, line=2.4)
text(2, -6.9, "Accuracy")
text(4.5, -6.9, "Speed")
mtext("Stimulus configuration", 1, line=3)
mtext("RT Variance per condition",3,outer=TRUE, f=2, line=-2, cex=1.5)
mtext("(Log scale)",3,outer=TRUE, f=2, line=-2, cex=1, col="gray50")
dev.off()
```