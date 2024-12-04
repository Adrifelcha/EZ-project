###########################################################################################
# 2x2 panel showing model predictions per stimulus configuration condition
# Panel 1: Predicted drift vs Recovered drift
# Panel 2: Posterior predictive check Accuracy rate
# Panel 3: Posterior predictive check Mean RT
# Panel 4: Posterior predictive check Variance RT
############################################################################################
load("../results/samples.RData")
source("./loadSamples.R")
source("./getPostPredictives.R")


png(file = "../../../figures/percEx_predictions.png", width = 10, height = 6, units="in",res=300) 
par(pty="m", mfrow = c(2,2), mai=c(0.3,0.7,0.1,0.1), oma= c(0,0,0,0), bg=NA)

###################################################################
# Panel 1: Predicted drift vs Recovered drift
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
#axis(1,c(-2,0,2,5,7,9),rep(c("More black","50/50","More white"),2), line = 0)
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,0:6,0:6, las=2)
#text(2.3, 0.53, "Accuracy")
#text(4.2, 0.53, "Speed")
mtext("Drift rate", 2, f=2, line=2.3, cex=1.5)
legend("topleft", c("Predicted", "Recovered"), col=c(predicted.color, recovered.color), cex=0.9, bty = "n", lwd=c(4,NA), pch=c(NA,16), bg = spd.bckg)

###################################################################
# Panel 2: Posterior predictive check on the Accuracy rate
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
plot(full_x,est.accRate.Full, col="white", ann=F, axes=F, 
     ylim=c(0.5,1.02),xaxs = "i", yaxs = "i", xlim=c(-3.5,10.3))
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
axis(1,c(-4,10.3),c("",""), line = 0, col.ticks = "white")
axis(2,seq(0,1,0.1),seq(0,1,0.1), las=2)
mtext("Accuracy rate",2, f=2, line=2.6, cex=1.5)
legend("bottomright", c("Posterior predictions", "Data"), col=c(error.color, data.color), cex=0.9, bty = "n", lwd=c(10,NA), pch=c(NA,8), bg = spd.bckg)

###################################################################
# Panel 3: Posterior predictive check on the Mean RT
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
#text(2.1, 0.95, "Accuracy")
#text(4.2, 0.95, "Speed")
mtext("Mean RT (secs)", 2, line=2.6, f=2, cex=1.5)
legend("topright", c("Posterior predictions", "Data"), col=c(error.color, data.color), cex=0.9, bty = "n", lwd=c(10,NA), pch=c(NA,8), bg = spd.bckg)

###################################################################
# Panel 4: Posterior predictive check on the Variance RT
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
mtext("log(RT variance)", 2, line=2.6, f=2, cex=1.5)
legend("topright", c("Posterior predictions", "Data"), col=c(error.color, data.color), cex=0.9, bty = "n", lwd=c(10,NA), pch=c(NA,8), bg = spd.bckg)
dev.off()

