###########################################################################################
# Load results and define plotting variables
############################################################################################
# Load packages
library(here)
# Load results
results_at <- here("output", "RData-results", "demo_brightness_results.RData")
if(file.exists(results_at)){
  load(results_at)
} else {
  source(here("demos", "applications", "brightness_perception", "src", "run_metaregression-example.R"))
}

###########################################################################################
# Prepare plotting variables and auxiliary functions
############################################################################################

# Needed custom function
makeLines <- function(posterior_object){
  x <- posterior_object
  line_acc <- c(x[1:16], NA,x[17:32])
  line_spd <- c(x[33:48],NA,x[49:64])
  full_line <- c(line_acc, NA, line_spd)
  return(full_line)
}

# Prepare plotting colors
predicted.color  <- "#FF8B0F"
recovered.color <- "#83460A"
acc.bckg <- "#F4E4FC"
spd.bckg <- "#E4EFE1"
data.color  <- "black"

# Prepare data for plotting
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

# Compute posterior predictive checks
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
# >>>>> Accuracy rate
pp.accRate <- apply(pp_accRate, 2, mean)
pp.perc.accRate <- apply(pp_accRate,2, quantile, probs=c(0.025,0.975))
est.accRate.Full   <- makeLines(pp.accRate)
accRate <- makeLines(df$acc_rate)
err.accRate.acc  <- c(pp.perc.accRate[1,1:16],  pp.perc.accRate[2,16:1],
                      pp.perc.accRate[1,17:32], pp.perc.accRate[2,32:17])
err.accRate.spd  <- c(pp.perc.accRate[1,33:48], pp.perc.accRate[2,48:33],
                      pp.perc.accRate[1,49:64], pp.perc.accRate[2,64:49])
# >>>>>> Mean RT
pp.rtMean <- apply(pp_meanRT, 2, mean)
pp.perc.rtMean <- apply(pp_meanRT,2, quantile, probs=c(0.025,0.975))
est.rtMean.Full <- makeLines(pp.rtMean)
meanRT  <- makeLines(df$mean_rt)
err.rtMean.acc  <- c(pp.perc.rtMean[1,1:16],  pp.perc.rtMean[2,16:1],
                     pp.perc.rtMean[1,17:32], pp.perc.rtMean[2,32:17])
err.rtMean.spd  <- c(pp.perc.rtMean[1,33:48], pp.perc.rtMean[2,48:33], 
                     pp.perc.rtMean[1,49:64], pp.perc.rtMean[2,64:49])
# >>>>>> RT Variance
pp.rtVar <- apply(pp_varRT, 2, mean)
pp.perc.rtVar <- apply(pp_varRT,2, quantile, probs=c(0.025,0.975))
est.rtVar.Full  <- makeLines(pp.rtVar)
varRT   <- makeLines(df$variance_rt)
err.rtVar.acc  <- c(pp.perc.rtVar[1,1:16],  pp.perc.rtVar[2,16:1],
                    pp.perc.rtVar[1,17:32], pp.perc.rtVar[2,32:17])
err.rtVar.spd  <- c(pp.perc.rtVar[1,33:48], pp.perc.rtVar[2,48:33],
                    pp.perc.rtVar[1,49:64], pp.perc.rtVar[2,64:49])

############################################################################################
#   P A R T   1:                                       #####################################
#                 I N D I V I D U A L   P L O T S      #####################################
############################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 1: Beta posteriors
# This plot shows the posterior distributions for beta3 and beta4 side by side
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_beta_plot <- function() {
        par(mfrow = c(1,2), mai=c(0.75,0.2,0.6,0.15), oma= c(0,0,0,0), bg=NA)
        line.color    <- "#DC9303"
        density.color <- "#F3EAD8"
        topY <- max(density(beta4)[2]$y)*1.05
        
        # First plot
        hist(beta3, freq = FALSE, breaks = 50, col=density.color, border = density.color, 
                ann=F, axes = F, xaxs = "i", yaxs = "i", ylim=c(0,topY))
        lines(density(beta3), lwd=4, col=line.color)
        axis(1,seq(0,2,0.4),seq(0,2,0.4),cex.axis=0.7)
        mtext("Posterior density",side=2,line=-0,cex=0.9, f=2)
        mtext(expression(paste(beta[3])),side=1,line=2.5,cex=1)
        mtext("Effect on slope", f=2, cex=0.8, line=0.3)
        abline(v=0,lty=2)
        mtext("Effect of instruction on the drift rate",3,outer=TRUE, f=2, line=-1.5, cex=0.9)

        # Second plot
        hist(beta4, freq = FALSE, breaks = 50, col=density.color, border = density.color, 
                ann=F, axes = F,xaxs = "i", yaxs = "i", xlim=c(0,1.6), ylim = c(0,topY))
        lines(density(beta4), lwd=4, col=line.color,cex.axis=0.8)
        mtext(expression(paste(beta[4])),side=1,line=2.5,cex=1)
        axis(1,seq(0,1.6,0.4),seq(0,1.6,0.4),cex.axis=0.7)
        mtext("Main effect", f=2, cex=0.8, line=0.3)
        abline(v=0,lty=2)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 2: Predicted drift vs Recovered drift
# This plot shows the predicted drift rate vs the recovered drift rate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_drift_plot <- function(point.size = 1){
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
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 3: Posterior predictive check on the Accuracy rate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
error.color <- "#E69FF8"
pred.color <- "#BE07C1"

create_accRate_plot <- function(point.size = 0.9) {
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
}

# Save as PNG
png(file = here("output", "figures", "demo_Brightness_accRate_slides.png"), 
    width = 10, height = 6, units="in", res=300)
create_accRate_plot()
dev.off()

# Save as PDF
pdf(file = here("output", "figures", "demo_Brightness_accRate_slides.pdf"), 
    width = 10, height = 6)
create_accRate_plot()
dev.off()

###################################################################
# Figure 4: Posterior predictive check on the Mean RT
###################################################################
error.color <- "#6DFA9C"
pred.color <- "#00A437"

create_meanRT_plot <- function(point.size = 0.9) {
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
}

# Save as PNG
png(file = here("output", "figures", "demo_Brightness_meanRT_slides.png"), 
    width = 10, height = 6, units="in", res=300)
create_meanRT_plot()
dev.off()

# Save as PDF
pdf(file = here("output", "figures", "demo_Brightness_meanRT_slides.pdf"), 
    width = 10, height = 6)
create_meanRT_plot()
dev.off()

###################################################################
# Figure 4b: Posterior predictive check on the Mean RT (log scale)
###################################################################
create_meanRT_log_plot <- function() {
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
}

# Save as PNG
png(file = here("output", "figures", "demo_Brightness_meanRT_log_slides.png"), 
    width = 10, height = 6, units="in", res=300)
create_meanRT_log_plot()
dev.off()

# Save as PDF
pdf(file = here("output", "figures", "demo_Brightness_meanRT_log_slides.pdf"), 
    width = 10, height = 6)
create_meanRT_log_plot()
dev.off()



###################################################################
# Figure 5: Posterior predictive check on the Variance RT
###################################################################
error.color <- "#8CE0FF"
pred.color <- "#095B79"

create_varRT_plot <- function() {
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
}

# Save as PNG
png(file = here("output", "figures", "demo_Brightness_varRT_slides.png"), 
    width = 10, height = 6, units="in", res=300)
create_varRT_plot()
dev.off()

# Save as PDF
pdf(file = here("output", "figures", "demo_Brightness_varRT_slides.pdf"), 
    width = 10, height = 6)
create_varRT_plot()
dev.off()

#######################################################################
# Figure 5b: Posterior predictive check on the Variance RT (log scale)
#######################################################################
create_varRT_log_plot <- function() {
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
}

# Save as PNG
png(file = here("output", "figures", "demo_Brightness_varRT_log_slides.png"), 
    width = 10, height = 6, units="in", res=300)
create_varRT_log_plot()
dev.off()

# Save as PDF
pdf(file = here("output", "figures", "demo_Brightness_varRT_log_slides.pdf"), 
    width = 10, height = 6)
create_varRT_log_plot()
dev.off()


###########################################################################################
# 2x2 panel showing model predictions per stimulus configuration condition
# Panel 1: Predicted drift vs Recovered drift
# Panel 2: Posterior predictive check Accuracy rate
# Panel 3: Posterior predictive check Mean RT
# Panel 4: Posterior predictive check Variance RT
############################################################################################


create_predictions_plot <- function() {
par(pty="m", mfrow = c(2,2), mai=c(0.3,0.7,0.1,0.1), oma= c(0,0,0,0), bg=NA)

###################################################################
# Panel 1: Predicted drift vs Recovered drift
###################################################################
point.size <- 1

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
}


########################################
# Store plots into figure files
########################################

# Save as PNG
png(file = here("output", "figures", "demo_Brightness_betas_slides.png"),
    width = 10, height = 6, units="in", res=300)
create_beta_plot()
dev.off()

# Save as PDF
pdf(file = here("output", "figures", "demo_Brightness_betas_slides.pdf"),
    width = 10, height = 6)
create_beta_plot()
dev.off()

# Save as PNG
png(file = here("output", "figures", "demo_Brightness_driftPreds_slides.png"), 
    width = 10, height = 6, units="in", res=300)
create_drift_plot()
dev.off()

# Save as PDF
pdf(file = here("output", "figures", "demo_Brightness_driftPreds_slides.pdf"), 
    width = 10, height = 6)
create_drift_plot()
dev.off()

# Save as PNG
png(file = here("output", "figures", "demo_Brightness_predictions_slides.png"), width = 10, height = 6, units="in",res=300) 
create_predictions_plot()
dev.off()

# Save as PDF
pdf(file = here("output", "figures", "demo_Brightness_predictions_slides.pdf"), width = 10, height = 6)
create_predictions_plot()
dev.off()


