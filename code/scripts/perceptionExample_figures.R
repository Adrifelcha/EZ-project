# Load necessary libraries/packages
library(R2jags)
set.seed(15)

###################################################################
# Load and prepare data
###################################################################
# Load the data from one participant
data_raw <- read.csv("../../examples/perception/nh.tsv", sep = "")
colnames(data_raw) <- c("index","cond","response","RT")
head(data_raw)
# No. of observations
nrow(data_raw)
# Create a copy of the raw data file 
data <- data_raw
# Get 'accuracy' binary coding based on the condition and response in raw data file
accuracy <- as.integer(data_raw$cond > 0  & data_raw$cond < 17 & data_raw$response==1|
                       data_raw$cond > 17 & data_raw$cond < 34 & data_raw$response==2|
                       data_raw$cond > 33 & data_raw$cond < 50 & data_raw$response==1|
                       data_raw$cond > 50 & data_raw$cond < 67 & data_raw$response==2)
# Update the 'response' column of the data copied so it reflects accuracy
data$response <- accuracy
# Remove rows where RT > 3000ms
data <- data[which(data$RT<=3000),]

###################################################################
# Compute EZ summary statistics
###################################################################
# Define a function to compute the summary statistics used by EZ-DDM
ez_summaries <- function(subset){
  # Identify condition ID
  cond <- unique(subset$cond)
  # Return relevant summary statistics
  return(data.frame("nTrials" = nrow(subset),
                    "score" = sum(subset$response),
                    "meanRT"  = mean(subset$RT/1000),
                    "varRT"   = var(subset$RT/1000),
                  # Index variable: Accuracy (-0.5) vs Speed (0.5) condition
                    "Xi"  = as.integer(cond>33)-0.5,
                  # Arbitrary scale of stimulus configuration | 0 is 50/50 black and white 
                    "Xs"  = ((cond-1) %% 33 - 16)/5))
}
# Initialize an empty output data frame (df)
tmp <- matrix(0,nrow = max(data$cond),ncol = 6)
df <- as.data.frame(tmp)
colnames(df) <- c("nTrials", "sum_accuracy", "mean_rt",
                  "variance_rt", "Xi", "Xs")
# Populate the df output using the ez_summaries function
for(i in 1:max(data$cond)){
  df[i,] <- ez_summaries(data[which(data$cond==i),])
}
# Remove the two ambiguous conditions (17 and 50, with 50/50 black and white)
df <- df[-which(df$Xs==0),]
head(df,3)
# Compute accuracy rate per condition from the summary data
df$acc_rate <- df$sum_accuracy/df$nTrials

###################################################################
# Write JAGS model
###################################################################
model <- write("
model {
        ##### Priors for hierarchical DDM parameters
        betaweight ~ dnorm(0.00, 1.00)
        beta0 ~ dnorm(0.00, 1.00)
        beta1 ~ dnorm(0.00, 1.00)
        beta2 ~ dnorm(0.00, 1.00)
        beta3 ~ dnorm(0.00, 1.00)
        beta4 ~ dnorm(0.00, 1.00)
        bound_mean ~ dnorm(1.50, (0.20^-2))T( 0.10, 3.00)
        drift_mean ~ dnorm(0.50, (0.50^-2))
        nondt_mean ~ dnorm(0.30, (0.06^-2))T( 0, )
        bound_sdev ~ dunif(0.01, 1.00)
        drift_sdev ~ dunif(0.01, 3.00)
        nondt_sdev ~ dunif(0.01, 0.50)
        
        # Hierarchical distributions of individual DDM parameters.        
        for (p in 1:length(meanRT)) {
            # Here, we focus on the drift rate
            drift_pred[p] = beta0*phi(beta1 + beta2*abs(Xs[p]) + beta3*Xi[p]*abs(Xs[p])) + beta4 * Xi[p] + drift_mean
            drift[p] ~ dnorm(drift_pred[p], (drift_sdev^-2))
            bound_pred[p] = bound_mean + betaweight * Xi[p]
            bound[p] ~ dnorm(bound_pred[p],(bound_sdev^-2))T( 0.10, 3.00)
            nondt[p] ~ dnorm(nondt_mean, (nondt_sdev^-2))T( 0.05, )
        
            # Forward equations from EZ DDM
            ey[p]  = exp(-bound[p] * drift[p])
            Pc[p]  = 1 / (1 + ey[p])
            PRT[p] = 2 * pow(drift[p], 3) / bound[p] * 
                     pow(ey[p] + 1, 2) / (2 * -bound[p] * 
                     drift[p] * ey[p] - ey[p] * ey[p] + 1)
            MDT[p] = (bound[p] / (2 * drift[p])) * 
                     (1 - ey[p]) / (1 + ey[p])
            MRT[p] = MDT[p] + nondt[p]
            
            # Noiseless predictions from forward EZ DDM
            ey_pred[p]  = exp(-bound_pred[p] * drift_pred[p])
            Pc_pred[p]  = 1 / (1 + ey_pred[p])
            PRT_pred[p] = 2 * pow(drift_pred[p], 3) / bound_pred[p] * 
                     pow(ey_pred[p] + 1, 2) / (2 * -bound_pred[p] * 
                     drift_pred[p] * ey_pred[p] - ey_pred[p] * ey_pred[p] + 1)
            MDT_pred[p] = (bound_pred[p] / (2 * drift_pred[p])) * 
                     (1 - ey_pred[p]) / (1 + ey_pred[p])
            MRT_pred[p] = MDT_pred[p] + nondt_mean
        
            # Sampling distributions for summary statistics
            correct[p] ~ dbin(Pc[p], nTrials[p])
            varRT[p]   ~ dnorm(1/PRT[p], 0.5*(nTrials[p]-1) 
                                         * PRT[p] * PRT[p])
            meanRT[p]  ~ dnorm(MRT[p], PRT[p] * nTrials[p])
      }
}", "./model_perception.bug")

###################################################################
# JAGS set up
###################################################################
# General setup
n.chains  <- 4;      n.iter    <- 5000
n.burnin  <- 250;    n.thin    <- 1
# Pass data to JAGS
data_toJAGS <- list("nTrials"  =  df$nTrials,
                    "meanRT"   =  df$mean_rt,
                    "varRT"    =  df$variance_rt,
                    "correct"  =  df$sum_accuracy,
                    "Xi"   =  df$Xi, "Xs"   =  df$Xs)
# Specify parameters to keep track of
parameters <- c('beta3', 'beta4', 'drift', 'drift_pred',
                "Pc_pred", "MRT_pred", "PRT_pred", "Pc", "PRT", "MRT")
# Prepare initial values
myinits <- rep(list(list()), n.chains)
for(i in 1:n.chains){
    myinits[[i]] <- list(drift = rnorm(length(data_toJAGS$nTrials),0,0.1))
}

###################################################################
# Run JAGS
###################################################################
samples <- jags(data=data_toJAGS,
                parameters.to.save=parameters,
                model="./model_perception.bug",
                n.chains=n.chains,  n.iter=n.iter,
                n.burnin=n.burnin,  n.thin=n.thin,
                DIC=T, inits=myinits)

########################################################################
# Load samples
########################################################################
################################################## Drift rate parameters
# Recovered drift rates
drift <- samples$BUGSoutput$sims.list$drift
# Effects of instruction
beta3 <- as.vector(samples$BUGSoutput$sims.list$beta3) # Main
beta4 <- samples$BUGSoutput$sims.list$beta4 # Interaction
# Fitted values / Predicted drift rates
drift_pred <- samples$BUGSoutput$sims.list$drift_pred
##### Summary statistics predicted from the predicted drift and boundary
accRate_hat   <- samples$BUGSoutput$sims.list$Pc_pred
rtMean_hat <- samples$BUGSoutput$sims.list$MRT_pred
rtVar_hat  <- 1/samples$BUGSoutput$sims.list$PRT_pred
##### Summary statistics computed from the recovered drift and boundary
Pc   <- samples$BUGSoutput$sims.list$Pc
PRT  <- samples$BUGSoutput$sims.list$PRT
MRT  <- samples$BUGSoutput$sims.list$MRT
###################################################################
# Get posterior predictions from the model
###################################################################
# Get dimensions
n <- nrow(Pc)  # Number of posterior samples
J <- ncol(Pc)  # Number of conditions
# Empty matrices to store posterior predictions
pp_accRate <- matrix(NA, nrow=n, ncol=J)
pp_meanRT  <- matrix(NA, nrow=n, ncol=J)
pp_varRT   <- matrix(NA, nrow=n, ncol=J)
# Obtain posterior predictions using sampling distributions
#        and the summary statistics derived from the recovered
#        drift and boundary parameters
for(i in 1:J){
  correct  <-  rbinom(n,df$nTrials[i],Pc[,i])
  pp_accRate[,i] <- correct/df$nTrials[i]
  pp_varRT[,i]   <- rnorm(n,1/PRT[,i], sqrt(2/((df$nTrials[i]-1) * PRT[,i] * PRT[,i])))
  pp_meanRT[,i]  <- rnorm(n,MRT[,i],sqrt(1/(PRT[,i]*df$nTrials[i])))
}
###################################################################
# Define plotting variables
###################################################################
# Identify conditions to plot on x axis
x_values <- unique(df$Xs)
# Insert a 'jump' in between these values
fit_x <- c(x_values[1:16],NA,x_values[17:32])
full_x <- c(fit_x,NA,fit_x+6.8)
# Define background colors for the "Accuracy" and "Speed" conditions
acc.bckg <- "#F4E4FC"
spd.bckg <- "#E4EFE1"
### Concatenate values by instruction and add a "jump" between pixel condition
makeLines <- function(posterior_object){
  x <- posterior_object
  line_acc <- c(x[1:16], NA,x[17:32])
  line_spd <- c(x[33:48],NA,x[49:64])
  full_line <- c(line_acc, NA, line_spd)
  return(full_line)
}

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


###################################################################
# Figure 1: Beta posteriors
###################################################################
png(file = "../../figures/percEx_betas.png", width = 7, height = 5, units="in",res=1200) # Width and height of the plot in inches
par(mfrow = c(1,2), mai=c(1.1,0.6,1,0), oma= c(0,0,0,0))
line.color    <- "#DC9303"
density.color <- "#F3EAD8"
source("../functions/plot_perceptionExBetas.R")
dev.off()

###################################################################
# Figure 2: Predicted drift vs Recovered drift
###################################################################
par(pty="m", mfrow = c(1,1), mai=c(1.1,0.6,0.5,0), oma= c(0,0,0,0))
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
png(file = "../../figures/percEx_driftPreds.png", width = 7, height = 5, units="in",res=1200) # Width and height of the plot in inches
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
mtext("Stimulus configuration", 1, line=3)
text(9,0.35,"Speed", cex=1.5)
mtext("Predicted and recovered drift rate per condition",3,outer=TRUE, f=2, line=-3, cex=1.5)
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

par(pty="m", mfrow = c(1,1), mai=c(1.1,0.6,0.5,0), oma= c(0,0,0,0))
data.color  <- "black"
point.size <- 0.9
error.color <- "#E69FF8"
pred.color <- "#BE07C1"
png(file = "../../figures/percEx_accPostPred.png", width = 7, height = 5, units="in",res=1200) # Width and height of the plot in inches
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
    mtext("Accuracy rate per condition",3,outer=TRUE, f=2, line=-3, cex=1.5)
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
par(pty="m", mfrow = c(1,1), mai=c(1.1,0.6,0.5,0), oma= c(0,0,0,0))
error.color <- "#6DFA9C"
pred.color <- "#00A437"
point.size <- 0.9
png(file = "../../figures/percEx_meanRTpostPred.png", width = 7, height = 5, units="in",res=1200) # Width and height of the plot in inches
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
    mtext("Mean RT per condition",3,outer=TRUE, f=2, line=-1.5, cex=1.8)
dev.off()

###################################################################
# Figure 4b: Posterior predictive check on the Mean RT (log scale)
###################################################################
png(file = "../../figures/percEx_meanRTpostPred_log.png", width = 7, height = 5, units="in",res=1200) # Width and height of the plot in inches
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
par(pty="m", mfrow = c(1,1), mai=c(1.1,0.6,0.5,0), oma= c(0,0,0,0))
error.color <- "#8CE0FF"
pred.color <- "#095B79"
png(file = "../../figures/percEx_varRTpostPred.png", width = 7, height = 5, units="in",res=1200) # Width and height of the plot in inches
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
    mtext("RT Variance per condition",3,outer=TRUE, f=2, line=-1.5, cex=1.8)
dev.off()

#######################################################################
# Figure 5b: Posterior predictive check on the Variance RT (log scale)
#######################################################################
png(file = "../../figures/percEx_varRTpostPred_log.png", width = 7, height = 5, units="in",res=1200) # Width and height of the plot in inches
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
mtext("(Log scale)",3,outer=TRUE, f=2, line=-3.5, cex=1, col="gray50")
dev.off()