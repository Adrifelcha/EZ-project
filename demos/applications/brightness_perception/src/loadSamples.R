seed <- 15

##### Drift rate parameters
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

set.seed(seed)

# Number of trials per condition
nTrials <- df$nTrials
# Number of posterior samples
n <- nrow(Pc) 
# Number of conditions
J <- ncol(Pc)
# Empty matrices to store posterior predictions
pp_accRate <- matrix(NA, nrow=n, ncol=J)
pp_meanRT  <- matrix(NA, nrow=n, ncol=J)
pp_varRT   <- matrix(NA, nrow=n, ncol=J)
# Obtain posterior predictions using sampling distributions
#        and the summary statistics derived from the recovered
#        drift and boundary parameters
for(i in 1:J){
  correct  <-  rbinom(n,nTrials[i],Pc[,i])
  pp_accRate[,i] <- correct/nTrials[i]
  pp_varRT[,i]   <- rnorm(n,1/PRT[,i], sqrt(2/((nTrials[i]-1) * PRT[,i] * PRT[,i])))
  pp_meanRT[,i]  <- rnorm(n,MRT[,i],sqrt(1/(PRT[,i]*nTrials[i])))
}



# Identify conditions to plot on x axis
x_values <- unique(df$Xs)
# Insert a 'jump' in between these values
fit_x <- c(x_values[1:16],NA,x_values[17:32])
full_x <- c(fit_x,NA,fit_x+6.8)