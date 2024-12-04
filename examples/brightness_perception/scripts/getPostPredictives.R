seed <- 15

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