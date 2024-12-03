#####################################################################
# Functions to automatically write desired JAGS models
#####################################################################

# A function to write the JAGS model using the priors values
writefixEffJAGSmodel <- function(priors, modelFile){
  opening <- "model{"
  priors.bound_m  <- paste("          bound_mean ~ dnorm(", priors$bound_mean_mean,",pow(",priors$bound_mean_sdev,",-2))T(0.10,)", sep="")
  priors.nondt_m  <- paste("          nondt_mean ~ dnorm(", priors$nondt_mean_mean,",pow(",priors$nondt_mean_sdev,",-2))T(0.05,)", sep="")
  priors.drift_m  <- paste("          drift_mean ~ dnorm(", priors$drift_mean_mean,",pow(",priors$drift_mean_sdev,",-2))", sep="")
  priors.bound_sd <- paste("          bound_sdev ~ dunif(", priors$bound_sdev_lower,",",priors$bound_sdev_upper,")", sep="")
  priors.nondt_sd <- paste("          nondt_sdev ~ dunif(", priors$nondt_sdev_lower,",",priors$nondt_sdev_upper,")", sep="")
  priors.drift_sd <- paste("          drift_sdev ~ dunif(", priors$drift_sdev_lower,",",priors$drift_sdev_upper,")", sep="")
  priors.beta     <- paste("          betaweight ~ dnorm(", priors$betaweight_mean,",pow(",priors$betaweight_sdev,",-2))", sep="")
  priorss <- c(priors.bound_m, priors.bound_sd, priors.nondt_m, priors.nondt_sd, priors.drift_m, priors.drift_sd, priors.beta)
  
  content.init <-"
  
                  for(p in 1:nParticipants) {
                      bound[p] ~ dnorm(bound_mean, pow(bound_sdev, -2))T(0.10,)
                      nondt[p] ~ dnorm(nondt_mean, pow(nondt_sdev, -2))T(0.05,)
                      for(j in 1:2){
                          drift[p,j] ~ dnorm(drift_mean+betaweight*(j-1), pow(drift_sdev, -2))
                      }
                  }"

  content.end <- "
              # Forward equations from EZ Diffusion
              for (k in 1:length(meanRT)) {
                  ey[k]  = exp(-bound[P[k]] * drift[P[k],(X[k])+1])
                  Pc[k]  = 1 / (1 + ey[k])
                  PRT[k] = 2 * pow(drift[P[k],(X[k]+1)], 3) / bound[P[k]] * pow(ey[k] + 1, 2) / (2 * -bound[P[k]] * drift[P[k],(X[k]+1)] * ey[k] - ey[k]*ey[k] + 1)
                  MDT[k] = (bound[P[k]] / (2 * drift[P[k],(X[k]+1)])) * (1 - ey[k]) / (1 + ey[k])
                  MRT[k] = MDT[k] + nondt[P[k]]

                  # Loss functions using MRT, PRT, and Pc
                  correct[k] ~ dbin(Pc[k], nTrialsPerCondition)
                  meanRT[k]  ~ dnorm(MRT[k], PRT[k] * nTrialsPerCondition)
                  varRT[k]   ~ dnorm(1/PRT[k], 0.5*(nTrialsPerCondition-1) * PRT[k] * PRT[k])
              }
      }"
  content <- c(content.init, content.end)
  final_file <- file(modelFile)
  writeLines(c(opening,priorss,content), final_file)
  close(final_file)
}