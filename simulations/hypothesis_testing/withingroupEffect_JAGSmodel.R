#####################################################################
# Functions to automatically write desired JAGS models
#####################################################################

# A function to write the JAGS model using the priors values
writefixEffJAGSmodel <- function(priors, beta.effect, modelFile){
  opening <- "model{"
  priors.bound_m  <- paste("          bound_mean ~ dnorm(", priors$bound_mean_mean,",pow(",priors$bound_mean_sdev,",-2))T(0.10,3.00)", sep="")
  priors.nondt_m  <- paste("          nondt_mean ~ dnorm(", priors$nondt_mean_mean,",pow(",priors$nondt_mean_sdev,",-2))T(0.05,)", sep="")
  priors.drift_m  <- paste("          drift_mean ~ dnorm(", priors$drift_mean_mean,",pow(",priors$drift_mean_sdev,",-2))T(-3.00,3.00)", sep="")
  priors.bound_sd <- paste("          bound_sdev ~ dunif(", priors$bound_sdev_lower,",",priors$bound_sdev_upper,")", sep="")
  priors.nondt_sd <- paste("          nondt_sdev ~ dunif(", priors$nondt_sdev_lower,",",priors$nondt_sdev_upper,")", sep="")
  priors.drift_sd <- paste("          drift_sdev ~ dunif(", priors$drift_sdev_lower,",",priors$drift_sdev_upper,")", sep="")
  priors.beta     <- paste("          betaweight ~ dunif(", priors$betaweight_lower,",",priors$betaweight_upper,")", sep="")
  priors.drift_cond <-"
                        # Sampling model
                        for (k in 1:2){
                            drift_cond[k] ~ dnorm(drift_mean + betaweight*X[k], pow(0.25, -2))T(-3.00,3.00)
                        }"    
  
  priorss <- c(priors.bound_m, priors.nondt_m, priors.drift_m, priors.bound_sd, priors.nondt_sd, priors.drift_sd, priors.beta, priors.drift_cond)
  content.init <-"
                  # Sampling model
                  for (p in 1:nParticipants){
                      bound[p] ~ dnorm(bound_mean, pow(bound_sdev, -2))T(0.10,3.00)
                      nondt[p] ~ dnorm(nondt_mean, pow(nondt_sdev, -2))T(0.05,)    
  
                      for(k in 1:2){
                      drift[p, k] ~ dnorm(drift_mean + betaweight*X[p], pow(drift_sdev, -2))T(-3.00,3.00)
                      }
                      "

  content.end <- "
                  # Forward equations from EZ Diffusion
                  ey[p]  = exp(-bound[p] * drift[p])
                  Pc[p]  = 1 / (1 + ey[p])
                  PRT[p] = 2 * pow(drift[p], 3) / bound[p] * pow(ey[p] + 1, 2) / (2 * -bound[p] * drift[p] * ey[p] - ey[p]*ey[p] + 1)
                  MDT[p] = (bound[p] / (2 * drift[p])) * (1 - ey[p]) / (1 + ey[p])
                  MRT[p] = MDT[p] + nondt[p]

                  # Loss functions using MRT, PRT, and Pc
                  correct[p] ~ dbin(Pc[p], nTrialsPerPerson)
                  meanRT[p]  ~ dnorm(MRT[p], PRT[p] * nTrialsPerPerson)
                  varRT[p]   ~ dnorm(1/PRT[p], 0.5*(nTrialsPerPerson-1) * PRT[p] * PRT[p])
              }
      }"
  content <- c(content.init, content.end)
  final_file <- file(modelFile)
  writeLines(c(opening,priorss,content), final_file)
  close(final_file)
}