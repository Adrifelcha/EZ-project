#####################################################################
# Functions to automatically write desired JAGS models
#####################################################################

# A function to write the JAGS model using the prior values
write_JAGSmodel <- function(settings, modelFile){
  prior  <- settings$prior
  crit   <- settings$criterion
  opening <- "model{"
  prior.bound_m  <- paste("          bound_mean ~ dnorm(", prior$bound_mean_mean,",pow(",prior$bound_mean_sdev,",-2))T(0.10,3.00)", sep="")
  prior.nondt_m  <- paste("          nondt_mean ~ dnorm(", prior$nondt_mean_mean,",pow(",prior$nondt_mean_sdev,",-2))T(0.05,)", sep="")
  prior.drift_m  <- paste("          drift_mean ~ dnorm(", prior$drift_mean_mean,",pow(",prior$drift_mean_sdev,",-2))T(-3.00,3.00)", sep="")
  prior.bound_sd <- paste("          bound_sdev ~ dunif(", prior$bound_sdev_lower,",",prior$bound_sdev_upper,")", sep="")
  prior.nondt_sd <- paste("          nondt_sdev ~ dunif(", prior$nondt_sdev_lower,",",prior$nondt_sdev_upper,")", sep="")
  prior.drift_sd <- paste("          drift_sdev ~ dunif(", prior$drift_sdev_lower,",",prior$drift_sdev_upper,")", sep="")
  priors <- c(prior.bound_m, prior.nondt_m, prior.drift_m, prior.bound_sd, prior.nondt_sd, prior.drift_sd)
  if(settings$modelType != "hierarchical"){
      prior.beta <- paste("          betaweight ~ dunif(", prior$betaweight_lower,",",prior$betaweight_upper,")", sep="")
      priors <- c(priors, prior.beta)
  }
  content.init <-"
              # Sampling model
              for (p in 1:nParticipants){
                  bound[p] ~ dnorm(bound_mean, pow(bound_sdev, -2))T(0.10,3.00)
                  nondt[p] ~ dnorm(nondt_mean, pow(nondt_sdev, -2))T(0.05,1.00)"
  if(!is.null(prior$drift_intercept_mean)){
    content.mid <- "                  drift_mean = drift_intrcpt"
    if(!is.null(prior$drift_coefficient_mean)){
      content.mid <- paste(content.mid," + (drift_coeff * x[p])", sep="") 
    }
    content.init <- c(content.init,content.mid)
  }
  content.end <- "                  drift[p] ~ dnorm(drift_mean, pow(drift_sdev, -2))T(-3.00,3.00)
      
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
  writeLines(c(opening,priors,content), final_file)
  close(final_file)
}