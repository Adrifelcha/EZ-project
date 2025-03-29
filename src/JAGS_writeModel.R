#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function generates a JAGS model file for the implementation of the 
# EZ Bayesian Hierarchical Drift Diffusion Model
# Inputs:
# - priors: Data frame containing prior distribution specifications
# - modelType: Type of model ("hierarchical", "metaregression", "ttest")
# - criterion: Parameter affected by the predictor ("drift", "bound", "nondt")
# - modelFile: File path where the JAGS model should be saved
# - custom_truncation_list: List specifying truncation values for any distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
JAGS_writeModel <- function(priors, modelType, criterion, modelFile=NA, custom_truncation_list = NULL){

  # If no custom truncation list is provided, use these default values
  if(is.null(custom_truncation_list)){
        custom_truncation_list <- list(
                "bound_mean" = c(0.1, 5.0),
                "nondt_mean" = c(0.05, ""),
                "drift_mean" = c(-3, 3),
                "bound_sdev" = c(0.01, ""),
                "nondt_sdev" = c(0.01, ""),
                "drift_sdev" = c(0.01, ""),
                "drift" = c(-3, 3),
                "bound" = c(0.1, 5.0),
                "nondt" = c(0.05, ""),
                "betaweight" = c(-3, 3)
        )
  }
  # Rename truncation list to simplify code
  t <- custom_truncation_list

  # If no model file is provided, use a generic model file name
  if(is.na(modelFile)){
    modelFile <- here("output", "BUGS-models", "JAGS_model.txt")
  }

  # Start the model definition
  opening <- "model{"

  # Define prior distributions for hierarchical means
  # Each parameter has a normal prior with specified mean and precision (1/variance)
  # T() indicates truncation to keep parameters in reasonable ranges
  priors.bound_m  <- paste("          bound_mean ~ dnorm(", priors$bound_mean_mean,",pow(",priors$bound_mean_sdev,",-2))T(", t$bound_mean[1],",", t$bound_mean[2], ")", sep="")
  priors.nondt_m  <- paste("          nondt_mean ~ dnorm(", priors$nondt_mean_mean,",pow(",priors$nondt_mean_sdev,",-2))T(", t$nondt_mean[1],",", t$nondt_mean[2], ")", sep="")
  priors.drift_m  <- paste("          drift_mean ~ dnorm(", priors$drift_mean_mean,",pow(",priors$drift_mean_sdev,",-2))T(", t$drift_mean[1],",", t$drift_mean[2], ")", sep="")
  
  # Define prior distributions for hierarchical standard deviations
  # Check if we're using uniform or inverse gamma priors
  if(all(c("bound_sdev_lower", "bound_sdev_upper") %in% names(priors))){
    # Uniform priors
    priors.bound_sd <- paste("          bound_sdev ~ dunif(", priors$bound_sdev_lower,",",priors$bound_sdev_upper,")", sep="")
    priors.nondt_sd <- paste("          nondt_sdev ~ dunif(", priors$nondt_sdev_lower,",",priors$nondt_sdev_upper,")", sep="")
    priors.drift_sd <- paste("          drift_sdev ~ dunif(", priors$drift_sdev_lower,",",priors$drift_sdev_upper,")", sep="")
  } else if(all(c("bound_sdev_shape", "bound_sdev_scale") %in% names(priors))){
    # Inverse gamma priors
    priors.bound_sd <- paste("          bound_sdev ~ dgamma(", priors$bound_sdev_shape,",",priors$bound_sdev_scale,")T(", t$bound_sdev[1],",", t$bound_sdev[2], ")", sep="")
    priors.nondt_sd <- paste("          nondt_sdev ~ dgamma(", priors$nondt_sdev_shape,",",priors$nondt_sdev_scale,")T(", t$nondt_sdev[1],",", t$nondt_sdev[2], ")", sep="")
    priors.drift_sd <- paste("          drift_sdev ~ dgamma(", priors$drift_sdev_shape,",",priors$drift_sdev_scale,")T(", t$drift_sdev[1],",", t$drift_sdev[2], ")", sep="")
  } else {
    cat("Unknown prior distribution for the hierarchical standard deviations")
  }
  
  # Combine all prior specifications
  priorss <- c(priors.bound_m, priors.nondt_m, priors.drift_m, priors.bound_sd, priors.nondt_sd, priors.drift_sd)
  
  # For models with a regression component, add betaweight parameter
  if(modelType != "hierarchical"){
      # Add prior for the regression coefficient
      priors.beta <- paste("          betaweight ~ dnorm(", priors$betaweight_mean,",pow(",priors$betaweight_sdev,",-2))T(", t$betaweight[1],",", t$betaweight[2], ")", sep="")
      priorss <- c(priorss, priors.beta)
      
      # Define model structure based on which parameter is affected by the predictor
      if(criterion=="drift"){
          # Effect on drift rate
          content.init <- paste0(
                  "\n                  # Sampling model",
                  "\n                  for (p in 1:nParticipants){",
                  "\n                      drift[p] ~ dnorm(drift_mean + betaweight*X[p], pow(drift_sdev, -2))T(", t$drift[1],",", t$drift[2], ")",
                  "\n                      bound[p] ~ dnorm(bound_mean, pow(bound_sdev, -2))T(", t$bound[1],",", t$bound[2], ")",
                  "\n                      nondt[p] ~ dnorm(nondt_mean, pow(nondt_sdev, -2))T(", t$nondt[1],",", t$nondt[2], ")"
          )
      } else if(criterion=="bound"){
          # Effect on boundary separation
          content.init <- paste0(
                  "\n                  # Sampling model",
                  "\n                  for (p in 1:nParticipants){",
                  "\n                      bound[p] ~ dnorm(bound_mean + betaweight*X[p], pow(bound_sdev, -2))T(", t$bound[1],",", t$bound[2], ")",
                  "\n                      nondt[p] ~ dnorm(nondt_mean, pow(nondt_sdev, -2))T(", t$nondt[1],",", t$nondt[2], ")",
                  "\n                      drift[p] ~ dnorm(drift_mean, pow(drift_sdev, -2))T(", t$drift[1],",", t$drift[2], ")"
          )
      } else {
          # Effect on non-decision time
          content.init <- paste0(
                  "\n                  # Sampling model",
                  "\n                  for (p in 1:nParticipants){",
                  "\n                      nondt[p] ~ dnorm(nondt_mean + betaweight*X[p], pow(nondt_sdev, -2))T(", t$nondt[1],",", t$nondt[2], ")",
                  "\n                      bound[p] ~ dnorm(bound_mean, pow(bound_sdev, -2))T(", t$bound[1],",", t$bound[2], ")",
                  "\n                      drift[p] ~ dnorm(drift_mean, pow(drift_sdev, -2))T(", t$drift[1],",", t$drift[2], ")"
          )
      }
   } else {
      # Basic hierarchical model without regression effects
      content.init <- paste0(
                "\n                # Sampling model",
                "\n                for (p in 1:nParticipants){",
                "\n                    bound[p] ~ dnorm(bound_mean, pow(bound_sdev, -2))T(", t$bound[1],",", t$bound[2], ")",
                "\n                    nondt[p] ~ dnorm(nondt_mean, pow(nondt_sdev, -2))T(", t$nondt[1],",", t$nondt[2], ")",
                "\n                    drift[p] ~ dnorm(drift_mean, pow(drift_sdev, -2))T(", t$drift[1],",", t$drift[2], ")"
      )
  }
  
  # Common model components for all model types
  # These implement the EZ-DDM equations and likelihood functions
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
  
  # Combine all model components
  content <- c(content.init, content.end)
  
  # Write the complete model to file
  final_file <- file(modelFile)
  writeLines(c(opening, priorss, content), final_file)
  close(final_file)
}


#JAGS_writeModel(priors, modelType = "hierarchical", criterion = "drift", modelFile=NA, custom_truncation_list = NULL)