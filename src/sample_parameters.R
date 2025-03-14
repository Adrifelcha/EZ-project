#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function samples random true parameter values used in simulation studies
#
# The function generates hierarchical parameter values from either: 
# a) the specified prior distributions
# b) suited uniform distributions
# Then, these values are used to generate individual-level parameters for data simulation.
#
# Inputs:
# - priors: Data frame containing prior distribution parameters
# - nPart: Number of participants to generate parameters for
# - modelType: Type of model ("hierarchical", "metaregression" or "ttest")
# - X: Vector of predictors for regression models
# - criterion: Parameter affected by the predictor (default: "drift")
# - fromPrior: Whether to sample parameter values from priors or uniform distributions
# - Show: Whether to display the sampled parameters in the console
# - fixedBeta: Optional fixed value for the regression coefficient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(priors, nPart, modelType, X, criterion=NA, fromPrior=TRUE, Show=TRUE, fixedBeta = NA){
  
  # Sample hierarchical parameters (means and standard deviations)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  if(fromPrior){
        # Sample hierarchical means from normal distributions
        bound_mean <- rnorm(1, priors$bound_mean_mean, priors$bound_mean_sdev)
        drift_mean <- rnorm(1, priors$drift_mean_mean, priors$drift_mean_sdev)
        nondt_mean <- rnorm(1, priors$nondt_mean_mean, priors$nondt_mean_sdev)        
        # Sample hierarchical standard deviations based on prior parameters specified
        if(all(c("bound_sdev_lower", "bound_sdev_upper") %in% names(priors))){  
            # Uniform prior
            bound_sdev <- runif(1, priors$bound_sdev_lower, priors$bound_sdev_upper)
            drift_sdev <- runif(1, priors$drift_sdev_lower, priors$drift_sdev_upper)
            nondt_sdev <- runif(1, priors$nondt_sdev_lower, priors$nondt_sdev_upper)
        } else if(all(c("bound_sdev_shape", "bound_sdev_scale") %in% names(priors))){
            # Inverse gamma prior
            bound_sdev <- 1/rgamma(1, shape=priors$bound_sdev_shape, rate=priors$bound_sdev_scale)
            drift_sdev <- 1/rgamma(1, shape=priors$drift_sdev_shape, rate=priors$drift_sdev_scale)
            nondt_sdev <- 1/rgamma(1, shape=priors$nondt_sdev_shape, rate=priors$nondt_sdev_scale)
        } else {
            stop("Unkown prior distribution for the hierarchical standard deviations")
        }        
  } else {
        # Sample from uniform distributions when not using priors
        # These ranges approximate the 95% density of the default priors
        bound_mean <- runif(1, 1.5, 3)
        drift_mean <- runif(1, -3, 3)
        nondt_mean <- runif(1, 0.2, 0.6)

        # Fixed standard deviations when not sampling from priors
        bound_sdev <- 0.3
        drift_sdev <- 0.5
        nondt_sdev <- 0.06
  }
  

  # Sample individual-level parameters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  bound <- rnorm(nPart, bound_mean, bound_sdev) 
  drift <- rnorm(nPart, drift_mean, drift_sdev)
  nondt <- rnorm(nPart, nondt_mean, nondt_sdev) 
  
  # Create parameter set list with all generated values
  parameter_set <- list("bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
                        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
                        "bound" = bound, "drift" = drift, "nondt" = nondt)
  
  # BETAWEIGHT: For models with a regression component, the betaweight is sampled
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  if(!(modelType=="hierarchical"|is.na(modelType))){  
        if(!is.na(fixedBeta)){
            # Use fixed beta value if provided
            betaweight <- fixedBeta
            parameter_set$drift_sdev <- 0.25
            parameter_set$drift <- rnorm(n = nPart*2, 
                                         mean = drift_mean+betaweight*X,
                                         sd = parameter_set$drift_sdev)
        } else {
            # Sample regression coefficient based on criterion
            if(criterion=="nondt"){   
                betaweight <- runif(1, 0, 1)  # Positive effect for non-decision time
            } else {   
                betaweight <- runif(1, -1, 1)  # Bidirectional effect for other parameters
            }

            # Apply regression effect to the appropriate parameter
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
            # Default criterion is drift
            if(is.na(criterion)||criterion=="drift"){  
                criterion <- "drift"  
                parameter_set$drift <- rnorm(nPart, drift_mean+(betaweight*X), drift_sdev)  
            } else if(criterion=="bound"){
                parameter_set$bound <- rnorm(nPart, bound_mean+(betaweight*X), bound_sdev)  
            } else if(criterion=="nondt"){
                parameter_set$nondt <- rnorm(nPart, nondt_mean+(betaweight*X), nondt_sdev)  
            }
        }
        # Add betaweight to parameter set
        parameter_set <- c(parameter_set, list("betaweight" = betaweight))
  }
  
  # Display parameters if requested
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  if(Show){   
        show_parameters(parameter_set)    
  }
  
  return(parameter_set)
}