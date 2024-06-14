# A function to sample true parameter values from the priors specified
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(settings, modelType, fromPrior=TRUE, Show=TRUE){
  prior <- settings$prior
  if(fromPrior){
        bound_mean <- rnorm(1,prior$bound_mean_mean,prior$bound_mean_sdev)
        drift_mean <- rnorm(1,prior$drift_mean_mean,prior$drift_mean_sdev)
        nondt_mean <- rnorm(1,prior$nondt_mean_mean,prior$nondt_mean_sdev)
        nondt_sdev <- runif(1,prior$nondt_sdev_lower,prior$nondt_sdev_upper)
        bound_sdev <- runif(1,prior$bound_sdev_lower,prior$bound_sdev_upper)  
  }else{
        bound_mean <- runif(1,1,2)
        drift_mean <- runif(1,-3,3)
        nondt_mean <- runif(1,0.15,0.3)
        nondt_sdev <- runif(1,0.015,0.04)
        bound_sdev <- runif(1,0.1,0.3)  
  }
  drift_sdev <- runif(1,prior$drift_sdev_lower,prior$drift_sdev_upper)
  bound <- rnorm(settings$nPart,bound_mean, bound_sdev)
  drift <- rnorm(settings$nPart,drift_mean, drift_sdev)
  nondt <- abs(rnorm(settings$nPart,nondt_mean, nondt_sdev))
  parameter_set <- list("bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
                        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
                        "bound" = bound,   "drift" = drift,   "nondt" = nondt)
  # Check modelType to determine the need for a coefficient
  if(!(modelType=="hierarchical"|is.na(modelType))){   
    # Sample and add coefficient to the parameter_set
    betaweight <- runif(1, prior$betaweight_lower, prior$betaweight_upper)
    parameter_set <- c(parameter_set, list("betaweight" = betaweight))
    # Identify criterion (i.e., parameter of interest)
    if(is.na(settings$criterion)){  settings$criterion <- "drift"  }
    if(settings$criterion=="bound"){  parameter_set$bound <- rnorm(settings$nPart,bound_mean+(betaweight*settings$X), bound_sdev)  }
    if(settings$criterion=="drift"){  parameter_set$drift <- rnorm(settings$nPart,drift_mean+(betaweight*settings$X), drift_sdev)  }
    if(settings$criterion=="nondt"){  parameter_set$nondt <- rnorm(settings$nPart,nondt_mean+(betaweight*settings$X), nondt_sdev)  }
  }
  
  
  if(Show){   show_parameters(parameter_set)    }
  return(parameter_set)
}