# A function to sample true parameter values from the priors specified
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(priors, nPart, modelType, X, criterion, fromPrior=TRUE, Show=TRUE){
  if(fromPrior){
        bound_mean <- rnorm(1,priors$bound_mean_mean,priors$bound_mean_sdev)
        drift_mean <- rnorm(1,priors$drift_mean_mean,priors$drift_mean_sdev)
        nondt_mean <- rnorm(1,priors$nondt_mean_mean,priors$nondt_mean_sdev)
        nondt_sdev <- runif(1,priors$nondt_sdev_lower,priors$nondt_sdev_upper)
        bound_sdev <- runif(1,priors$bound_sdev_lower,priors$bound_sdev_upper)  
  }else{
        bound_mean <- runif(1,1,2)
        drift_mean <- runif(1,-1.5,1.5)
        nondt_mean <- runif(1,0.1,0.5)
        nondt_sdev <- runif(1,0.05,0.25)
        bound_sdev <- runif(1,0.1,0.35)  
  }
  drift_sdev <- runif(1,priors$drift_sdev_lower,priors$drift_sdev_upper)
  bound <- rnorm(nPart,bound_mean, bound_sdev)
  drift <- rnorm(nPart,drift_mean, drift_sdev)
  nondt <- abs(rnorm(nPart,nondt_mean, nondt_sdev))
  parameter_set <- list("bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
                        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
                        "bound" = bound,   "drift" = drift,   "nondt" = nondt)
  # Check modelType to determine the need for a coefficient
  if(!(modelType=="hierarchical"|is.na(modelType))){   
    # Sample and add coefficient to the parameter_set
    betaweight <- runif(1, priors$betaweight_lower, priors$betaweight_upper)
    parameter_set <- c(parameter_set, list("betaweight" = betaweight))
    # Identify criterion (i.e., parameter of interest)
    if(is.na(criterion)){  criterion <- "drift"  }
    if(criterion=="bound"){  parameter_set$bound <- rnorm(nPart,bound_mean+(betaweight*X), bound_sdev)  }
    if(criterion=="drift"){  parameter_set$drift <- rnorm(nPart,drift_mean+(betaweight*X), drift_sdev)  }
    if(criterion=="nondt"){  parameter_set$nondt <- rnorm(nPart,nondt_mean+(betaweight*X), nondt_sdev)  }
  }
  
  
  if(Show){   show_parameters(parameter_set)    }
  return(parameter_set)
}