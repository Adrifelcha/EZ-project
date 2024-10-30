# A function to sample true parameter values from the priors specified
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(priors, nPart, modelType, X, criterion=NA, fromPrior=TRUE, Show=TRUE, fixedBeta = NA){
  bound_mean <- rnorm(1,priors$bound_mean_mean,priors$bound_mean_sdev)
  drift_mean <- rnorm(1,priors$drift_mean_mean,priors$drift_mean_sdev)
  nondt_mean <- rnorm(1,priors$nondt_mean_mean,priors$nondt_mean_sdev)
  bound_sdev <- runif(1,priors$bound_sdev_lower,priors$bound_sdev_upper)  
  nondt_sdev <- runif(1,priors$nondt_sdev_lower,priors$nondt_sdev_upper)
  drift_sdev <- runif(1,priors$drift_sdev_lower,priors$drift_sdev_upper)
  bound <- rnorm(nPart,bound_mean, bound_sdev) # Extra-precaution / shouldn't be needed often
  drift <- rnorm(nPart,drift_mean, drift_sdev)
  nondt <- rnorm(nPart,nondt_mean, nondt_sdev) # Precaution
  parameter_set <- list("bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
                        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
                        "bound" = bound,   "drift" = drift,   "nondt" = nondt)
  # Check modelType to determine the need for a coefficient
  if(!(modelType=="hierarchical"|is.na(modelType))){  
        if(!is.na(fixedBeta)){
        }else{
            # Sample and add coefficient to the parameter_set
            betaweight <- runif(1,-1,1)
            # Identify criterion (i.e., parameter of interest)
            if(is.na(criterion)){  criterion <- "drift"  }
            if(criterion=="bound"){  parameter_set$bound <- rnorm(nPart,bound_mean+(betaweight*X), bound_sdev)  }
            if(criterion=="drift"){  parameter_set$drift <- rnorm(nPart,drift_mean+(betaweight*X), drift_sdev)  }
            if(criterion=="nondt"){  betaweight <- abs(betaweight)
                                     parameter_set$nondt <- rnorm(nPart,nondt_mean+(betaweight*X), nondt_sdev)  }
        }
    parameter_set <- c(parameter_set, list("betaweight" = betaweight))
  }
  
  if(Show){   show_parameters(parameter_set)    }
  return(parameter_set)
}