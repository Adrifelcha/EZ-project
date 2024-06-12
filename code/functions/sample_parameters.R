# A function to sample true parameter values from the priors specified
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(settings, modelType, Show=TRUE){
  prior <- settings$prior
  bound_mean <- rnorm(1,prior$bound_mean_mean,prior$bound_mean_sdev)
  drift_mean <- rnorm(1,prior$drift_mean_mean,prior$drift_mean_sdev)
  nondt_mean <- rnorm(1,prior$nondt_mean_mean,prior$nondt_mean_sdev)
  bound_sdev <- runif(1,prior$bound_sdev_lower,prior$bound_sdev_upper)  
  drift_sdev <- runif(1,prior$drift_sdev_lower,prior$drift_sdev_upper)
  nondt_sdev <- runif(1,prior$nondt_sdev_lower,prior$nondt_sdev_upper)
  bound <- rnorm(settings$nPart,bound_mean, bound_sdev)
  drift <- rnorm(settings$nPart,drift_mean, drift_sdev)
  nondt <- abs(rnorm(settings$nPart,nondt_mean, nondt_sdev))
  parameter_set <- list("bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
                        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
                        "bound" = bound,   "drift" = drift,   "nondt" = nondt)
  # Check modelType to determine the need for a predictor and coefficient
  if(!(modelType=="hierarchical"|is.na(modelType))){   
           X <- 0:(settings$nPart-1)                      # Default predictor       
           if(modelType=="ttest"){   X <- X %% 2    }     # Dummy predictor
           # Sample and add coefficient to the parameter_set
           betaweight <- runif(1, prior$betaweight_lower, prior$betaweight_upper)
           parameter_set <- c(parameter_set, list("betaweight" = betaweight, "X" = X))
           # Identify criterion (i.e., parameter of interest)
           if(is.na(settings$criterion)){  settings$criterion <- "drift"  }
           crit <- settings$criterion
           # Use coefficient to generate individual true parameters for the criterion
           if(crit=="bound"){  parameter_set$bound <- rnorm(settings$nPart,bound_mean+betaweight*X, bound_sdev)  }
           if(crit=="drift"){  parameter_set$drift <- rnorm(settings$nPart,drift_mean+betaweight*X, drift_sdev)  }
           if(crit=="nondt"){  parameter_set$nondt <- rnorm(settings$nPart,nondt_mean+betaweight*X, nondt_sdev)  }
     }
  if(Show){   show_parameters(parameter_set)    }
  return(parameter_set)
}