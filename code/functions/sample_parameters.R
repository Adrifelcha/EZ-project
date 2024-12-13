# A function to sample true parameter values from the priors specified
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(priors, nPart, modelType, X, criterion=NA, fromPrior=TRUE, Show=TRUE, fixedBeta = NA){
  if(fromPrior){
        bound_mean <- rnorm(1,priors$bound_mean_mean,priors$bound_mean_sdev)
        drift_mean <- rnorm(1,priors$drift_mean_mean,priors$drift_mean_sdev)
        nondt_mean <- rnorm(1,priors$nondt_mean_mean,priors$nondt_mean_sdev)
        bound_sdev <- runif(1,priors$bound_sdev_lower, priors$bound_sdev_upper)  
        nondt_sdev <- runif(1,priors$nondt_sdev_lower, priors$nondt_sdev_upper)
        drift_sdev <- runif(1,priors$drift_sdev_lower, priors$drift_sdev_upper)
  }else{
        # Hierarchical mean values are sampled from a uniform defined because...
        bound_mean <- runif(1,1.5,3)     # ...95% density of the default prior falls here
        drift_mean <- runif(1,-3,3)      # ...95% density of the default prior falls here
        nondt_mean <- runif(1,0.2,0.6)  # ...95% density of the default prior falls here
        # Hierarchical standard deviations are sampled from arbitrary uniforms
        bound_sdev <- 0.3
        drift_sdev <- 0.5
        nondt_sdev <- 0.06
  }
  bound <- rnorm(nPart,bound_mean, bound_sdev) # Extra-precaution / shouldn't be needed often
  drift <- rnorm(nPart,drift_mean, drift_sdev)
  nondt <- rnorm(nPart,nondt_mean, nondt_sdev) # Precaution
  parameter_set <- list("bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
                        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
                        "bound" = bound,   "drift" = drift,   "nondt" = nondt)
  # Check modelType to determine the need for a coefficient
  if(!(modelType=="hierarchical"|is.na(modelType))){  
        if(!is.na(fixedBeta)){
          betaweight <- fixedBeta
          parameter_set$drift_sdev <- 0.25
          parameter_set$drift <- rnorm(nPart*2,drift_mean+betaweight*X, 0.25)
        }else{
            # Sample and add coefficient to the parameter_set
            if(criterion=="nondt"){   betaweight <- runif(1, 0, 1)
                             }else{   betaweight <- runif(1, -1, 1)       }
            # Identify criterion (i.e., parameter of interest)
            if(is.na(criterion)){  criterion <- "drift"  }
            if(criterion=="bound"){  parameter_set$bound <- rnorm(nPart,bound_mean+(betaweight*X), bound_sdev)  }
            if(criterion=="drift"){  parameter_set$drift <- rnorm(nPart,drift_mean+(betaweight*X), drift_sdev)  }
            if(criterion=="nondt"){  parameter_set$nondt <- rnorm(nPart,nondt_mean+(betaweight*X), nondt_sdev)  }
        }
    parameter_set <- c(parameter_set, list("betaweight" = betaweight))
  }
  
  if(Show){   show_parameters(parameter_set)    }
  return(parameter_set)
}