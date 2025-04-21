# A function to sample true parameter values from arbitrary uniform distributions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(priors, nPart, modelType, X, criterion=NA, fromPrior=TRUE, Show=TRUE, fixedBeta=NA){
  # Hierarchical mean values are sampled from a uniform defined because...
  bound_mean <- runif(1,0.5,4)        # ...95% Prior in {0.29, 4.2}. These values have been fixed arbitrarily
  drift_mean <- runif(1,-5.5,5.5)     # ...95% Prior in {-5.8,5.8}.  These values have been fixed arbitrarily
  nondt_mean <- runif(1,0.15,0.5)    # ...95% Prior in {0.06, 1.03}. These are arbitrary values
  # Standard deviations are proportional
  bound_sdev <- bound_mean/5
  drift_sdev <- abs(drift_mean/5)
  nondt_sdev <- nondt_mean/5
  bound <- rnorm(nPart,bound_mean, bound_sdev) # Extra-precaution / shouldn't be needed often
  drift <- rnorm(nPart,drift_mean, drift_sdev)
  nondt <- rnorm(nPart,nondt_mean, nondt_sdev) # Precaution
  parameter_set <- list("bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
                        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
                        "bound" = bound,   "drift" = drift,   "nondt" = nondt)
  # Check modelType to determine the need for a coefficient
  if(!(modelType=="hierarchical"|is.na(modelType))){  
            # Sample and add coefficient to the parameter_set
            betaweight <- runif(1,-1,1)
            # Identify criterion (i.e., parameter of interest)
            if(is.na(criterion)){  criterion <- "drift"  }
            if(criterion=="bound"){  parameter_set$bound <- rnorm(nPart,bound_mean+(betaweight*X), bound_sdev)  }
            if(criterion=="drift"){  parameter_set$drift <- rnorm(nPart,drift_mean+(betaweight*X), drift_sdev)  }
            if(criterion=="nondt"){  betaweight <- abs(betaweight)
                                     parameter_set$nondt <- rnorm(nPart,nondt_mean+(betaweight*X), nondt_sdev)  }
    parameter_set <- c(parameter_set, list("betaweight" = betaweight))
  }
  if(!is.na(fixedBeta)){}
  if(Show){   show_parameters(parameter_set)    }
  return(parameter_set)
}