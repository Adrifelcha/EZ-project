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
# - withinSubject: Whether to use a within-subject design (default: FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

########################################################################################
#   Main function
########################################################################################
########################################################################################
sample_parameters <- function(priors, nPart, modelType = "hierarchical", X = NULL, 
                             criterion = NA, fromPrior = TRUE, Show = TRUE, 
                             fixedBeta = NA, withinSubject = FALSE, generative_uniforms = NULL) {
      
      # Defensive programming: No criterion is needed for hierarchical models
      if(modelType == "hierarchical"){  criterion <- NA     }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      # Sample hierarchical parameters (means and standard deviations)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      if(fromPrior) {
          # Sample hierarchical parameters from priors
          hierarchical_parameters <- sample_from_priors(priors = priors, 
                                                        withinSubject = withinSubject)
      } else {
          # Sample hierarchical parameters from uniform distributions
          hierarchical_parameters <- sample_from_uniforms(generative_uniforms = generative_uniforms)    
      }

      # Extract hierarchical parameters
      drift_mean <- hierarchical_parameters$drift_mean
      nondt_mean <- hierarchical_parameters$nondt_mean
      bound_mean <- hierarchical_parameters$bound_mean
      bound_sdev <- hierarchical_parameters$bound_sdev
      nondt_sdev <- hierarchical_parameters$nondt_sdev
      drift_sdev <- hierarchical_parameters$drift_sdev
      # Within-subject design uses a fixed standard deviation for the population drift rate
      if(withinSubject) {   drift_sdev <- 0.25     }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      # Sample non-hierarchical parameters
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      # Initialize parameters
      bound <- sample_bound(nPart, bound_mean, bound_sdev, X, criterion)      
      nondt <- sample_nondt(nPart, nondt_mean, nondt_sdev, X, criterion)
      betaweight <- sample_betaweight(fixedBeta = fixedBeta, criterion = criterion)
      drift <- sample_drift(nPart, drift_mean, drift_sdev, betaweight, X, withinSubject, criterion)  

      # Create parameter set list with all generated values
      parameter_set <- list(
        "bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
        "bound" = bound, "drift" = drift, "nondt" = nondt, "betaweight" = betaweight
      )
      
      # Display parameters if requested
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      if(Show) {      show_parameters(parameter_set)          }

      # Let user know that criterion wasn't specified and no effect was sampled
      if(modelType != "hierarchical" & is.na(criterion)){
         cat("modelType suggests a regression model, but no criterion was specified. No effect was sampled.")
      }

return(parameter_set)
}
########################################################################################
########################################################################################

#+
#+
#+
#+
#+
#+
#+
#+
#+
#+
#+

########################################################################################
#   Auxiliary functions
########################################################################################

##############################################
# Sample hierarchical parameters
##############################################
# Sample hierarchical parameters from priors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_from_priors <- function(priors, withinSubject = FALSE) {
        # Sample hierarchical means from normal distributions
        hierarchical_parameters <- list(
                    "bound_mean" = rnorm(1, priors$bound_mean_mean, priors$bound_mean_sdev),
                    "drift_mean" = rnorm(1, priors$drift_mean_mean, priors$drift_mean_sdev),
                    "nondt_mean" = rnorm(1, priors$nondt_mean_mean, priors$nondt_mean_sdev))

        # Sample hierarchical standard deviations based on prior parameters specified
        # Use a uniform distribution...
        if(all(c("bound_sdev_lower", "bound_sdev_upper") %in% names(priors))) {
              bound_sdev <- runif(1, priors$bound_sdev_lower*2, priors$bound_sdev_upper*0.9)
              nondt_sdev <- runif(1, priors$nondt_sdev_lower*2, priors$nondt_sdev_upper*0.9)
              if(!is.null(priors$drift_sdev_lower)) {
                  drift_sdev <- runif(1, priors$drift_sdev_lower, priors$drift_sdev_upper)
              }
        # ...or an inverse gamma distribution
        } else if(all(c("bound_sdev_shape", "bound_sdev_scale") %in% names(priors))) {
            # Inverse gamma prior
            bound_sdev <- 1/rgamma(1, shape=priors$bound_sdev_shape, rate=priors$bound_sdev_scale)      
            nondt_sdev <- 1/rgamma(1, shape=priors$nondt_sdev_shape, rate=priors$nondt_sdev_scale)
            if(!is.null(priors$drift_sdev_shape)) {
                drift_sdev <- 1/rgamma(1, shape=priors$drift_sdev_shape, rate=priors$drift_sdev_scale)
            }
        } else {
          stop("Unknown prior distribution for the hierarchical standard deviations")
        }
        # Add standard deviations to the parameter list
        hierarchical_parameters <- c(hierarchical_parameters, list(
                                                                    "bound_sdev" = bound_sdev,
                                                                    "nondt_sdev" = nondt_sdev,
                                                                    "drift_sdev" = drift_sdev))
  return(hierarchical_parameters)
}

# Sample hierarchical parameters from uniform distributions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_from_uniforms <- function(generative_uniforms) {
      # Function requires a list specifying the uniform distributions for each parameter
      if(is.null(generative_uniforms)) {
            cat("No generative uniform distributions specified. Fill in the generative_uniforms list.")
            return(list(
                  "bound_mean" = c(NA, NA), "nondt_mean" = c(NA, NA), "drift_mean" = c(NA, NA),
                  "bound_sdev" = c(NA, NA), "nondt_sdev" = c(NA, NA), "drift_sdev" = c(NA, NA)))
      } else {
        # Sample hierarchical means from uniform distributions
        hierarchical_parameters <- list(
          "bound_mean" = runif(1, generative_uniforms$bound_mean[1], generative_uniforms$bound_mean[2]),
          "drift_mean" = runif(1, generative_uniforms$drift_mean[1], generative_uniforms$drift_mean[2]),
          "nondt_mean" = runif(1, generative_uniforms$nondt_mean[1], generative_uniforms$nondt_mean[2])
        )
        
        # Sample hierarchical standard deviations from uniform distributions or use fixed values
        if(length(generative_uniforms$bound_sdev) == 2) {
          bound_sdev <- runif(1, generative_uniforms$bound_sdev[1], generative_uniforms$bound_sdev[2])
        } else {    bound_sdev <- generative_uniforms$bound_sdev      }
        if(length(generative_uniforms$nondt_sdev) == 2) {
          nondt_sdev <- runif(1, generative_uniforms$nondt_sdev[1], generative_uniforms$nondt_sdev[2])
        } else {    nondt_sdev <- generative_uniforms$nondt_sdev      }
        if(length(generative_uniforms$drift_sdev) == 2) {
          drift_sdev <- runif(1, generative_uniforms$drift_sdev[1], generative_uniforms$drift_sdev[2])
        } else {    drift_sdev <- generative_uniforms$drift_sdev      }

        # Add standard deviations to the parameter list
        hierarchical_parameters <- c(hierarchical_parameters, list("bound_sdev" = bound_sdev,
                                                                   "nondt_sdev" = nondt_sdev,
                                                                   "drift_sdev" = drift_sdev))
        return(hierarchical_parameters)
      }
}

##############################################
# Sample individual parameters and beta-weight
##############################################
# Beta-weight: Effect size for the regression model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_betaweight <- function(fixedBeta, criterion) {
    betaweight <- NA
    if(is.na(fixedBeta)) {
          betaweight <- ifelse(criterion == "nondt", 
                               runif(1, 0, 1),    # Positive effect for non-decision time
                               runif(1, -1, 1))   # Bidirectional effect for other parameters
    } else {            betaweight <- fixedBeta                                                 }
return(betaweight)
}

# Individual-level drift rates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_drift <- function(nPart, drift_mean, drift_sdev, betaweight, X, 
                         withinSubject = FALSE, criterion = NA) {
      if(withinSubject) {   drift <- rnorm(nPart*2, drift_mean + (betaweight*X), drift_sdev)
      }else{  
              if(criterion == "drift") {
                drift <- rnorm(nPart, drift_mean + (betaweight*X), drift_sdev)
              } else {
                drift <- rnorm(nPart, drift_mean, drift_sdev)
              }
      }
return(drift)
}

# Individual-level non-decision times
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_nondt <- function(nPart, nondt_mean, nondt_sdev, X, criterion = NA) {
    if(criterion == "nondt") {
                nondt <- rnorm(nPart, nondt_mean + (betaweight*X), nondt_sdev)
    } else {
                nondt <- rnorm(nPart, nondt_mean, nondt_sdev)
    }
return(nondt)
}

# Individual-level boundary separation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_bound <- function(nPart, bound_mean, bound_sdev, X, criterion = NA) {
    if(criterion == "bound") {
                bound <- rnorm(nPart, bound_mean + (betaweight*X), bound_sdev)
    } else {
                bound <- rnorm(nPart, bound_mean, bound_sdev)
    }
return(bound)
}


##############################################
# Print parameter values to console
##############################################
show_parameters <- function(parameter_set){  
      cat("===== EZBHDDM True Parameters: ==============\n")
      
      # Print drift rate hierarchical parameters
      cat("Drift Mean:   ", parameter_set$drift_mean,"\n")
      cat("Drift SD:     ", parameter_set$drift_sdev,"\n")
      # Print decision boundary hierarchical parameters
      cat("Bound Mean:   ", parameter_set$bound_mean,"\n")
      cat("Bound SD:     ", parameter_set$bound_sdev,"\n")
      # Print non-decision time hierarchical parameters
      cat("Non-decision Time Mean:", parameter_set$nondt_mean,"\n")
      cat("Non-decision Time SD:  ", parameter_set$nondt_sdev,"\n")
      # Print betaweight parameter if it exists
      if(!is.null(parameter_set$betaweight)){
        cat("Betaweight:   ", parameter_set$betaweight,"\n")
      }
        
      cat("=============================================\n")
}