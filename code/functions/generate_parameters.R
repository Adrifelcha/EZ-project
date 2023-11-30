###############################################################################
# Functions to load default priors and generate sets of parameter values
###############################################################################

# A function to load the default prior values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
default_priors <- function(Show=TRUE){
  prior <- data.frame("bound_mean_mean" = 1.50,
                      "bound_mean_sdev" = 0.20,
                      "drift_mean_mean" = 0.00,
                      "drift_mean_sdev" = 0.50,
                      "nondt_mean_mean" = 0.30,
                      "nondt_mean_sdev" = 0.06,
                      "bound_sdev_lower" = 0.10,
                      "bound_sdev_upper" = 0.20,
                      "drift_sdev_lower" = 0.20,
                      "drift_sdev_upper" = 0.40,
                      "nondt_sdev_lower" = 0.05,
                      "nondt_sdev_upper" = 0.1)
  if(Show){       show_priors(prior)           }
  return(prior)
}

# A function to sample true parameter values from the priors specified
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(settings, Show=TRUE){
  prior <- settings$prior
  bound_mean <- rnorm(1,prior$bound_mean_mean,prior$bound_mean_sdev)
  if(is.null(prior$drift_intercept_mean)){
    drift_mean <- rnorm(1,prior$drift_mean_mean,prior$drift_mean_sdev)
  }else{
    drift_mean <- 0
  }
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
  if(Show){         show_parameters(parameter_set)              }
  return(parameter_set)
}