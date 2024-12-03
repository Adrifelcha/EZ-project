# A function to sample true parameter values for the within-subject hypothesis testing example
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_parameters <- function(priors, nPart, X, Show=TRUE, betaweight=0){
  # Hierarchical parameters for the boundary separation and nondecision time
   bound_mean <- rnorm(1,priors$bound_mean_mean,priors$bound_mean_sdev)
   nondt_mean <- rnorm(1,priors$nondt_mean_mean,priors$nondt_mean_sdev)
   bound_sdev <- runif(1,priors$bound_sdev_lower*2, priors$bound_sdev_upper*0.5)  
   nondt_sdev <- runif(1,priors$nondt_sdev_lower*2, priors$nondt_sdev_upper*0.7)
  #bound_mean <- runif(1,1.0,4.0)
  #nondt_mean <- runif(1,0.2,0.6)
  #bound_sdev <- 0.2
  #nondt_sdev <- 0.05
  # Sample participant-level boundary separations and nondecision times
  bound <- rnorm(nPart,bound_mean, bound_sdev) 
  nondt <- rnorm(nPart,nondt_mean, nondt_sdev) 
  # The within-subjects design is imposed on the drift rate parameter, with a fixed betaweight
  #  drift_mean <- rnorm(1,priors$drift_mean_mean,priors$drift_mean_sdev)
  drift_mean <- runif(1,-3,3)
  drift_sdev <- 0.25
  drift <- rnorm(nPart*2,drift_mean+(betaweight*X), drift_sdev)
  
  parameter_set <- list("bound_mean" = bound_mean, "drift_mean" = drift_mean, "nondt_mean" = nondt_mean, 
                        "bound_sdev" = bound_sdev, "drift_sdev" = drift_sdev, "nondt_sdev" = nondt_sdev,
                        "bound" = bound,   "drift" = drift,   "nondt" = nondt, "betaweight" = betaweight)
  
  if(Show){   show_parameters(parameter_set)    }
  return(parameter_set)
}