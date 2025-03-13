#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function loads the default priors for all hierarchical DDM parameters 
# including, (if needed) the betaweight regression coefficient.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
default_priors <- function(Show=TRUE, modelType="NA", custom_prior_list=NULL){
   
   # Load default priors
   prior <- data.frame( # Normal priors for the hierarchical means
                       "bound_mean_mean" = 1.50,  "bound_mean_sdev" = 0.20,
                       "drift_mean_mean" = 0.00,  "drift_mean_sdev" = 0.50,
                       "nondt_mean_mean" = 0.30,  "nondt_mean_sdev" = 0.06,
                       # Uniform priors for the hierarchical standard deviations
                       "bound_sdev_lower" = 0.10, "bound_sdev_upper" = 0.40,
                       "drift_sdev_lower" = 0.20, "drift_sdev_upper" = 0.40,
                       "nondt_sdev_lower" = 0.05, "nondt_sdev_upper" = 0.25)
  if(modelType!="hierarchical"){
    # Normal prior for the betaweight regression coefficient
    prior$betaweight_mean = 0
    prior$betaweight_sdev = 1
  }

  # If needed, update the default priors with custom priors
  if(!is.null(custom_prior_list)){
    # For each parameter in the custom prior list
    for(param_name in names(custom_prior_list)){
      # Check if this parameter exists in the prior data frame
      if(param_name %in% names(prior)){
        # If it exists, update its value
        prior[param_name] <- custom_prior_list[[param_name]]
      }
    }
  }

  # If Show is TRUE, print the priors to the console
  if(Show){  show_priors(prior)  }

  # Return the priors
  return(prior)
}
