#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function prints to console the true parameter values used to sample data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
show_parameters <- function(parameter_set){
  # Print header
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
  # This is only present in models with condition effects (e.g., metaregression, ttest)
  if(!is.null(parameter_set$betaweight)){
    cat("Betaweight:   ", parameter_set$betaweight,"\n")
  }
  
  # Print footer
  cat("=============================================\n")
}