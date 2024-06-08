# A function to print the parameter values used to generate data used in the simulation.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
show_parameters <- function(parameter_set){
  cat("===== EZBHDDM True Parameters: ==============\n")
  if(!is.null(parameter_set$drift_B0)){  # Simple hierarchical model
    cat("Drift Intercept:   ", parameter_set$drift_intercept,"\n")
    if(!is.null(parameter_set$drift_B1)){
      cat("Drift Coefficient: ", parameter_set$drift_coefficient,"\n")
    }
    cat("Drift SD:          ", parameter_set$drift_sdev,"\n") 
  }else{
    cat("Drift Mean:   ", parameter_set$drift_mean,"\n")
    cat("Drift Error:     ", parameter_set$drift_sdev,"\n")             
  }
  cat("Bound Mean:   ", parameter_set$bound_mean,"\n")
  cat("Bound SD:     ", parameter_set$bound_sdev,"\n")
  cat("Non-decision Time Mean:", parameter_set$nondt_mean,"\n")
  cat("Non-decision Time SD:  ", parameter_set$nondt_sdev,"\n")
  cat("=============================================\n")
}