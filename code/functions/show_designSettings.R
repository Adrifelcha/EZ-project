#######################################################################
#  Functions to print to screen design settings 
#######################################################################

# A function to print the default prior values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
show_priors <- function(prior){
  cat("========== EZBHDDM Priors: ==================\n")
  cat("Drift rate:\n")
  if(!is.null(prior$drift_mean_mean)){  # Simple hierarchical model
      cat("Drift Mean Mean:", prior$drift_mean_mean,"\n")
      cat("Drift Mean Std Dev:",prior$drift_mean_sdev,"\n")
      cat("Drift Std Dev Shape:",prior$drift_sdev_lower,"\n")
      cat("Drift Std Dev Scale:",prior$drift_sdev_upper,"\n")
  }else{  # Meta regression on the drift rate
      cat("Drift Intercept Mean:", prior$drift_intercept_mean,"\n")
      cat("Drift Intercept Std Dev:", prior$drift_intercept_sdev,"\n")
      cat("Drift Coefficient Mean:",prior$drift_coefficient_mean,"\n")
      cat("Drift Coefficient Std Dev:",prior$drift_coefficient_sdev,"\n")
      cat("Drift Error Shape:",prior$drift_sdev_lower,"\n")
      cat("Drift Error Scale:",prior$drift_sdev_upper,"\n")
  }
  cat("Bound distance:\n")
  cat("Bound Mean Mean:   ", prior$bound_mean_mean,"\n")
  cat("Bound Mean Std Dev:",prior$bound_mean_sdev,"\n")
  cat("Bound Std Dev Shape:",prior$bound_sdev_lower,"\n")
  cat("Bound Std Dev Scale:",prior$bound_sdev_upper,"\n")
  cat("Non-decision Time:\n")
  cat("Non-decision Time Mean Mean:",prior$nondt_mean_mean,"\n")
  cat("Non-decision Time Mean Std: ", prior$nondt_mean_sdev,"\n")
  cat("Non-decision Time Shape:",prior$nondt_sdev_lower,"\n")
  cat("Non-decision Time Scale:",prior$nondt_sdev_upper,"\n")
}

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