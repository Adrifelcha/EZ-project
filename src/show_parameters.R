# A function to print the parameter values used to generate data used in the simulation.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
show_parameters <- function(parameter_set){
  cat("===== EZBHDDM True Parameters: ==============\n")
  cat("Drift Mean:   ", parameter_set$drift_mean,"\n")
  cat("Drift SD:     ", parameter_set$drift_sdev,"\n")             
  cat("Bound Mean:   ", parameter_set$bound_mean,"\n")
  cat("Bound SD:     ", parameter_set$bound_sdev,"\n")
  cat("Non-decision Time Mean:", parameter_set$nondt_mean,"\n")
  cat("Non-decision Time SD:  ", parameter_set$nondt_sdev,"\n")
  if(!is.null(parameter_set$betaweight)){
  cat("Betaweight:   ", parameter_set$betaweight,"\n")
  }
  cat("=============================================\n")
}