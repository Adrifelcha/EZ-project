# A function to print the default prior values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
show_priors <- function(prior){
      cat("========== EZBHDDM Priors: ==================\n")
      cat("Drift rate:\n")
      cat("Drift Mean Mean:", prior$drift_mean_mean,"\n")
      cat("Drift Mean Std Dev:",prior$drift_mean_sdev,"\n")
      cat("Drift Std Dev Lower-bound:",prior$drift_sdev_lower,"\n")
      cat("Drift Std Dev Upper-bound:",prior$drift_sdev_upper,"\n")
      cat("Bound:\n")
      cat("Bound Mean Mean:   ", prior$bound_mean_mean,"\n")
      cat("Bound Mean Std Dev:",prior$bound_mean_sdev,"\n")
      cat("Bound Std Dev Lower-bound:",prior$bound_sdev_lower,"\n")
      cat("Bound Std Dev Upper-nound:",prior$bound_sdev_upper,"\n")
      cat("Non-decision Time:\n")
      cat("Non-decision Time Mean Mean:",prior$nondt_mean_mean,"\n")
      cat("Non-decision Time Mean Std: ", prior$nondt_mean_sdev,"\n")
      cat("Non-decision Time Lower-bound:",prior$nondt_sdev_lower,"\n")
      cat("Non-decision Time Upper-bound:",prior$nondt_sdev_upper,"\n")
      if(!is.null(prior$betaweight_mean)){
      cat("Betaweight:\n")
      cat("Betaweight Mean:", prior$betaweight_mean,"\n")
      cat("Betaweight Std:", prior$betaweight_sdev,"\n")  
      }
}