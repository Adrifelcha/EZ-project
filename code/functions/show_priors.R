# A function to print the default prior values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
show_priors <- function(prior){
      cat("========== EZBHDDM Priors: ==================\n")
      cat("Drift rate:\n")
      cat("Drift Mean Mean:", prior$drift_mean_mean,"\n")
      cat("Drift Mean Std Dev:",prior$drift_mean_sdev,"\n")
      cat("Drift Std Dev Shape:",prior$drift_sdev_lower,"\n")
      cat("Drift Std Dev Scale:",prior$drift_sdev_upper,"\n")
      cat("Bound:\n")
      cat("Bound Mean Mean:   ", prior$bound_mean_mean,"\n")
      cat("Bound Mean Std Dev:",prior$bound_mean_sdev,"\n")
      cat("Bound Std Dev Shape:",prior$bound_sdev_lower,"\n")
      cat("Bound Std Dev Scale:",prior$bound_sdev_upper,"\n")
      cat("Non-decision Time:\n")
      cat("Non-decision Time Mean Mean:",prior$nondt_mean_mean,"\n")
      cat("Non-decision Time Mean Std: ", prior$nondt_mean_sdev,"\n")
      cat("Non-decision Time Shape:",prior$nondt_sdev_lower,"\n")
      cat("Non-decision Time Scale:",prior$nondt_sdev_upper,"\n")
      if(!is.null(prior$betaweight_lower)){
      cat("Betaweight:\n")
      cat("Betaweight Lower-bound:", prior$betaweight_lower,"\n")
      cat("Betaweight Upper-bound:", prior$betaweight_upper,"\n")  
      }
}