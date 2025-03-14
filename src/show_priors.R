#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function prints to the console the values used to define the 
# hierarchical prior distributions in the EZBHDDM model.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
show_priors <- function(prior){
      cat("========== EZBHDDM Priors: ==================\n")
      
      # Check which distribution is used for the hierarchical standard deviations
      # Case 1: Uniform prior
      if(all(c("drift_sdev_lower", "drift_sdev_upper") %in% names(prior))){
        par1 <- "lower-bound"
        par2 <- "upper-bound"
        drift_sdev_1 <- prior$drift_sdev_lower
        drift_sdev_2 <- prior$drift_sdev_upper   
        bound_sdev_1 <- prior$bound_sdev_lower
        bound_sdev_2 <- prior$bound_sdev_upper
        nondt_sdev_1 <- prior$nondt_sdev_lower
        nondt_sdev_2 <- prior$nondt_sdev_upper
      } else if(all(c("drift_sdev_shape", "drift_sdev_scale") %in% names(prior))){
        # Case 2: shape-scale prior
        par1 <- "shape parameter"
        par2 <- "scale parameter"
        drift_sdev_1 <- prior$drift_sdev_shape
        drift_sdev_2 <- prior$drift_sdev_scale
        bound_sdev_1 <- prior$bound_sdev_shape
        bound_sdev_2 <- prior$bound_sdev_scale
        nondt_sdev_1 <- prior$nondt_sdev_shape
        nondt_sdev_2 <- prior$nondt_sdev_scale
      } else {  cat("Unknown distribution for the hierarchical standard deviations\n")  }


      # Display drift rate priors
      cat(">> Drift rate:\n")
      cat("Drift MEAN - mean:", prior$drift_mean_mean,"\n")
      cat("Drift MEAN - stdv:",prior$drift_mean_sdev,"\n")
      cat("Drift STDV -",par1,":",drift_sdev_1,"\n")
      cat("Drift STDV -",par2,":",drift_sdev_2,"\n")
      
      # Display boundary separation priors
      cat(">> Boundary separation:\n")
      cat("Bound MEAN - mean:", prior$bound_mean_mean,"\n")
      cat("Bound MEAN - stdv:",prior$bound_mean_sdev,"\n")
      cat("Bound STDV -",par1,":",bound_sdev_1,"\n")
      cat("Bound STDV -",par2,":",bound_sdev_2,"\n")
      
      # Display non-decision time priors
      cat(">> Nondecision time:\n")
      cat("Nondecision time MEAN - mean:",prior$nondt_mean_mean,"\n")
      cat("Nondecision time MEAN - stdv:", prior$nondt_mean_sdev,"\n")
      cat("Nondecision time STDV -",par1,":",nondt_sdev_1,"\n")
      cat("Nondecision time STDV -",par2,":",nondt_sdev_2,"\n")
      
      # Check if a betaweight parameter is included in the model      
      betaweight_cols <- grep("betaweight", names(prior), value = TRUE)      
      # If any betaweight columns exist, display them
      if(length(betaweight_cols) > 0){
             cat(">> Betaweight:\n")
             # For each betaweight column, display its name and value
             for(col in betaweight_cols){
                   # Extract the part after "betaweight_" to use as label
                   param_type <- sub("betaweight_", "", col)
                   cat("Betaweight ", param_type, ": ", prior[[col]], "\n", sep="")
             }
      }
}