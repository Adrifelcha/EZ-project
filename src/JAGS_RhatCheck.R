#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function evaluates the R-hat values computed for the posterior chains
# and identifies parameters with R-hat values greater than 1.05
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
JAGS_RhatCheck <- function(rhats){
  # Exclude deviance column (it's not a model parameter)
  if("deviance" %in% colnames(rhats)){ 
    # Identify columns containing model parameters
    model_parameters <- colnames(rhats) != "deviance"
    # Keep only these columns
    rhats <- rhats[,model_parameters]
  }
  
  # Set threshold for acceptable R-hat values
  rule <- 1.05
  
  # Find parameters with R-hat values exceeding the threshold
  bad.Rhat <- which(rhats > rule, arr.ind = TRUE)
  # Check if any 'bad' R-hat values were found
  test.rhat <- nrow(bad.Rhat) > 0

  # If problematic R-hat values found, create diagnostic plot
  if(test.rhat){
    # Default plotting space
    par(mfrow=c(1,1))
    
    # Identify parameters with convergence issues
    which.are.bad.Rhats <- colnames(rhats)[bad.Rhat[,2]]
    
    # Create histogram of all R-hat values
    hist(rhats, breaks = 50)    
    # Add vertical line at the threshold
    abline(v=rule, col="red", lty=2)    
    # Display percentage of problematic chains
    legend("top", paste("Rhat > ", rule, " | ",
                       (round(nrow(bad.Rhat)/(length(as.vector(rhats))),5))*100,
                       "% of chains | ", length(which.are.bad.Rhats), " chains", sep=""), 
           lty=2, col="red", cex=0.4)    
        
    # Print message indicating that convergence issues were found
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!\n")
    cat("Convergence issues detected in the following parameters:\n")
    cat(paste(which.are.bad.Rhats, collapse = ", "), "\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!\n")
    print(table(which.are.bad.Rhats))
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!\n")
  } else {
    # If all R-hat values are acceptable, print confirmation message

    # Header
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!\n")        
    # Print a label specifying parameters checked
    if(ncol(rhats)==3){
      cat("R-hat check (Hierarchical mean parameters):\n")  
    } else {
      cat("R-hat check (All parameters):\n")
    }
    # Print good news: No R-hat values greater than 1.05!
    cat(paste("No Rhat greater than", rule, "\n"))
    # Footer
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!\n")
  }
}
