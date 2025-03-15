#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function creates diagnostic plots showing the MCMC chains
# for all hierarchical parameters.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

JAGS_plotChain <- function(samples){

  # Extract the 3D array of posterior samples
  # Dimensions: (iterations × chains × parameters)
  posterior.samples <- samples$BUGSoutput$sims.array
  
  # Get the names of all parameters in the posterior samples array
  labels <- names(posterior.samples[1,1,])
  
  # Find indices of hierarchical parameters (those containing underscores)
  # Hierarchical parameters typically have names like "drift_mean", "bound_sdev", etc.
  locateHier <- which(grepl("_", labels))
  
  # Count the number of hierarchical parameters
  N <- length(locateHier)
  
  # Determine the number of chains
  n.chains <- ncol(posterior.samples[,,labels[locateHier[1]]])
  

  # Plotting layout  
  par(mfrow = c(2, 2),              # 2 rows, 2 columns
      mar = c(1, 1, 1, 1),          # Minimal margins
      mai = c(0.5, 0.5, 0.5, 0.2),  # Inner margins in inches
      bty = "o")                    # Box type: "o" for complete box
  
  # Loop through each hierarchical parameter to create trace plots
  for(i in locateHier){
    # Create the base plot using the first chain
    # This establishes the plot area and axes
    plot(posterior.samples[, 1, i],  # Samples from first chain
         type = "l",                 # Line plot
         main = labels[i],           # Parameter name as title
         xlab = "Iteration",         # X-axis label
         ylab = "Value sampled")     # Y-axis label
    
    # If there are multiple chains, add them to the plot in different colors
    if(n.chains > 1){
      for(a in 2:n.chains){
        # Add each additional chain as a colored line
        # Chain 1 is black (default), chains 2-n are colors 2-n
        lines(posterior.samples[, a, i], col = a)
      }
    }
  }   
}