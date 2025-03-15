#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function creates diagnostic plots showing the MCMC chains
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

JAGS_plotChain <- function(samples, track_allParameters){
  # Extract the 3D array of MCMC samples (iterations × chains × parameters)
  posterior.samples <- samples$BUGSoutput$sims.array
  
  # Get the names of all parameters in the MCMC output
  labels <- names(posterior.samples[1,1,])
  
  # Find indices of hierarchical parameters (those containing underscores)
  # Hierarchical parameters typically have names like "drift_mean", "bound_sdev", etc.
  locateHier <- which(grepl("_", labels))
  
  # Count the number of hierarchical parameters
  N <- length(locateHier)
  
  # Determine the number of MCMC chains used
  n.chains <- ncol(posterior.samples[,,labels[locateHier[1]]])
  
  # Set up the plotting layout based on whether we're tracking all parameters
  if(track_allParameters){
    # Larger layout (2 rows, 3 columns) for tracking more parameters
    par(mfrow = c(2, 3), 
        mar = c(5.1, 2, 4.1, 2),  # Margins: bottom, left, top, right
        bty = "o")                # Box type: "o" for complete box
  } else {
    # Smaller layout (2 rows, 2 columns) for tracking fewer parameters
    par(mfrow = c(2, 2), 
        mar = c(1, 1, 1, 1),      # Minimal margins
        mai = c(0.5, 0.5, 0.5, 0.2),  # Inner margins in inches
        bty = "o")                # Box type: "o" for complete box
  }
  
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
  
  # Note: The function doesn't return any values, it produces graphical output
}