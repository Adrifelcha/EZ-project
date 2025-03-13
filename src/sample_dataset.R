#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function generates a DATASET (size n) of observations from the DDM model
# Inputs:
# - a: decision threshold
# - v: drift rate
# - t: non-decision time
# - n: number of trials
# This function calls sample_trial() n times to generate a dataset of n trials
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_dataset <- function(a,v,t,n){
  # Set random walk emulation parameters
  dt = 0.001  # Time step size (in seconds)
  max_steps = 10 / dt  # Maximum number of steps (~10 seconds)
  
  # Initialize vectors to store results
  rt = rep(NA,n)        # Reaction times
  accuracy = rep(NA,n)  # Accuracy (1 = correct, 0 = incorrect)
  
  # Generate n trials
  for(i in 1:n){
    # Generate a single DDM trial
    X <- sample_trial(a, v, dt, max_steps)
    
    # Store the reaction time (without non-decision time for now)
    rt[i] <- X$RT 
    
    # Determine accuracy based on the final evidence value
    if(X$C > 0){  
      # Hitting the upper boundary is considered a "correct" response
      accuracy[i] <- 1
    } else {      
      # Hitting the lower boundary is considered an "incorrect" response
      accuracy[i] <- 0  
    }
  }

  RT <- rt + t  # Add the non-decision time (t) to all reaction times

  # Create output data frame with reaction times and accuracy
  output <- data.frame("RT" = RT, "accuracy" = accuracy)
  return(output)
}