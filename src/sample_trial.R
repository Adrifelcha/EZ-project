#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function generates a SINGLE TRIAL observation from the DDM model
# Inputs:
# - a: decision threshold
# - v: drift rate
# - dt: time step
# - max_steps: maximum number of steps
# This function emulates the random walk process implied by the DDM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_trial <- function(a, v, dt, max_steps){
  # Initialize the evidence accumulator at 0 (starting point)
  x <- 0
  
  # Generate  random  step size noise in advance 
  # (Stochastic component of evidence accumulation - Noise around the drift rate)
  random_dev <- rnorm(max_steps)  
  
  # Scale the random noise and drift rate
  # (This ensures the discrete process approximates a continuous Wiener process)
  noise <- random_dev * sqrt(dt)
  drift <- v * dt
  
  # Start the random walk!
  for(i in 2:max_steps){
    # Calculate the evidence 'sampled' in this step
    this_step = drift + noise[i]
    
    # Update the evidence accumulated so far
    x = x + this_step
    
    # Check if a decision boundary has been reached
    # The boundaries are at +a/2 and -a/2
    if(abs(x)>=(a/2)){  
      break  # Stop the simulation if a boundary is reached
    }
  }
  
  # Create the output:
  # - RT: Reaction time (number of steps (i) - 1 * time step size (dt))
  #   Note: We subtract 1 from i to avoid counting the starting point as a step
  # - C: Final evidence accumulated (x)
  #   Note: This value gets discretized in function sample_dataset()
  output <- list("RT" = (i-1)*dt, "C"  = x)
  
  return(output)
}