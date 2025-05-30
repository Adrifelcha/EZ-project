#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function generates a SINGLE TRIAL observation from the DDM model by
# emulating the random walk process implied by the DDM
# Inputs:
# - a: decision threshold
# - v: drift rate
# - dt: time step
# - max_steps: maximum number of steps
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
sample_trial <- function(a, v, dt, max_steps){
    x <- 0  # Initialize the evidence accumulator at 0 (starting point)

    # Generate random step-size noise in advance - Noise around the drift rate
    random_dev <- rnorm(max_steps)

    # Scale the random noise and drift rate to approximate a continuous Wiener process
    noise <- random_dev * sqrt(dt)
    drift <- v * dt
    
    # Compute the evidence sampled on each step
    steps <- drift + noise
    
    # Start the random walk
    for(i in 2:max_steps){
          # Update the evidence accumulator on each step
          x = x + steps[i]
          
          # Check if a decision boundary has been reached
          # The boundaries are at +a/2 and -a/2
          if(abs(x)>=(a/2)){
            break  # Stop the simulation if a boundary is reached
          }
    }

    # Create the output:
    # - RT: Reaction time (number of steps (i) - initial step * time step size (dt))
    # - C: Final evidence accumulated (x)  
    output <- list("RT" = (i-1)*dt, "C"  = x)
return(output)
}