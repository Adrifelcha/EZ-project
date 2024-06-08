# Simulate single trial outcome
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
generate_trial <- function(a, v, dt, max_steps){
  x <- 0   
  random_dev <- rnorm(max_steps)  
  noise <- random_dev * sqrt(dt)
  drift <- v * dt
  for(i in 2:max_steps){
    this_step = drift + noise[i]
    x = x + this_step
    if(abs(x)>=(a/2)){  break  }
  }
  output <- list("RT" = (i+1)*dt, "C"  = x)
  return(output)
}