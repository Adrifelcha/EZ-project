###########################################################
#  Functions to simulate bivariate data under DDM
###########################################################

# Simulate single trial outcome
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
simulate_ddm <- function(a, v, dt, max_steps){
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

# Simulate over 'n' trials
#~~~~~~~~~~~~~~~~~~~~~~~~~!
wdmrnd <- function(a,v,t,n){
  dt = 0.001
  max_steps = 10 / dt
  rt = rep(NA,n)
  accuracy = rep(NA,n)
  
  for(i in 1:n){
    X <- simulate_ddm(a, v, dt, max_steps)
    rt[i] <- X$RT 
    if(X$C>0){  accuracy[i] <- 1
    }else{    accuracy[i] <- 0  }
  }
  output <- data.frame("RT" = rt + t, "accuracy" = accuracy)
  return(output)
}