# Simulate over 'n' trials
#~~~~~~~~~~~~~~~~~~~~~~~~~!
generate_dataset <- function(a,v,t,n){
  dt = 0.001
  max_steps = 10 / dt
  rt = rep(NA,n)
  accuracy = rep(NA,n)
  
  for(i in 1:n){
    X <- generate_trial(a, v, dt, max_steps)
    rt[i] <- X$RT 
    if(X$C>0){  accuracy[i] <- 1
    }else{    accuracy[i] <- 0  }
  }
  output <- data.frame("RT" = rt + t, "accuracy" = accuracy)
  return(output)
}