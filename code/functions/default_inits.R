default_inits <- function(n.chains,nParticipants,modelType=NA){
  myinits <- rep(list(list()), n.chains)
  for(i in 1:n.chains){
    myinits[[i]] <- list(drift = rnorm(nParticipants,0,0.5))
                         #bound = rnorm(nParticipants,2.25,0.4))
                         #bound_mean = 2.25,
                         #drift_mean = 0,
                         #nondt_mean = 0.55)
                         #drift_sdev = 0.5,
                         #bound_sdev = 0.4,
                         #nondt_sdev = 0.11)
  }
  return(myinits)
}