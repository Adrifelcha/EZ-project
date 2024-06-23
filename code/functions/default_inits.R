default_inits <- function(n.chains,nParticipants,modelType=NA){
  myinits <- rep(list(list()), n.chains)
  for(i in 1:n.chains){
    myinits[[i]] <- list(drift = rnorm(nParticipants,0,0.5))
  }
  return(myinits)
}