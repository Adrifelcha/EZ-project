default_inits <- function(n.chains,nParticipants){
  myinits <- rep(list(list()), n.chains)
  for(i in 1:n.chains){
    myinits[[i]] <- list(drift = rnorm(nParticipants,0,0.1))
  }
  return(myinits)
}