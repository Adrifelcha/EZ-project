default_inits <- function(n.chains,nParticipants,modelType=NA){
  myinits <- rep(list(list()), n.chains)
  if(modelType=="hierarchical"){
      for(i in 1:n.chains){
        myinits[[i]] <- list(drift = rnorm(nParticipants,0,0.4))
      }
  }else{
      for(i in 1:n.chains){
        myinits[[i]] <- list(drift = rnorm(nParticipants,0,1))
      }
  }
  return(myinits)
}