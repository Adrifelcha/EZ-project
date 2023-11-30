###############################################################################
# Functions to load and prepare variables needed to work with JAGS
###############################################################################

# A function to create a list with all the data objects in the JAGS model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
data_toJAGS <- function(){
  passData <- list("nParticipants", "nTrialsPerPerson",
                   "meanRT", "varRT", "correct")
  return(passData)
}

default_inits <- function(n.chains,nParticipants){
  myinits <- rep(list(list()), n.chains)
  for(i in 1:n.chains){
    myinits[[i]] <- list(drift = rnorm(nParticipants,0,0.1))
  }
  return(myinits)
}