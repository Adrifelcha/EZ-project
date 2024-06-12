# A function to create a list with all the data objects in the JAGS model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
data_toJAGS <- function(modelType=NA){
  passData <- list("nParticipants", "nTrialsPerPerson",
                   "meanRT", "varRT", "correct")
  if(!(is.na(modelType)|modelType=="hierarchical")){
          passData <- c(passData, "X")
  }
  return(passData)
}