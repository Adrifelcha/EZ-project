nameOutput <- function(nTrials, nParticipants){
  fileName <- paste("./sim_P",nParticipants,"T",nTrials,".RData", sep="")
  return(fileName)
}