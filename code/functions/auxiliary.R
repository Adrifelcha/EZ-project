############################################################################
# Auxiliary/Miscellaneous functions
############################################################################

# A function to create outputFile name from the number of trials/participants
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
nameOutput <- function(nTrials, nParticipants){
  fileName <- paste("./sim_P",nParticipants,"T",nTrials,".RData", sep="")
  return(fileName)
}