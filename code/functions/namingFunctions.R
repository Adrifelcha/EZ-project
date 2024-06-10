nameOutput <- function(nTrials, nParticipants, nDatasets,Design.Label="EZBHDDM"){
  fileName <- paste("./sim_P",nParticipants,"T",nTrials,"D",nDatasets,"_",
                    Design.Label,".RData", sep="")
  return(fileName)
}