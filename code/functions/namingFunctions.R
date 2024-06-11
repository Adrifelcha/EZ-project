nameOutput <- function(nTrials, nParticipants, nDatasets, modelType=NA){
  if(is.na(modelType)|modelType=="hierarchical"){
          fileName <- paste("./sim_P",nParticipants,"T",nTrials,"D",nDatasets,
                            "_EZBHDDM.RData", sep="")
  }else{if(modelType=="metaregression"){
          fileName <- paste("./sim_P",nParticipants,"T",nTrials,"D",nDatasets,
                            "_MetaRegEZBHDDM.RData", sep="")
  }else{if(modelType=="ttest"){
          fileName <- paste("./sim_P",nParticipants,"T",nTrials,"D",nDatasets,
                            "_ttestEZBHDDM.RData", sep="")
  }else{  stop("Invalid modelType")  }}}
  return(fileName)
}