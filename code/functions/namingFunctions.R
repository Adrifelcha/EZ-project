nameOutput <- function(nTrials, nParticipants, nDatasets, modelType=NA, fromPrior=NA){
  if(is.na(modelType)|modelType=="hierarchical"){
          fileName <- paste("./sim_P",nParticipants,"T",nTrials,"D",nDatasets,
                            "_EZBHDDM", sep="")
  }else{if(modelType=="metaregression"){
          fileName <- paste("./sim_P",nParticipants,"T",nTrials,"D",nDatasets,
                            "_MetaRegEZBHDDM", sep="")
  }else{if(modelType=="ttest"){
          fileName <- paste("./sim_P",nParticipants,"T",nTrials,"D",nDatasets,
                            "_ttestEZBHDDM", sep="")
  }else{  stop("Invalid modelType")  }}}
  
  if(fromPrior){
        fileName <- paste(fileName,"_genPrior.RData", sep="")
  }else{
        fileName <- paste(fileName,"_genUnif.RData", sep="")
  }
  
  return(fileName)
}