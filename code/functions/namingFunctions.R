nameOutput <- function(nTrials, nParticipants, nDatasets, modelType=NA, fromPrior=NA, output.folder = NA){
  if(is.na(output.folder)){       output.folder <- "./"        }
  
  start <- paste(output.folder,"sim_P", sep="")
  
  if(is.na(modelType)|modelType=="hierarchical"){
          fileName <- paste(start,nParticipants,"T",nTrials,"D",nDatasets,
                            "_EZBHDDM", sep="")
  }else{if(modelType=="metaregression"){
          fileName <- paste(start,nParticipants,"T",nTrials,"D",nDatasets,
                            "_MetaRegEZBHDDM", sep="")
  }else{if(modelType=="ttest"){
          fileName <- paste(start,nParticipants,"T",nTrials,"D",nDatasets,
                            "_ttestEZBHDDM", sep="")
  }else{  stop("Invalid modelType")  }}}
  
  if(fromPrior){
        fileName <- paste(fileName,"_genPrior.RData", sep="")
  }else{
        fileName <- paste(fileName,"_genUnif.RData", sep="")
  }
  
  return(fileName)
}