#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function generates standardized filenames for simulation output files
# Note: This function ISN'T made to work for within-subject designs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
nameOutput <- function(nTrials, nParticipants, nDatasets, modelType=NA, fromPrior=NA, output.folder = NA){
  # Set default output folder to current directory if not specified
  if(is.na(output.folder)){
    output.folder <- here("output")
  }
  
  # Filename should specify the number of participants and trials, and the number of datasets
  start <- paste("sim_P", nParticipants,"T", nTrials,"D", nDatasets, sep="")
  
  # Filename should specify the model type
  if(is.na(modelType)|modelType=="hierarchical"){
          # Basic hierarchical model
          fileName <- paste(start, "_EZBHDDM", sep="")
  }else{if(modelType=="metaregression"){
          # Meta-regression model
          fileName <- paste(start, "_MetaRegEZBHDDM", sep="")
  }else{if(modelType=="ttest"){
          # T-test model
          fileName <- paste(start, "_ttestEZBHDDM", sep="")
  }else{  
          # Invalid model type
          stop("Invalid modelType")  
  }}}
  
  # Filename should specify the parameter generation method
  if(fromPrior){
        # Parameters generated from prior distributions
        fileName <- paste(fileName,"_genPrior.RData", sep="")
  }else{
        # Parameters generated from uniform distributions
        fileName <- paste(fileName,"_genUnif.RData", sep="")
  }
  
  return(fileName)
}