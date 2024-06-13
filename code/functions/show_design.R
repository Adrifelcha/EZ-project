show_design = function(settings){
      cat("========== EZBHDDM Design Parameters: =======\n")
      cat("Model type:              ", settings$modelType,"\n")
      if(!(settings$modelType=="hierarchical"|is.na(settings$modelType))){
      cat("Testing for an effect on:", settings$criterion,"\n") 
      }
      cat("Number of Participants:  ", settings$nPart,"\n")
      cat("Trials Per Person:       ", settings$nTrials,  "\n")
      cat('Iterations to run:       ', settings$nDatasets,"\n")
}