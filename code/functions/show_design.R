show_design = function(settings){
      cat("========== EZBHDDM Design Parameters: =======\n")
      cat("Model type:              ", settings$modelType,"\n")
      if(!(settings$modelType=="hierarchical"|is.na(settings$modelType))){
      cat("Testing for an effect on:", settings$criterion,"\n") 
      }
      cat("Number of Participants:  ", settings$nPart,"\n")
      if(is.null(settings$nTrialsPerCondition)){
            cat("Trials Per Person:       ", settings$nTrials,"\n")
      }else{
            cat("Trials Per Condition:    ", settings$nTrialsPerCondition,"\n")
            cat("Number of Conditions:    ", 2,"\n")
      }
      cat('Iterations to run:       ', settings$nDatasets,"\n")
}