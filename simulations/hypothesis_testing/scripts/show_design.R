show_design = function(settings){
      cat("== Within-subject t-test design on the drift rate\n")
      cat("Number of Participants:  ", settings$nPart,"\n")
      cat("Trials Per Condition:    ", settings$nTrialsPerCondition,"\n")
      cat("Number of Conditions:    ", 2,"\n")
      cat('Iterations to run:       ', settings$nDatasets,"\n")
}