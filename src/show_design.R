#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function displays the simulation design parameters to the console
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
show_design = function(settings, withinSubjectDesign = FALSE){
      
      # Print header
      cat("========== EZBHDDM Design Parameters: ==========\n")

      # Print model information based on design type
      # Case 1: Within-subject t-test on drift rate
      if(withinSubjectDesign){
            cat("Model type:  Within-subject t-test on drift rate\n")
      } else {           
      # Case 2: Between-subjects designs
            # modelType is either "hierarchical", "metaregression", or "ttest"
            cat("Model type:              ", settings$modelType,"\n")
            
            # For "metaregression" and "ttest" models, 
            # indicate which parameter is being tested for effects
            if(!(settings$modelType=="hierarchical"|is.na(settings$modelType))){
                  cat("Testing for an effect on:", settings$criterion,"\n") 
            }
      }
            
      # Display number of participants
      cat("Number of Participants:  ", settings$nPart,"\n")
      
      # Display trial information
      if(withinSubjectDesign || !is.null(settings$nTrialsPerCondition)){
            # Within-subject design
            cat("Trials Per Condition:    ", settings$nTrialsPerCondition,"\n")
            cat("Number of Conditions:    ", 2,"\n")
      } else {
            # Between-subjects design
            cat("Trials Per Person:       ", settings$nTrials,"\n")
      }
      
      # Display number of simulation iterations
      cat('Iterations to run:       ', settings$nDatasets,"\n")
}