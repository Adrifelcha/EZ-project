#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function specifies which data objects need to be passed to the JAGS model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# Inputs:
# - modelType: Type of model being used
#   * "hierarchical": Basic hierarchical model (default if NA)
#   * Other values (e.g., "metaregression", "ttest"): Models that include predictors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
JAGS_passData <- function(modelType=NA){
  # Define the basic data variables needed for all model types
  passData <- list("nParticipants", "nTrialsPerPerson",
                   "meanRT", "varRT", "correct")
  
  # For models that include predictors (i.e., metaregression, t-test),
  # we also need to pass the predictor vector X
  if(!(is.na(modelType)|modelType=="hierarchical")){
          # Add the predictor variable X to the list of data to pass
          passData <- c(passData, "X")
  }
  
  # Return the list of data to pass to JAGS
  return(passData)
}