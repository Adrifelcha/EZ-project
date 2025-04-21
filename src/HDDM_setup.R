###################################################################################
# MODULAR FUNCTION CODING AHEAD:
#
# HDDM_setup is a higher-level function that initiates the simulation process by 
# orchestrating all data generation processes.
###################################################################################
# Inputs:
# - priors: Data frame containing prior distribution specifications
# - nPart: Number of participants to simulate
# - nTrials: Number of trials per participant
# - modelType: Type of model ("hierarchical", "metaregression", "ttest", etc.)
# - X: Design matrix for models with predictors
# - criterion: Parameter affected by the predictor (default: "drift")
# - fromPrior: Whether to sample from prior distributions (TRUE) or uniform distributions (FALSE)
# - Show: Whether to display the sampled parameters
# Returns:
# - A list containing three elements:
#   * parameter_set: The true parameter values used in the simulation
#   * rawData: The generated trial-by-trial data
#   * sumData: Summary statistics calculated from the raw data
###################################################################################

HDDM_setup <-function(priors, nPart, nTrials, modelType=NA, X=NA, criterion=NA, fromPrior=TRUE, Show=TRUE, 
                      nTrialsPerCondition=NA, prevent_zero_accuracy=TRUE, fixedBeta=NA, withinSubject=FALSE,
                      generative_uniforms=NULL){

    # Step 1: Obtain parameter values to be used as ground truth in simulation studies    
    parameter_set <- sample_parameters(priors = priors, nPart = nPart, modelType = modelType, X = X, 
                                       criterion = criterion, fromPrior = fromPrior, Show = Show, 
                                       fixedBeta = fixedBeta, withinSubject = withinSubject, 
                                       generative_uniforms = generative_uniforms)

    # Step 2: Generate hierarchical DDM data from the parameter set, using simulation settings
    rawData = sample_data(nPart = nPart, nTrials = nTrials, parameter_set = parameter_set, 
                          nTrialsPerCondition = nTrialsPerCondition, prevent_zero_accuracy = prevent_zero_accuracy)
    

    # Step 3: Calculate EZ summary statistics from the raw data
    summData = getStatistics(data = rawData) 
    
    # Return all components needed for subsequent analysis
    return(list("parameter_set" = parameter_set,
                "rawData" = rawData, 
                "sumData" = summData))
}