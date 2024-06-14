HDDM_setup <-function(priors, nPart, nTrials, modelType=NA, X=NA, criterion=NA, fromPrior=TRUE, Show=TRUE){
    # Sample "true parameters" for the simulation using the priors
    parameter_set <- sample_parameters(priors, nPart, modelType, X, criterion, fromPrior, Show)
    # Generate data
    rawData = sample_data(nPart, nTrials, parameter_set)
    summData = getStatistics(rawData) 
    return(list("parameter_set" = parameter_set, "rawData" = rawData, "sumData" = summData))
}