HDDM_setup <-function(settings, modelType=NA, Show=TRUE){
    nParticipants <- settings$nPart
    nTrials <- settings$nTrials
    # Sample "true parameters" for the simulation using the priors
    parameter_set <- sample_parameters(settings, modelType, Show)  
    # Generate data
    rawData = sample_data(settings,parameter_set)
    summData = getStatistics(rawData) 
    return(list("parameter_set" = parameter_set, "rawData" = rawData, "sumData" = summData))
}