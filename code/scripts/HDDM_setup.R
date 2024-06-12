HDDM_setup <-function(settings, modelType=NA, criterion=NA, priors=NA, Show=TRUE){
    nParticipants <- settings$nPart
    nTrials <- settings$nTrials
  
    # Sample "true parameters" for the simulation using the priors
    parameter_set <- sample_parameters(settings, modelType, Show)  
    
    rawData = sample_data(settings,parameter_set)
    summData = getStatistics(rawData) 
    # Establish two arrays to store the data, with the first page already filled
    rawData <- array(c(rawData, rep(NA,prod(nParticipants*nTrials,3,1))), 
                     dim=c(nParticipants*nTrials,3,1), 
                     dimnames = list(NULL, colnames(rawData), NULL))
    summData <- array(c(summData, rep(NA,prod(nParticipants*nTrials,5,1))),
                      dim=c(nParticipants,5,1),
                      dimnames = list(NULL, colnames(summData), NULL))
    return(list("settings" = settings, "parameter_set" = parameter_set, "priors" = priors,
                "rawData" = rawData, "sumData" = summData))
}