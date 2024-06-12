HDDM_setup <-function(nParticipants, nTrials, nDatasets=1, modelType=NA, criterion=NA, priors=NA, Show=TRUE){
    ############################################################################
    # Part 1: Parameter set up
    ############################################################################
    # Set up PRIORS
    if(sum(is.na(priors))>0){    priors <- default_priors(Show, modelType)     }
    # Identify and (optionally) print the design settings to screen
    settings <- list("nPart"= nParticipants, "nTrials"= nTrials, "prior"= priors, "criterion" = criterion)
    if(!(modelType=="hierarchical"|is.na(modelType))){   
        X <- 0:(settings$nPart-1)                      # Default predictor       
        if(modelType=="ttest"){   X <- X %% 2    }     # Dummy predictor
        settings <- c(settings, "X" = X)
    }
    if(Show){  show_design(settings)  }
    # Sample "true parameters" for the simulation using the priors
    parameter_set <- sample_parameters(settings, modelType, Show)
    
    ############################################################################
    # Part 2: Prepare data
    ############################################################################
    # Generate a single (first) data set using the settings established above
    rawData = sample_data(settings,parameter_set)
    summData = getStatistics(rawData) 
    # Establish two arrays to store nDatasets, with the first page already filled
    rawData <- array(c(rawData, rep(NA,prod(nParticipants*nTrials,3,nDatasets-1))), 
                     dim=c(nParticipants*nTrials,3,nDatasets), 
                     dimnames = list(NULL, colnames(rawData), NULL))
    summData <- array(c(summData, rep(NA,prod(nParticipants*nTrials,5,nDatasets-1))),
                      dim=c(nParticipants,5,nDatasets),
                      dimnames = list(NULL, colnames(summData), NULL))
    if(nDatasets>1){
          for(i in 2:nDatasets){
              rawData[,,i] = sample_data(settings,parameter_set)
              summData[,,i] = as.matrix(getStatistics(rawData[,,i]))
              cat("Generating dataset", i, "of", nDatasets,"\n")
          }
    }
    return(list("settings" = settings, "parameter_set" = parameter_set, "priors" = priors,
                "rawData" = rawData, "sumData" = summData))
}