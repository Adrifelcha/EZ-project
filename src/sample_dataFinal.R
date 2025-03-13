#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function generates a complete dataset for simulation studies using the DDM model
# Ex: It creates data for multiple participants with individual parameter values.
#
# Inputs:
# - nPart: Number of participants to simulate
# - nTrials: Total number of trials per participant (used when no conditions)
# - parameter_set: A list containing parameter values for each participant:
#   * bound: Decision threshold (a) for each participant
#   * drift: Drift rate (v) for each participant/condition
#   * nondt: Non-decision time (t) for each participant
# - nTrialsPerCondition: Number of trials per condition (used when conditions exist)
#
# Returns:
# - A matrix with columns for participant ID, reaction time, accuracy
#   (and condition, if applicable)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# Sample data using simulation settings and true parameter values sampled
sample_data <- function(nPart, nTrials = NA, parameter_set, 
                        nTrialsPerCondition = NA, prevent_zero_accuracy = TRUE){
  # Case 1: No conditions specified (single condition design)
  if(is.na(nTrialsPerCondition)){
        # Calculate total number of observations and initialize data matrix
        nObs <- nPart*nTrials
        data <- matrix(NA, ncol=3, nrow=nObs)
        
        # Assign participant IDs to each row
        data[,1] <- rep(1:nPart, each=nTrials)
        
        # Generate data for each participant
        for(i in 1:nPart){
          # Identify rows for current participant
          this.sub <- which(data[,1]==i)
          
          # Generate dataset for this participant using their specific parameters
          accuracy = 0
          while(sum(accuracy)==0 && prevent_zero_accuracy){
            temp <- sample_dataset(a = parameter_set$bound[i], 
                                     v = parameter_set$drift[i], 
                                     t = parameter_set$nondt[i], 
                                     n = nTrials)
            accuracy = temp$accuracy
          }
          
          # Store reaction times and accuracy in the data matrix
          data[this.sub,3] <- accuracy
          data[this.sub,2] <- temp$RT
        }
        
        # Convert to matrix and add column names
        data <- as.matrix(data)
        colnames(data) <- c("sub", "rt", "accuracy")
  
  # Case 2: Conditions specified (factorial design)
  } else {
        # Calculate total observations (participants × trials × conditions)
        # Assumes 2 conditions per participant
        nObs <- nPart*nTrialsPerCondition*2
        data <- matrix(NA, ncol=4, nrow=nObs)
        
        # Assign participant IDs
        data[,1] <- rep(1:nPart, each=(nTrialsPerCondition*2))
        
        # Assign condition IDs (alternating blocks of 1s and 0s for each participant)        
        data[,2] <- rep(rep(c(1,0), each=nTrialsPerCondition), nPart)
        
        # Counter for drift parameter index
        j = 1
        
        # Generate data for each participant and condition
        for(i in 1:nPart){
            # For each condition (1 and 0)
            for(k in c(1,0)){
                # Identify rows for current participant and condition
                this.cell <- which(data[,1]==i & data[,2]==k)
                
                # Generate dataset ensuring non-zero accuracy
                accuracy = 0
                while(sum(accuracy)==0 && prevent_zero_accuracy){
                  temp <- sample_dataset(a = parameter_set$bound[i], 
                                           v = parameter_set$drift[j], 
                                           t = parameter_set$nondt[i], 
                                           n = nTrialsPerCondition)
                  accuracy = temp$accuracy
                }
                
                # Store accuracy and reaction times
                data[this.cell,4] <- accuracy
                data[this.cell,3] <- temp$RT
                
                # Increment drift parameter index
                # This allows different drift rates for different conditions
                j = j+1
            }
        }
        
        # Convert to matrix and add column names
        data <- as.matrix(data)
        colnames(data) <- c("sub","cond","rt", "accuracy")    
  }
  
  return(data)
}