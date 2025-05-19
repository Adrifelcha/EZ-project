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

######################################
# Main function
######################################
# Sample data using simulation settings and true parameter values sampled
sample_data <- function(nPart, nTrials = NA, parameter_set, 
                        nTrialsPerCondition = NA, prevent_zero_accuracy = TRUE){
  
  design <- identify_design(nPart, nTrials, nTrialsPerCondition, parameter_set)
            data <- design$data
            nObs <- design$nObs
            nCols <- design$nCols
            n_subIndex <- design$n_subIndex
            col_accuracy <- design$col_accuracy
            col_rt <- design$col_rt
            N <- design$N
            colNames <- design$colNames
            adjusted_parameter_set <- design$adjusted_parameter_set
            cell_index <- design$cell_index
            n_par_sets <- length(adjusted_parameter_set$bound)

  for(i in 1:n_par_sets){
        # Identify rows for current participant
        this.cell <- which(cell_index==i)

        params <- list(bound = parameter_set$bound[i],
                      drift = parameter_set$drift[i],
                      nondt = parameter_set$nondt[i])
        # Generate dataset for this participant using their specific parameters
        # First generate the dataset once
        temp <- get_data(nPart, N, params, prevent_zero_accuracy)
        data[this.cell,col_accuracy] <- temp$accuracy
        data[this.cell,col_rt] <- temp$RT
  }
  # Convert to matrix and add column names
  data <- as.matrix(data)
  colnames(data) <- colNames
  
return(data)
}
########################################################################################
########################################################################################

#+
#+
#+
#+
#+
#+

########################################################################################
#   Auxiliary functions
########################################################################################

##############################################
# Identify relevant design parameters based
# on whether this is a within-subject design
##############################################
identify_design <- function(nPart, nTrials, nTrialsPerCondition, parameter_set){
      withinSubject <- !is.na(nTrialsPerCondition)
      if(withinSubject){
          nObs <- nPart*nTrialsPerCondition*2
          nCols <- 4
          colNames <- c("sub","cond","rt", "accuracy")
          n_subIndex <- nTrialsPerCondition*2
          col_accuracy <- 4
          col_rt <- 3
          N <- nTrialsPerCondition
          adjusted_parameter_set <- list(
                  bound = rep(parameter_set$bound, each=2),
                  drift = parameter_set$drift,
                  nondt = rep(parameter_set$nondt, each=2))
      } else {
          nObs <- nPart*nTrials
          nCols <- 3
          colNames <- c("sub","rt", "accuracy")
          n_subIndex <- nTrials
          col_accuracy <- 3
          col_rt <- 2
          N <- nTrials
          adjusted_parameter_set <- parameter_set
      }

      data <- matrix(NA, ncol=nCols, nrow=nObs)  
      data[,1] <- rep(1:nPart, each=n_subIndex)
      cell_index <- data[,1]

      if(withinSubject){
          data[,2] <- rep(rep(c(1,0), each=nTrialsPerCondition), nPart)
          cell_index <- rep(1:nPart*2, each=nTrialsPerCondition)
      }

  return(list(data = data, nObs = nObs,
              nCols = nCols, N = N,
              n_subIndex = n_subIndex,
              col_accuracy = col_accuracy,
              col_rt = col_rt,
              colNames = colNames,
              adjusted_parameter_set = adjusted_parameter_set,
              cell_index = cell_index))
}

##############################################################
# Generate dataset for a particular set of parameter values
##############################################################
get_data <- function(nPart, N, params, prevent_zero_accuracy){
      # Generate dataset for this participant using their specific parameters
      # First generate the dataset once
      temp <- sample_many_trials(a = params$bound, v = params$drift, t = params$nondt, n = N)
      accuracy <- temp$accuracy

      # If prevent_zero_accuracy is TRUE and we got all zeros, keep trying
      while(sum(accuracy)==0 && prevent_zero_accuracy){
        temp <- sample_many_trials(a = params$bound, v = params$drift, t = params$nondt, n = N)
        accuracy <- temp$accuracy
      }

return(temp)
}