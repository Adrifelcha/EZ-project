#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function takes the raw data (data) and returns the summary statistics 
# needed by the EZ-DDM model:
# - mean accuracy (and number of correct responses)
# - mean RT
# - variance of RT
# These summary statistics are computed per cell design
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

# Get individual statistics from raw data: mean accuracy and mean and variance of correct-RT
getStatistics <- function(data){
    # Check if required columns exist in the data
    if(is.null(data[,"accuracy"])|is.null(data[,"rt"])){
      stop("Data not available.")
    }
    
    # Case 1: Data has only 3 columns (subject, accuracy, rt) - no condition variable
    if(ncol(data)==3){
        # Extract unique subject IDs
        subID <- unique(data[,"sub"])
        
        # Calculate sum of correct responses per subject
        sum_correct <- tapply(data[,"accuracy"], data[,"sub"], sum)  
        
        # Remove participants with no correct answers
        # This is necessary because EZ-DDM can't handle zero accuracy
        always_0 <- which(sum_correct==0)
        if(length(always_0)!=0){
          bad_participants <- (data[,"sub"] %in% always_0)
          data <- data[-bad_participants,]
          # Recalculate sum_correct after removing participants
          sum_correct <- tapply(data[,"accuracy"], data[,"sub"], sum) 
        }
        
        # Calculate mean accuracy per subject
        mean_accuracy <- tapply(data[,"accuracy"], data[,"sub"], mean) 
        
        # Calculate mean reaction time per subject
        mean_rt <- tapply(data[,"rt"], data[,"sub"], mean)
        
        # Calculate variance of reaction time per subject
        var_rt  <- tapply(data[,"rt"], data[,"sub"], var)
        
        # Combine all statistics into a single matrix
        data_statistics <- cbind(subID, sum_correct, mean_accuracy, mean_rt, var_rt)
        data_statistics <- as.matrix(data_statistics)
        colnames(data_statistics) = c("sub", "sum_correct","meanAccuracy", "meanRT", "varRT")
    
    # Case 2: Data has more than 3 columns, including a condition variable
    }else{
        # Calculate sum of correct responses per subject and condition
        sum_correct <- tapply(data[,"accuracy"], list(data[,"sub"], data[,"cond"]), sum)
        
        # Calculate mean accuracy per subject and condition
        mean_accuracy <- tapply(data[,"accuracy"], list(data[,"sub"], data[,"cond"]), mean) 
        
        # Calculate mean reaction time per subject and condition
        mean_rt <- tapply(data[,"rt"], list(data[,"sub"], data[,"cond"]), mean)
        
        # Calculate variance of reaction time per subject and condition
        var_rt  <- tapply(data[,"rt"], list(data[,"sub"], data[,"cond"]), var)
        
        # Extract unique subject and condition IDs
        subID <- unique(data[,"sub"])
        condID <- unique(data[,"cond"])
        
        # Create vectors for the subject and condition columns in the output
        # Each subject will have a row for each condition
        sub <- rep(subID, each=2)  # Assumes exactly 2 conditions
        cond <- rep(condID, length(subID))
        
        # Initialize the output matrix
        data_statistics <- matrix(NA, ncol=6, nrow=length(cond))
        colnames(data_statistics) = c("sub", "cond", "sum_correct","meanAccuracy", "meanRT", "varRT")
        
        # Fill in subject and condition columns
        data_statistics[,"sub"] <- sub
        data_statistics[,"cond"] <- cond
        
        # Fill in statistics for each condition
        # The seq() function creates indices to place values in the correct rows
        for(i in condID){
            # Calculate row indices for current condition (alternating rows)
            # Note: condID values are 0 and 1            
            row_indices <- seq(abs(i-2), length(subID)*2, 2)
            
            # Fill in statistics for the current condition
            data_statistics[row_indices, "sum_correct"] <- sum_correct[, as.character(i)]
            data_statistics[row_indices, "meanAccuracy"] <- mean_accuracy[, as.character(i)]
            data_statistics[row_indices, "meanRT"] <- mean_rt[, as.character(i)]
            data_statistics[row_indices, "varRT"] <- var_rt[, as.character(i)]
        }
    }
    
    # Return the matrix of summary statistics
    return(data_statistics)
}