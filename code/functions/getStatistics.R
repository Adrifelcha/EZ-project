# Get individual statistics from raw data: mean accuracy and mean and variance of correct-RT
getStatistics <- function(data){
  if(is.null(data$accuracy)|is.null(data$rt)){
    error.msg = "Data not available."
    return(print(error.msg))
  }
  subID <- unique(data$sub)
  sum_correct <- tapply(data$accuracy, data$sub, sum)  
  # Remove participants with no correct answer
  always_0 <- which(sum_correct==0)
  if(length(always_0)!=0){
    bad_participants <- (data$sub %in% always_0)
    data <- data[-bad_participants,]
    sum_correct <- tapply(data$accuracy, data$sub, sum) 
  }
  # Get proportion of correct responses
  mean_accuracy <- tapply(data$accuracy, data$sub, mean) 
  mean_rt <- tapply(data$rt, data$sub, mean)
  var_rt  <- tapply(data$rt, data$sub, var)
  # Create a data.frame with just summary statistics
  data_statistics <- cbind(subID, sum_correct, mean_accuracy, mean_rt, var_rt)
  data_statistics <- as.data.frame(data_statistics)
  colnames(data_statistics) = c("sub", "sum_correct","meanAccuracy", "meanRT", "varRT")
  return(data_statistics)
}