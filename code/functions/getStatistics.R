# Get individual statistics from raw data: mean accuracy and mean and variance of correct-RT
get_Statistics <- function(data){
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
  # Get mean and variance of correct RT
  keep.correct <- which(data$accuracy==1)
  correct_only <- data[keep.correct,]
  mean_rt_correct <- tapply(correct_only$rt, correct_only$sub, mean)
  var_rt_correct  <- tapply(correct_only$rt, correct_only$sub, var)
  # Create a data.frame with just summary statistics
  data_statistics <- cbind(subID, sum_correct, mean_accuracy, mean_rt_correct, var_rt_correct)
  data_statistics <- as.data.frame(data_statistics)
  colnames(data_statistics) = c("sub", "sum_correct","meanAccuracy", "meanRT_correct", "varRT_correct")
  return(data_statistics)
}