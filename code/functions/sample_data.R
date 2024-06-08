# Sample data using simulation settings and true parameter values sampled
sample_data <- function(settings, parameter_set){
  nObs <- settings$nPart*settings$nTrials
  data <- matrix(NA,ncol=3,nrow=nObs)
  data[,1] <- rep(1:settings$nPart, each=settings$nTrials)
  for(i in 1:settings$nP){      # Get data for every Participant
    this.sub <- which(data[,1]==i)
    accuracy = 0
    while(sum(accuracy)==0){
      temp <- generate_dataset(a = parameter_set$bound[i], v = parameter_set$drift[i], 
                               t = parameter_set$nondt[i], n = settings$nTrials)
      accuracy = temp$accuracy
    }
    data[this.sub,3] <- accuracy
    data[this.sub,2] <- temp$RT
  }
  data <- as.data.frame(data)
  colnames(data) <- c("sub", "rt", "accuracy")
  return(data)
}