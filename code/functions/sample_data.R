# Sample data using simulation settings and true parameter values sampled
sample_data <- function(nPart, nTrials, parameter_set){
  nObs <- nPart*nTrials
  data <- matrix(NA,ncol=3,nrow=nObs)
  data[,1] <- rep(1:nPart, each=nTrials)
  for(i in 1:nPart){      # Get data for every Participant
    this.sub <- which(data[,1]==i)
    accuracy = 0
    while(sum(accuracy)==0){
      temp <- generate_dataset(a = parameter_set$bound[i], v = parameter_set$drift[i], 
                               t = parameter_set$nondt[i], n = nTrials)
      accuracy = temp$accuracy
    }
    data[this.sub,3] <- accuracy
    data[this.sub,2] <- temp$RT
  }
  data <- as.matrix(data)
  colnames(data) <- c("sub", "rt", "accuracy")
  return(data)
}