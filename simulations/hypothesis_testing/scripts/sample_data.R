# Sample data using simulation settings and true parameter values sampled
sample_data <- function(nPart, parameter_set, nTrialsPerCondition){
        nObs <- nPart*nTrialsPerCondition*2
        data <- matrix(NA,ncol=4,nrow=nObs)
        data[,1] <- rep(1:nPart, each=(nTrialsPerCondition*2))
        data[,2] <- rep(c(1,0),nPart)
        j = 1
        for(i in 1:nPart){      # Get data for every Participant
            for(k in c(1,0)){
                this.cell <- which(data[,1]==i&data[,2]==k)
                accuracy = 0
                temp <- generate_dataset(a = parameter_set$bound[i], v = parameter_set$drift[j], 
                                         t = parameter_set$nondt[i], n = nTrialsPerCondition)
                accuracy = temp$accuracy
                data[this.cell,4] <- accuracy
                data[this.cell,3] <- temp$RT
                j = j+1
            }
        }
        data <- as.matrix(data)
        colnames(data) <- c("sub","cond","rt", "accuracy")    
  return(data)
}