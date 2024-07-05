# Get individual statistics from raw data: mean accuracy and mean and variance of correct-RT
getStatistics <- function(data){
    if(is.null(data[,"accuracy"])|is.null(data[,"rt"])){
      stop("Data not available.")
    }
    if(ncol(data)==3){
        subID <- unique(data[,"sub"])
        sum_correct <- tapply(data[,"accuracy"], data[,"sub"], sum)  
        # Remove participants with no correct answer
        always_0 <- which(sum_correct==0)
        if(length(always_0)!=0){
          bad_participants <- (data[,"sub"] %in% always_0)
          data <- data[-bad_participants,]
          sum_correct <- tapply(data[,"accuracy"], data[,"sub"], sum) 
        }
        # Get proportion of correct responses
        mean_accuracy <- tapply(data[,"accuracy"], data[,"sub"], mean) 
        mean_rt <- tapply(data[,"rt"], data[,"sub"], mean)
        var_rt  <- tapply(data[,"rt"], data[,"sub"], var)
        # Create a data.frame with just summary statistics
        data_statistics <- cbind(subID, sum_correct, mean_accuracy, mean_rt, var_rt)
        data_statistics <- as.matrix(data_statistics)
        colnames(data_statistics) = c("sub", "sum_correct","meanAccuracy", "meanRT", "varRT")
    }else{
        # Get proportion of correct responses
        sum_correct <- tapply(data[,"accuracy"], list(data[,"sub"], data[,"cond"]), sum)
        mean_accuracy <- tapply(data[,"accuracy"], list(data[,"sub"], data[,"cond"]), mean) 
        # Get RT indices
        mean_rt <- tapply(data[,"rt"], list(data[,"sub"], data[,"cond"]), mean)
        var_rt  <- tapply(data[,"rt"], list(data[,"sub"], data[,"cond"]), var)
        subID <- unique(data[,"sub"])
        condID <- unique(data[,"cond"])
        sub <- rep(subID, each=2)
        cond <- rep(condID, length(subID))
        data_statistics <- matrix(NA, ncol=6, nrow=length(cond))
        colnames(data_statistics) = c("sub", "cond", "sum_correct","meanAccuracy", "meanRT", "varRT")
        data_statistics[,"sub"] <- sub;  data_statistics[,"cond"] <- cond
        for(i in condID){
            data_statistics[seq(abs(i-2),length(subID)*2,2),"sum_correct"] <- sum_correct[,as.character(i)]
            data_statistics[seq(abs(i-2),length(subID)*2,2),"meanAccuracy"] <- mean_accuracy[,as.character(i)]
            data_statistics[seq(abs(i-2),length(subID)*2,2),"meanRT"] <- mean_rt[,as.character(i)]
            data_statistics[seq(abs(i-2),length(subID)*2,2),"varRT"] <- var_rt[,as.character(i)]
        }
    }
    return(data_statistics)
}