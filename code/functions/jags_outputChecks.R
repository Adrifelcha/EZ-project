###############################################################################
# Functions to run checks on JAGS output
###############################################################################

# A function to take difference between the true values and estimates retrieved
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
getError <- function(estimates, parameter_set){
  error <- list("bound" = estimates$bound - parameter_set$bound,
                "nondt" = estimates$nondt - parameter_set$nondt,
                "drift" = estimates$drift - parameter_set$drift,
                "bound_mean" = estimates$bound_mean - parameter_set$bound_mean,
                "bound_sdev" = estimates$bound_sdev - parameter_set$bound_sdev,
                "nondt_mean" = estimates$nondt_mean - parameter_set$nondt_mean,
                "nondt_sdev" = estimates$nondt_sdev - parameter_set$nondt_sdev,
                "drift_mean" = estimates$drift_mean - parameter_set$drift_mean,
                "drift_sdev" = estimates$drift_sdev - parameter_set$drift_sdev)
  return(error)
}

# A function to check the Rhats for a simulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
check_Rhat <- function(simOutput){
  rule <- 1.05
  rhats <-simOutput$sim_rhats
  bad.Rhat <- which(rhats>rule,arr.ind = TRUE)
  test.rhat <- length(bad.Rhat) > 0
  if(!test.rhat){
    par(mfrow=c(1,1))
    bad.Rhat.ID <- colnames(rhats)[bad.Rhat[,2]]
    
    hist(rhats[which(rhats>rule)],
         main=paste("Rhats > or = than ", rule))
    abline(v=rule, col="red", lty=2)
    legend("top",paste("Rhat = ",rule," | ",
                       (round(nrow(bad.Rhat)/prod(dim(rhats)),5))*100,
                       "% of chains | ", length(bad.Rhat.ID), " chains", sep=""), lty=2, col="red", cex=0.4)
    table(bad.Rhat.ID)
  }else{
    paste("No Rhat greater than ", rule, sep="")      
  }
}


# A function to plot the merging chains for hierarchical parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
plot.Chain <- function(samples){
  posterior.samples <- samples$BUGSoutput$sims.array
  labels <- names(posterior.samples[1,1,])
  locateHier <- which(grepl("_",labels))
  N <- length(locateHier)
  n.chains <- ncol(posterior.samples[,,labels[locateHier[1]]])
  par(mfrow = c(2, 3), mar =c(5.1,2,4.1,2), bty = "o")
  for(i in locateHier){
    plot(posterior.samples[,1,i], type="l", main=labels[i], 
         xlab="Iteration", ylab="Value sampled")  
    if(n.chains>1){
      for(a in 2:n.chains){
        lines(posterior.samples[,a,i],col=a)
      }
    }
  }
}