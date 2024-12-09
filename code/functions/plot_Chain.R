# A function to plot the merging chains for hierarchical parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
plot_Chain <- function(samples, track_allParameters){
  posterior.samples <- samples$BUGSoutput$sims.array
  labels <- names(posterior.samples[1,1,])
  locateHier <- which(grepl("_",labels))
  N <- length(locateHier)
  n.chains <- ncol(posterior.samples[,,labels[locateHier[1]]])
  if(track_allParameters){
           par(mfrow = c(2, 3), mar =c(5.1,2,4.1,2), bty = "o")
  }else{   par(mfrow = c(2, 2), mar =c(1,1,1,1), mai=c(0.5,0.5,0.5,0.2), bty = "o")     }
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