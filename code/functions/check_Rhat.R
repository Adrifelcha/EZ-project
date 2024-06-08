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