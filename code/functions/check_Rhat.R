# A function to check the Rhats for a simulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
check_Rhat <- function(rhats){
  rule <- 1.05
  bad.Rhat <- which(as.numeric(rhats)>rule)
  test.rhat <- length(bad.Rhat) > 0
  if(test.rhat){
    par(mfrow=c(1,1))
    which.are.bad.Rhats <- names(rhats)[bad.Rhat]
    hist(rhats)
    abline(v=rule, col="red", lty=2)
    legend("top",paste("Rhat > ",rule," | ",
                       (round(length(bad.Rhat)/(length(rhats)),5))*100,
                       "% of chains | ", length(bad.Rhat.ID), " chains", sep=""), lty=2, col="red", cex=0.4)
    table(which.are.bad.Rhats)
  }else{
    paste("No Rhat greater than ", rule, sep="")      
  }
}