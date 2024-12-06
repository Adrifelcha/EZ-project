# A function to check the Rhats for a simulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
check_Rhat <- function(rhats){
  if("deviance" %in% colnames(rhats)){ rhats <- rhats[,colnames(rhats) != "deviance"]}
  rule <- 1.05
  bad.Rhat <- which(rhats>rule, arr.ind = TRUE)
  test.rhat <- nrow(bad.Rhat) > 0
  if(test.rhat){
          par(mfrow=c(1,1))
          which.are.bad.Rhats <- colnames(rhats)[bad.Rhat[,2]]
          hist(rhats, breaks = 50)
          abline(v=rule, col="red", lty=2)
          legend("top",paste("Rhat > ",rule," | ",
                             (round(nrow(bad.Rhat)/(length(as.vector(rhats))),5))*100,
                             "% of chains | ", length(which.are.bad.Rhats), " chains", sep=""), lty=2, col="red", cex=0.4)
          print(table(which.are.bad.Rhats))
  }else{    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!\n")
            if(ncol(rhats)==3){      cat("R-hat check (Hierarchical mean parameters):\n")  
            }else{                   cat("R-hat check (All parameters):\n")        }
            cat(paste("No Rhat greater than", rule, "\n"))       
            cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!\n")  }
}
