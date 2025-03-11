store_parallelOutput <- function(output, settings, saveTo = "./"){
  #################################################################################
  # Identify relevant properties of the simulation study
  #################################################################################
   nDatasets      <- settings$nDatasets
   nCells         <- settings$nCells
   output.folder  <- settings$output.folder
   nP   <- settings$nParticipants
   nT   <- settings$nTrialsPerCondition
   beta_levels  <- settings$beta_levels
   nchain <- settings$n.chains
   #out <- rbind(output[,"noDiff"],output[,"Diff"])
   B.files <- paste("B", c("",1:(length(settings$beta_levels)-1)), sep="")
   nParams <- 7+(4*nParticipants)
   
   i <- 1
   for(b in beta_levels){
       if(b == 0){ out = output[,"noDiff"]  }else{  out = output[,"Diff"]  }
       clock <- rep(NA, nDatasets) 
       clock2 <- rep(NA, nDatasets) 
       # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # Fill the empty lists created above with empty arrays to store output
       #################################################################################################
       # Identify parameter names
       names_true      <- sub('.*true.values.','',names(unlist(out[[1]][which(out[[1]][,"beta"]==b)[1],"true.values"])))
       names_estimates <- sub(".*\\.", "", sub('.*mean.estimates.','',names(unlist(out[[1]][which(out[[1]][,"beta"]==b)[1],"mean.estimates"]))))
       names_rhats     <- sub(".*\\.", "", names(unlist(out[[1]][which(out[[1]][,"beta"]==b)[1],"rhats"])))
       # Create empty arrays to store True values
       betaChains <- array(NA, dim=c(nDatasets,nParams,nc), 
                           dimnames = list(paste("seed", 1:nDatasets), names_true))
       trueVals <- array(NA, dim=c(nDatasets,nParams), 
                               dimnames = list(paste("seed", 1:nDatasets), names_true))
       # Create empty arrays for estimates and errors
       meanPosts <- array(NA, dim=c(nDatasets,nParams), 
                           dimnames = list(paste("seed", 1:nDatasets), names_estimates))
       sdevPosts <- array(NA, dim=c(nDatasets,nParams), 
                          dimnames = list(paste("seed", 1:nDatasets), names_estimates))
       # Create empty arrays for Rhats
       rhats <- array(NA, dim=c(nDatasets,nParams+1), 
                             dimnames = list(paste("seed", 1:nDatasets), names_rhats))
       
       # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # Fill the arrays corresponding to each level 'p'
       #################################################################################################
       for(k in 1:nDatasets){
           at <- paste("result.",k,sep="")
           thisH <- as.numeric(which(out[[at]][,"beta"]==b))
           trueVals[k,] <- unlist(out[[at]][thisH,"true.values"])
           meanPosts[k,] <- unlist(out[[at]][thisH,"mean.estimates"])
           sdevPosts[k,] <- unlist(out[[at]][thisH,"std.estimates"])
           rhats[k,] <- unlist(out[[at]][thisH,"rhats"])
           clock[k] <- as.numeric(out[[at]][thisH,"jags.time"])
           clock2[k] <- as.numeric(out[[at]][thisH,"total.time"])
       }
    
       simStudy_Beta <- list("true" = trueVals,
                                     "recovered" = meanPosts,
                                     "estimates_sdev" = sdevPosts,
                                     "rhats" = sdevPosts,
                             "jagsTime" = clock, "totalTime" = clock2,
                             "settings" = settings)
       outputFile = paste(saveTo,"simStudy_",B.files[i],".RData", sep="")
       save(simStudy_Beta, file=outputFile)
    i <- i +1      
   }
} 