store_output <- function(output, settings){
  ########## Load relevant Settings #########################################
  ### R packages
  iterations            <- settings$nDatasets
  possible.combinations <- settings$nCells
  output.folder         <- settings$output.folder
  
  x <- output[,"hierarchical"]
  x[[1]][1,"true.values"]
  out_TrueVals <- array(, dim=c(iterations, ,possible.combinations))
  
  ########## Create empty arrays to save output #############################
  ### Size variables
  # Parameter labels
  par.labels <- c("mu1","mu2", "bound","ndt")
  # No. of parameters
  npar <- length(par.labels)
  # Extensive no. of columns (for True value matrix and Rhats)
  ncols <- npar+1
  # Number of samples kept per chain
  nrows <- n.iter-n.burnin
  ### Array 1: True parameter values used to generate data
  trueValues            <- array(NA, dim=c(possible.combinations,npar))
  colnames(trueValues)  <- par.labels
  ### Array 2: Mean posteriors
  retrievedValues           <- array(NA,dim=c(iterations,npar,possible.combinations))
  colnames(retrievedValues) <- par.labels
  ### Array 3: Standard deviation
  retrievedValues_sd            <- array(NA,dim=c(iterations,npar,possible.combinations))
  colnames(retrievedValues_sd)  <- par.labels
  ### Array 4: MAP
  mapValues           <- array(NA,dim=c(iterations,npar,possible.combinations))
  colnames(mapValues) <- par.labels
  ### Array 5: R hats
  rhats   <- array(NA,dim=c(iterations,ncols,possible.combinations))
  ### Array 6: Seconds elapsed per simulation
  timers  <- array(NA,dim=c(iterations,possible.combinations))
  ### Array 7: Record seeds
  seeds <- array(NA,dim=c(iterations,possible.combinations))
  
  out.size <- possible.combinations * iterations
  for(set in 1:possible.combinations){
    J <- seq(set,out.size,possible.combinations) 
    for(i in 1:iterations){
      j <- J[i]
      S <- output[j,]
      seeds[i,set] <- S$seed
      timers[i,set] <- S$elapsed.time
      rhats[i,,set] <- S$rhats
      trueValues[set,] <- S$true.values
      retrievedValues[i,,set] <- S$mean.estimates
      retrievedValues_sd[i,,set] <- S$std.estimates
      mapValues[i,,set] <-S$map.estimates
    }
  }
  
  save(timers, file = paste(output.folder,studyName,"_timers.RData",sep=""))
  save(seeds, file = paste(output.folder,studyName,"_seeds.RData",sep=""))
  colnames(rhats) <- names(S$rhats)
  save(rhats, file = paste(output.folder,studyName,"Rhats.RData",sep=""))
  save(trueValues, file = paste(output.folder,studyName,"trueValues.RData",sep=""))
  save(retrievedValues, file = paste(output.folder,studyName,"meanPosteriors.RData",sep=""))
  save(retrievedValues_sd, file = paste(output.folder,studyName,"std.RData",sep=""))
  save(mapValues, file = paste(output.folder,studyName,"MAPs.RData",sep=""))
  save(settings, file = paste(output.folder,studyName,"settings.RData",sep=""))
}