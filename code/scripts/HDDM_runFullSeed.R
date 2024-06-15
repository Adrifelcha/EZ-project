HDDM_runFullSeed <- function(seed, settings){
  set.seed(seed)
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  
  ########## Print a file to indicate the start of a new seed ###############
  write('Seed has been initiated', paste(settings$output.folder,"seed-",seed,"_start.txt",sep=""))
  
  out_H <- list()
  out_Beta <- list()
  cell <- 0
  for(d in settings$design_levels){
      
      for(p in settings$participant_levels){
          X <- cbind(rep(NA,p),(0:(p-1))%%2,(0:(p-1))/p)
          colnames(X) <- settings$design_levels
          
          for(t in settings$trial_levels){
            
              if(d=="hierarchical"){
                design <- HDDM_setup(settings$priors[[d]], p, t, d, X[,d], criterion = NA, settings$fromPrior, Show = FALSE)
                runJags <- HDDM_runJAGS(summaryData = design$sumData, t, X[,d], settings$jagsData[[d]], 
                                        settings$jagsParameters[[d]], settings$jagsInits[[as.character(p)]],  
                                        settings$n.chains, settings$modelFile[d,1], Show = FALSE)
                out_H <- rbind(out_H,list(seed = seed, p = p, t = t, d = d, rhats = runJags$rhats,
                               true.values    = design$parameter_set, mean.estimates = runJags$estimates,
                               std.estimates  = runJags$estimates, elapsed.time   = runJags$clock))
                cell <- 1 + cell
                cat("Iteration", cell, "of",  settings$nCells,"\n")
              }else{
                    for(c in settings$criterion_levels){
                        design <- HDDM_setup(settings$priors[[d]], p, t, d, X[,d], c, settings$fromPrior, Show = FALSE)
                        runJags <- HDDM_runJAGS(summaryData = design$sumData, t, X[,d], settings$jagsData[[d]], 
                                                settings$jagsParameters[[d]], settings$jagsInits[[as.character(p)]],
                                                settings$n.chains, settings$modelFile[d,c], Show = FALSE)
                        out_Beta <- rbind(out_Beta,list(seed = seed, p = p, t = t, d = d, c = c, rhats = runJags$rhats,
                                          true.values    = design$parameter_set, mean.estimates = runJags$estimates,
                                          std.estimates  = runJags$estimates, elapsed.time   = runJags$clock))
                        cell <- 1 + cell
                        cat("Iteration", cell, "of",  settings$nCells,"\n")
                    }
              }
          }
      }
  }
  ########## Print a file to indicate the end of a the seed ###############
  write('Seed has ended running', paste(settings$output.folder,"seed-",seed,"_end.txt",sep=""))
  save(output, file=paste(settings$output.folder,"seed-",seed,".RData",sep=""))  
  return(output)
}
  
  
#   ###################################
#   # Run simulation study (if needed)
#   ###################################
#   if(needToRun){
#           # ~~~~~~~~~~~~~~~~ Storing objects
#           # Count number of parameters (i.e. we always assume individual parameters)
#           nParams <- (length(settings$jagsParameters)-3) + (nParticipants*3)
#           MatEstimates <- matrix(NA, nrow=nDatasets, ncol=nParams)
#           MatTrueVal   <- matrix(NA, nrow=nDatasets, ncol=nParams)
#           ArrayCredInt <- array(NA, dim=c(nDatasets,nParams,2))
#           MatRhats     <- matrix(NA, nrow=nDatasets, ncol=(nParams+1))
#           ######################
#           #   Run iterations   #
#           ######################
#           for(k in 1:nDatasets){
#              
#               MatRhats[k,] <- runJags$rhats
#               c <- 0; d <- 0
#               for(j in 1:length(runJags$estimates)){
#                  m <- length(runJags$estimates[[j]])
#                  w <- length(design$parameter_set[[j]])
#                  MatEstimates[k,(c+1):(c+m)] <- runJags$estimates[[j]]
#                  MatTrueVal[k,(d+1):(d+w)]   <- design$parameter_set[[j]]
#                  if(is.vector(runJags$credInterval[[j]])){
#                        ArrayCredInt[k,(c+1):(c+m),1] <- runJags$credInterval[[j]][1]
#                        ArrayCredInt[k,(c+1):(c+m),2] <- runJags$credInterval[[j]][2]
#                  }else{
#                        ArrayCredInt[k,(c+1):(c+m),1] <- runJags$credInterval[[j]][1,]
#                        ArrayCredInt[k,(c+1):(c+m),2] <- runJags$credInterval[[j]][2,]
#                  }
#                  c <- c+m; d <- d+w
#               }
#           }
#           
#           paramNames <- NA
#           paramNames2 <- NA
#           for(j in 1:length(runJags$estimates)){
#                 if(is.vector(runJags$credInterval[[j]])){
#                    paramNames <- c(paramNames, names(runJags$credInterval[j]))
#                 }else{
#                    paramNames <- c(paramNames, colnames(runJags$credInterval[[j]]))
#                 }
#                 if(length(design$parameter_set[[j]])==1){
#                   paramNames2 <- c(paramNames2, names(design$parameter_set[j]))
#                 }else{
#                   labels <- paste(names(design$parameter_set[j]), "[",1:length(design$parameter_set[[j]]),"]",sep="")
#                   paramNames2 <- c(paramNames2, labels)
#                 }
#           }
#           paramNames <- paramNames[-1]
#           paramNames2 <- paramNames2[-1]
#           colnames(MatEstimates) <- paramNames
#           colnames(ArrayCredInt) <- paramNames
#           colnames(MatTrueVal)   <- paramNames2
#           colnames(MatRhats) <- names(runJags$rhats)
#           
#           if(Show){check_Rhat(MatRhats)}
#           
#           output <- list("rhats"  = MatRhats, "estimates" = MatEstimates, "credIntervals" = ArrayCredInt,
#                          "trueValues" = MatTrueVal, "settings" = settings, "settings$n.chains" = settings$n.chains)
#           save(output, file=outputFile)
#           return(output)
#   }else{  cat("This simulation had been run before.\nLoading stored results: COMPLETE!")  
#           return(output)
#   }
# }