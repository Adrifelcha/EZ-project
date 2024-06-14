HDDM_runFullSeed <- function(seed, participant_levels, trial_levels, criterion_levels = c("drift", "nondt", "bound"), 
                          design_levels = c("hierarchical","ttest","metaregression"), 
                          priors = NA, n.chains = 2, Show=TRUE, forceSim = FALSE, fromPrior=TRUE){
  # Load necessary R libraries
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  # Make sure inputs are valid
  valid.models <- c("hierarchical", "metaregression", "ttest")
  if(sum(!(design_levels %in% valid.models))>0){
     stop("Only valid design levels are: 'hierarchical', 'metaregression' and 'ttest'")
  }
  valid.criteria <- c("drift", "nondt", "bound")
  if(sum(!(criterion_levels %in% valid.criteria))>0){
     stop("Only valid criterions are: 'drift', 'bound' and 'nondt'")
  }  
  
  jagsInits <- default_inits(n.chains, nParticipants)
  
  jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                      "drift_sdev", "nondt_sdev", "bound_sdev", "drift")
  jagsParameters <- list(jagsParameters, c(jagsParameters, "betaweight"), c(jagsParameters, "betaweight"))
  names(jagsParameters) <- valid.models
  
  jagsData = list(data_toJAGS("hierarchical"), data_toJAGS("metaregression"), data_toJAGS("ttest"))
  names(jagsData) <- valid.models
  
  priors <- list(default_priors(Show=FALSE, "hierarchical"), default_priors(Show=FALSE, "metaregression"),
                 default_priors(Show=FALSE, "ttest"))
  names(priors) <- valid.models
  
  modelFile <- matrix(c(rep("./EZHBDDM.bug" ,3), rep(c("./EZHBDDM_BetaDrift.bug",
                      "./EZHBDDM_BetaNondt.bug","./EZHBDDM_BetaBound.bug"),2)), byrow = TRUE, ncol = 3)
  colnames(modelFile) <- valid.criteria
  rownames(modelFile) <- valid.models
  
  for(model in valid.models){
      for(crit in valid.criteria){
          write_JAGSmodel(priors[[3]], model, crit, modelFile[model,crit])
      }
  }
  
  for(p in participant_levels){
      X <- cbind(rep(NA,p),(0:(p-1))/p,(0:(p-1))%%2)
      colnames(X) <- valid.models
    
      for(d in design_levels){
          
          for(t in trial_levels){
            
              if(d=="hierarchical"){
                design <- HDDM_setup(priors[[d]], p, t, d, X[,d], criterion = NA, fromPrior, Show = FALSE)
                runJags <- HDDM_runJAGS(summaryData = design$sumData, t, X[,d], jagsData[[d]], 
                                        jagsParameters[[d]], jagsInits,  n.chains, modelFile[d,1], Show = FALSE)  
              }else{
                    for(c in criterion_levels){
                        design <- HDDM_setup(priors[[d]], p, t, d, X[,d], c, fromPrior, Show = FALSE)
                        runJags <- HDDM_runJAGS(summaryData = design$sumData, t, X[,d], jagsData[[d]], 
                                                jagsParameters[[d]], jagsInits,  n.chains, modelFile[d,c], Show = FALSE)  
                    }
              }
          }
      }
  }
    save(output, file="seed1.RData")  
}
  
  
  ###################################
  # Run simulation study (if needed)
  ###################################
  if(needToRun){
          # ~~~~~~~~~~~~~~~~ Storing objects
          # Count number of parameters (i.e. we always assume individual parameters)
          nParams <- (length(jagsParameters)-3) + (nParticipants*3)
          MatEstimates <- matrix(NA, nrow=nDatasets, ncol=nParams)
          MatTrueVal   <- matrix(NA, nrow=nDatasets, ncol=nParams)
          ArrayCredInt <- array(NA, dim=c(nDatasets,nParams,2))
          MatRhats     <- matrix(NA, nrow=nDatasets, ncol=(nParams+1))
          ######################
          #   Run iterations   #
          ######################
          for(k in 1:nDatasets){
             
              MatRhats[k,] <- runJags$rhats
              c <- 0; d <- 0
              for(j in 1:length(runJags$estimates)){
                 m <- length(runJags$estimates[[j]])
                 w <- length(design$parameter_set[[j]])
                 MatEstimates[k,(c+1):(c+m)] <- runJags$estimates[[j]]
                 MatTrueVal[k,(d+1):(d+w)]   <- design$parameter_set[[j]]
                 if(is.vector(runJags$credInterval[[j]])){
                       ArrayCredInt[k,(c+1):(c+m),1] <- runJags$credInterval[[j]][1]
                       ArrayCredInt[k,(c+1):(c+m),2] <- runJags$credInterval[[j]][2]
                 }else{
                       ArrayCredInt[k,(c+1):(c+m),1] <- runJags$credInterval[[j]][1,]
                       ArrayCredInt[k,(c+1):(c+m),2] <- runJags$credInterval[[j]][2,]
                 }
                 c <- c+m; d <- d+w
              }
          }
          
          paramNames <- NA
          paramNames2 <- NA
          for(j in 1:length(runJags$estimates)){
                if(is.vector(runJags$credInterval[[j]])){
                   paramNames <- c(paramNames, names(runJags$credInterval[j]))
                }else{
                   paramNames <- c(paramNames, colnames(runJags$credInterval[[j]]))
                }
                if(length(design$parameter_set[[j]])==1){
                  paramNames2 <- c(paramNames2, names(design$parameter_set[j]))
                }else{
                  labels <- paste(names(design$parameter_set[j]), "[",1:length(design$parameter_set[[j]]),"]",sep="")
                  paramNames2 <- c(paramNames2, labels)
                }
          }
          paramNames <- paramNames[-1]
          paramNames2 <- paramNames2[-1]
          colnames(MatEstimates) <- paramNames
          colnames(ArrayCredInt) <- paramNames
          colnames(MatTrueVal)   <- paramNames2
          colnames(MatRhats) <- names(runJags$rhats)
          
          if(Show){check_Rhat(MatRhats)}
          
          output <- list("rhats"  = MatRhats, "estimates" = MatEstimates, "credIntervals" = ArrayCredInt,
                         "trueValues" = MatTrueVal, "settings" = settings, "n.chains" = n.chains)
          save(output, file=outputFile)
          return(output)
  }else{  cat("This simulation had been run before.\nLoading stored results: COMPLETE!")  
          return(output)
  }
}