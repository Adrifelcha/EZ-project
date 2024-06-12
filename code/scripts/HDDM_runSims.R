HDDM_runSims <- function(nParticipants, nTrials, nDatasets = 10, priors = NA, modelType = NA,
                         criterion = NA, n.chains = 4, Show=TRUE, forceSim = FALSE){
    suppressMessages(library(R2jags))
    suppressMessages(library(rstan))
    if(is.na(modelType)){    modelType = "hierarchical"
    }else{  valid.models <- c("hierarchical", "metaregression", "ttest") 
            if(!(modelType %in% valid.models)){
               stop("Please specify a valid modelType: 'hierarchical' (default), 'metaregression' 'ttest'")
            }
    }
    outputFile <- nameOutput(nTrials, nParticipants, nDatasets, modelType)
    
    if(!forceSim){
          if(file.exists(outputFile)){
              if(sum(is.na(priors))>0){  myPriors <- default_priors(Show = FALSE, modelType)
                                 }else{  myPriors <- priors}
              myNChains <- n.chains
              load(outputFile)                    
              checkPriors <- sum(output$settings$prior != myPriors, na.rm = TRUE)
              checkNChains <- sum(output$n.chains != myNChains, na.rm = TRUE)
              needToRun <- sum(checkPriors,checkNChains)>0
          }else{  needToRun <- TRUE  }
    }else{  needToRun <- TRUE  }
    
    if(needToRun){
            ############################
            #    Variable Set - up
            ############################
            # Load priors
            if(sum(is.na(priors))>0){    priors <- default_priors(Show, modelType)     }
            # Define the design settings according to modelType and (optionally) print to screen
            settings <- list("nPart"= nParticipants, "nTrials"= nTrials, "prior"= priors, 
                             "criterion" = criterion, "modelType" = modelType)
            if(!(modelType=="hierarchical"|is.na(modelType))){   
              X <- 0:(settings$nPart-1)                      # Default predictor       
              if(modelType=="ttest"){   X <- X %% 2    }     # Dummy predictor
              settings <- c(settings, list("X" = X))
            }
            if(Show){  show_design(settings)  }
            # Define parameters to be tracked on JAGS, according to the modelType
            jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                                "drift_sdev", "nondt_sdev", "bound_sdev", "drift")
            if(modelType=="metaregression"){  jagsParameters <- c(jagsParameters, "betaweight")  }
            # Write pertinent JAGS model
            write_JAGSmodel(settings$prior)
            # Data to be passed to JAGS
            jagsData = data_toJAGS(modelType)
            # init values
            jagsInits    <- default_inits(n.chains, nParticipants)  
            # Count number of parameters (i.e. we always assume individual parameters)
            nParams <- (length(jagsParameters)-3) + (nParticipants*3)
            if(!modelType=="hierarchical"){nParams = nParams +1} # Add betaweight
            MatEstimates <- matrix(NA, nrow=nDatasets, ncol=nParams)
            MatTrueVal   <- matrix(NA, nrow=nDatasets, ncol=nParams)
            ArrayCredInt <- array(NA, dim=c(nDatasets,nParams,2))
            MatRhats     <- matrix(NA, nrow=nDatasets, ncol=(nParams+1))
            showChains <- rep(FALSE,nDatasets)
            if(Show){showChains[sample(nDatasets,1)] <- TRUE}
            for(k in 1:nDatasets){
                set.seed(k)
                cat("============>> Dataset", k, "of", nDatasets,"\n")
                design <- HDDM_setup(settings, modelType, Show=FALSE)
                runJags <- HDDM_runJAGS(summaryData = design$sumData, settings, 
                                        jagsData, jagsParameters, jagsInits, 
                                        n.chains, modelType, Show = showChains[k])  
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