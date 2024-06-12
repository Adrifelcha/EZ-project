HDDM_runSims <- function(nParticipants, nTrials, nDatasets = 10, priors = NA, modelType = NA,
                         n.chains = 4, Show=TRUE, forceSim = FALSE){
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
              if(is.na(priors)){  myPriors <- default_priors(Show = FALSE, modelType)
                          }else{  myPriors <- priors}
              myNChains <- n.chains
              load(outputFile)                    
              checkPriors <- sum(output$priors != myPriors, na.rm = TRUE)
              checkNChains <- sum(output$n.chains != myNChains, na.rm = TRUE)
              needToRun <- sum(checkPriors,checkNChains)>0
          }else{  needToRun <- TRUE  }
    }else{  needToRun <- TRUE  }
    
    if(needToRun){
            jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                                "drift_sdev", "nondt_sdev", "bound_sdev", "drift")
            if(modelType=="metaregression"){  jagsParameters <- c(jagsParameters, "betaweight")  }
      
            design <- HDDM_setup(nParticipants, nTrials, nDatasets=1, modelType, priors=NA, Show=FALSE)
            settings <- design$settings
            write_JAGSmodel(myPriors)
            jagsData = data_toJAGS()
            jagsInits    <- default_inits(n.chains, nParticipants)  
            
            nParams <- sum(lengths(design$parameter_set))
            MatEstimates <- matrix(NA, nrow=nDatasets, ncol=nParams)
            MatTrueVal   <- matrix(NA, nrow=nDatasets, ncol=nParams)
            ArrayCredInt <- array(NA, dim=c(nDatasets,nParams,2))
            MatRhats     <- matrix(NA, nrow=nDatasets, ncol=(nParams+1))
            showChains <- rep(FALSE,nDatasets)
            if(Show){showChains[sample(nDatasets,1)] <- TRUE}
            for(k in 1:nDatasets){
                set.seed(k)
                cat("Dataset", k, "of", nDatasets,"\n")
                if(k>2){
                   design <- HDDM_setup(nParticipants,nTrials,nDatasets=1, priors=NA, Show=FALSE)
                }
                runJags <- HDDM_runJAGS(summaryData_page = design$sumData[,,1], settings, 
                                        jagsData, jagsParameters, jagsInits, 
                                        n.chains, Show = showChains[k])  
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
                           "trueValues" = MatTrueVal, "priors" = design$priors, "n.chains" = n.chains)
            save(output, file=outputFile)
            return(output)
    }else{  cat("This simulation had been run before.\nLoading stored results: COMPLETE!")  
            return(output)
    }
}