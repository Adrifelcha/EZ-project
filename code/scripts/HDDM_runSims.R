HDDM_runSims <- function(nParticipants, nTrials, nDatasets = 10, priors = NA, modelType = NA,
                         criterion = NA, n.chains = 4, Show=TRUE, forceSim = FALSE, fromPrior=TRUE){
    #################################
    # Initial checks
    #################################
    # Load necessary R libraries
    suppressMessages(library(R2jags))
    suppressMessages(library(rstan))
    # Make sure modelType is valid
    if(is.na(modelType)){    modelType = "hierarchical"
    }else{  valid.models <- c("hierarchical", "metaregression", "ttest") 
            if(!(modelType %in% valid.models)){
               stop("Please specify a valid modelType: 'hierarchical' (default), 'metaregression' 'ttest'")
            }
    }
    # Identify output File
    outputFile <- nameOutput(nTrials, nParticipants, nDatasets, modelType)
    
    # Check if we needToRun simulations again (overruled by 'forceSim')
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
    
    ###################################
    # Run simulation study (if needed)
    ###################################
    if(needToRun){
            ######################
            #      SET UP        #
            ######################
            # ~~~~~~~~~~~~~~~~ Settings
            # Define the design settings according to modelType and (optionally) print to screen
            settings <- list("nPart"= nParticipants, "nTrials"= nTrials,
                             "modelType" = modelType, "nDatasets" = nDatasets)
            # If the model includes an effect (betaweight)
            if(modelType!="hierarchical"){   
                # Make sure we have a valid "criterion" (default to 'drift')
                if(is.na(criterion)){    criterion <- "drift"   }
                X <- 0:settings$nPart   # Default predictor       
                if(modelType=="ttest"){   X <- X %% 2    # Dummy predictor
                                 }else{   X <- X/settings$nPart          }        
                settings <- c(settings, list("X" = X, "criterion" = criterion))
            }else{    X <- NA    }
            if(Show){  show_design(settings)  }
            # Load default priors if needed and add to settings
            if(sum(is.na(priors))>0){    priors <- default_priors(Show, modelType)    }else{
                            if(Show){    show_priors(priors)}                         }
            settings <- c(settings, list("prior" = priors))
            # ~~~~~~~~~~~~~~~~ JAGS variables
            # Define parameters to be tracked on JAGS, according to the modelType
            jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                                "drift_sdev", "nondt_sdev", "bound_sdev", "drift")
            if(modelType!="hierarchical"){  jagsParameters <- c(jagsParameters, "betaweight")  }
            # Write pertinent JAGS model
            if(modelType=="hierarchical"){  modelFile <- "./EZHBDDM.bug"  
            }else{
                  if(criterion=="bound"){ modelFile <- "./EZHBDDM_BetaBound.bug"  }else{
                  if(criterion=="nondt"){ modelFile <- "./EZHBDDM_BetaNondt.bug"  }else{  
                                                   modelFile <- "./EZHBDDM_BetaDrift.bug"  }
            }}
            write_JAGSmodel(priors, modelType, criterion, modelFile)
            # Data to be passed to JAGS
            jagsData = data_toJAGS(modelType)
            # init values
            jagsInits <- default_inits(n.chains, nParticipants)  
            # ~~~~~~~~~~~~~~~~ Storing objects
            # Count number of parameters (i.e. we always assume individual parameters)
            nParams <- (length(jagsParameters)-3) + (nParticipants*3)
            MatEstimates <- matrix(NA, nrow=nDatasets, ncol=nParams)
            MatTrueVal   <- matrix(NA, nrow=nDatasets, ncol=nParams)
            ArrayCredInt <- array(NA, dim=c(nDatasets,nParams,2))
            MatRhats     <- matrix(NA, nrow=nDatasets, ncol=(nParams+1))
            # ~~~~~~~~~~~~~~~~~~ Only Show output for a random iteration
            showChains <- rep(FALSE,nDatasets)
            if(Show){showChains[sample(nDatasets,1)] <- TRUE}
            ######################
            #   Run iterations   #
            ######################
            for(k in 1:nDatasets){
                set.seed(k)
                cat("============>> Dataset", k, "of", nDatasets,"\n")
                design <- HDDM_setup(priors, nPart, nTrials, modelType, X, criterion, fromPrior, Show=FALSE)
                runJags <- HDDM_runJAGS(summaryData = design$sumData, nTrials, X, 
                                        jagsData, jagsParameters, jagsInits, 
                                        n.chains, modelFile, Show = showChains[k])  
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