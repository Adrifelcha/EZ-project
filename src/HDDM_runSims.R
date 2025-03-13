HDDM_runSims <- function(nParticipants, nTrials, nDatasets = 10, priors = NA, modelType = NA, criterion = NA, n.chains = 3, 
                         n.burnin=250, n.iter=2000, n.thin=1, Show=TRUE, forceSim = FALSE, fromPrior=TRUE, output.folder = "./",
                         track_allParameters = FALSE, rhatCheck=TRUE, redo_if_bad_rhat=FALSE){
    grand_tic <- clock::date_now(zone="UTC")
    #################################
    # Initial checks
    #################################
    # Load necessary R libraries
    suppressMessages(library(R2jags))
    # Make sure modelType is valid
    if(is.na(modelType)){    modelType = "hierarchical"
    }else{  valid.models <- c("hierarchical", "metaregression", "ttest") 
            if(!(modelType %in% valid.models)){
               stop("Please specify a valid modelType: 'hierarchical' (default), 'metaregression' 'ttest'")
            }
    }
    # Identify output File
    outputFile <- nameOutput(nTrials, nParticipants, nDatasets, modelType, fromPrior, output.folder = output.folder)
    
    
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
            settings <- list("nPart"= nParticipants, "nTrials"= nTrials,
                             "modelType" = modelType, "nDatasets" = nDatasets)
            # If the model includes an effect (betaweight)
            if(modelType!="hierarchical"){   
                # Make sure we have a valid "criterion" (default to 'drift')
                if(is.na(criterion)){    criterion <- "drift"   }
                X <- 0:nParticipants   # Default predictor       
                if(modelType=="ttest"){   X <- X %% 2    # Dummy predictor
                                 }else{   X <- X/nParticipants          }        
                settings <- c(settings, list("X" = X, "criterion" = criterion))
            }else{    X <- NA    }
            if(Show){  show_design(settings)  }
            # Load default priors if needed and add to settings
            if(sum(is.na(priors))>0){    priors <- default_priors(Show, modelType)    }else{
                            if(Show){    show_priors(priors)}                         }
            settings <- c(settings, list("prior" = priors))
            # ~~~~~~~~~~~~~~~~ JAGS variables
            # Define parameters to be tracked on JAGS, according to the modelType
            jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean")
            if(track_allParameters){jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                                                        "drift_sdev", "nondt_sdev", "bound_sdev", "drift")
            }else{                  jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean")                    }
            if(modelType!="hierarchical"){  jagsParameters <- c(jagsParameters, "betaweight")  }
            # Write pertinent JAGS model
            
            name_start <- paste(output.folder, "EZHBDDM", sep="")
            if(modelType=="hierarchical"){  modelFile <- paste(name_start, ".bug", sep="")
            }else{
                  if(criterion=="bound"){ modelFile <- paste(name_start, "_BetaBound.bug", sep="")  }else{
                  if(criterion=="nondt"){ modelFile <- paste(name_start, "_BetaNondt.bug", sep="")  }else{  
                                          modelFile <- paste(name_start, "_BetaDrift.bug", sep="")  }
            }}
            write_JAGSmodel(priors, modelType, criterion, modelFile)
            # Data to be passed to JAGS
            jagsData = data_toJAGS(modelType)
            # init values
            jagsInits <- default_inits(n.chains, nParticipants)  
            # ~~~~~~~~~~~~~~~~ Storing objects
            # Count number of parameters
            if(track_allParameters){  nParams <- (length(jagsParameters)-3) + (nParticipants*3)    
            }else{                    nParams <- (length(jagsParameters))                           }
            MatEstimates <- matrix(NA, nrow=nDatasets, ncol=nParams)
            MatTrueVal   <- matrix(NA, nrow=nDatasets, ncol=nParams)
            ArrayCredInt <- array(NA, dim=c(nDatasets,nParams,2))
            MatRhats     <- matrix(NA, nrow=nDatasets, ncol=(nParams+1))
            # ~~~~~~~~~~~~~~~~~~ Only Show output for a random iteration
            showChains <- rep(FALSE,nDatasets)
            seed_id <- rep(NA,nDatasets)
            if(Show){showChains[sample(nDatasets,1)] <- TRUE}
            ######################
            #   Run iterations   #
            ######################
            repetition_counts <- 0
            for(k in 1:nDatasets){
                rhat_not_verified <- TRUE
                seed <- k
                cat("============>> Dataset", k, "of", nDatasets,"\n")
                while(rhat_not_verified){
                      set.seed(seed)
                      design <- HDDM_setup(priors = priors, nPart = nParticipants, nTrials = nTrials, modelType = modelType, 
                                           X = X, criterion = criterion, fromPrior = fromPrior, Show=FALSE)
                      if(k>100){
                                runJags <- try(HDDM_runJAGS(summaryData = design$sumData, nTrials = nTrials, X = X, 
                                                        jagsData = jagsData, jagsParameters = jagsParameters, jagsInits = jagsInits, 
                                                        n.chains = n.chains, modelFile = modelFile, Show = showChains[k],
                                                        track_allParameters = track_allParameters))
                                while(inherits(runJags, "try-error")){
                                  repetition_counts <- repetition_counts+1
                                  seed <- seed+10000
                                  set.seed(seed)
                                  cat("============>> Dataset", k, "of", nDatasets,"+",repetition_counts,"\n")
                                  design <- HDDM_setup(priors = priors, nPart = nParticipants, nTrials = nTrials, modelType = modelType, 
                                                       X = X, criterion = criterion, fromPrior = fromPrior, Show=FALSE)
                                  runJags <- try(HDDM_runJAGS(summaryData = design$sumData, nTrials = nTrials, X = X, 
                                                              jagsData = jagsData, jagsParameters = jagsParameters, jagsInits = jagsInits, 
                                                              n.chains = n.chains, modelFile = modelFile, Show = showChains[k],
                                                              track_allParameters = track_allParameters))
                                }
                      }else{
                                  runJags <- HDDM_runJAGS(summaryData = design$sumData, nTrials = nTrials, X = X, 
                                                          jagsData = jagsData, jagsParameters = jagsParameters, jagsInits = jagsInits, 
                                                          n.chains = n.chains, modelFile = modelFile, Show = showChains[k],
                                                          track_allParameters = track_allParameters)
                      }
                    
                      count_bad_rhats <- sum(runJags$rhats[jagsParameters]>1.05)
                      if((!redo_if_bad_rhat)|(count_bad_rhats==0)){ rhat_not_verified <-  FALSE}
                      seed <- seed+10000
                }
                MatRhats[k,] <- runJags$rhats
                c <- 0; d <- 0
                for(j in 1:length(runJags$estimates)){
                   this <-  names(runJags$estimates[j])
                   m <- length(runJags$estimates[[this]])
                   w <- length(design$parameter_set[[this]])
                   MatEstimates[k,(c+1):(c+m)] <- runJags$estimates[[this]]
                   MatTrueVal[k,(d+1):(d+w)]   <- design$parameter_set[[this]]
                   if(is.vector(runJags$credInterval[[j]])){
                         ArrayCredInt[k,(c+1):(c+m),1] <- runJags$credInterval[[this]][1]
                         ArrayCredInt[k,(c+1):(c+m),2] <- runJags$credInterval[[this]][2]
                   }else{
                         ArrayCredInt[k,(c+1):(c+m),1] <- runJags$credInterval[[this]][1,]
                         ArrayCredInt[k,(c+1):(c+m),2] <- runJags$credInterval[[this]][2,]
                   }
                   c <- c+m; d <- d+w
                }
                
                seed_id[k] <- seed
            }
            
            paramNames <- NA
            paramNames2 <- NA
            for(j in 1:length(runJags$estimates)){
                  this <-  names(runJags$estimates[j])
                  if(is.vector(runJags$credInterval[[this]])){
                     paramNames <- c(paramNames, names(runJags$credInterval[this]))
                  }else{
                     paramNames <- c(paramNames, colnames(runJags$credInterval[[this]]))
                  }
                  if(length(design$parameter_set[[this]])==1){
                    paramNames2 <- c(paramNames2, names(design$parameter_set[this]))
                  }else{
                    labels <- paste(names(design$parameter_set[this]), "[",1:length(design$parameter_set[[this]]),"]",sep="")
                    paramNames2 <- c(paramNames2, labels)
                  }
            }
            paramNames <- paramNames[-1]
            paramNames2 <- paramNames2[-1]
            colnames(MatEstimates) <- paramNames
            colnames(ArrayCredInt) <- paramNames
            colnames(MatTrueVal)   <- paramNames2
            colnames(MatRhats) <- names(runJags$rhats)
            
            if(Show|rhatCheck){check_Rhat(MatRhats)}
            
            grand_toc <- clock::date_now(zone="UTC")
            total_time <- difftime(grand_toc, grand_tic, units="mins")
            output <- list("rhats"  = MatRhats, "estimates" = MatEstimates, "credIntervals" = ArrayCredInt,
                           "trueValues" = MatTrueVal, "settings" = settings, "n.chains" = n.chains,
                           "totalTime" = total_time, "seed_id" = seed_id)
            save(output, file=outputFile)
            
            cat("Running this simulation study took ", total_time, "minutes.\n")
    }else{  cat("This simulation had been run before.\nLoading stored results: COMPLETE!\n",
                "Running this simulation study took ", output$totalTime, "minutes.\n")
            if(Show|rhatCheck){   check_Rhat(output$rhats)      }
    }
    return(output)
}