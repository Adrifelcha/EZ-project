HDDM_simFixedEffect <- function(nParticipants, nTrialsPerCondition, nDatasets = 10, betaweight = 0, 
                                n.chains = 3,  Show=TRUE, forceSim = FALSE, label=NA){
    #################################
    # Initial checks
    #################################
    # Load necessary R libraries
    suppressMessages(library(R2jags))
    suppressMessages(library(rstan))
    # Identify output File
    
    if(!is.na(label)){
          outputFile <- paste("./sim_P",nParticipants,"Tc",nTrialsPerCondition,"D",nDatasets,
                              "_FixedEff_",label,".RData", sep="")
    }else{
          outputFile <- paste("./sim_P",nParticipants,"Tc",nTrialsPerCondition,"D",nDatasets,
                              "_FixedEff.RData", sep="")
    }
    
    # Check if we needToRun simulations again (overruled by 'forceSim')
    if(!forceSim){
          if(file.exists(outputFile)){
              if(sum(is.na(priors))>0){  myPriors <- default_priors(Show = FALSE, modelType = "ttest")
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
            # Define the design settings according to modelType and (optionally) print to screen
            settings <- list("nPart"= nParticipants, "nTrialsPerCondition"= nTrialsPerCondition,
                             "effect" = betaweight, "nDatasets" = nDatasets)
            if(Show){  show_design(settings)  }
            # Load default priors and add to settings
            priors <- default_priors(Show)
            settings <- c(settings, list("prior" = priors))
            # ~~~~~~~~~~~~~~~~ JAGS variables
            # Define parameters to be tracked on JAGS, according to the modelType
            jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                                "drift_sdev", "nondt_sdev", "bound_sdev", "drift", "betaweight")
            # Write pertinent JAGS model
            modelFile <- "./FixedEffect.bug"  
            writefixEffJAGSmodel(priors, modelFile)
            # Data to be passed to JAGS
            X <- rep(c(1,0),nParticipants)
            P <- rep(1:nParticipants, each=2)
            jagsData = list("nParticipants", "nTrialsPerCondition", "X", "P",
                            "meanRT", "varRT", "correct")
            # Init values
            jagsInits <- rep(list(list()), n.chains)
            for(i in 1:n.chains){
              jagsInits[[i]] <- list(drift = matrix(rep(rnorm(nParticipants,0,0.25),2),ncol=2, byrow = FALSE))
            }
            # ~~~~~~~~~~~~~~~~ Storing objects
            # Count number of parameters (i.e. we always assume individual parameters)
            nParams <- (length(jagsParameters)-3) + (nParticipants*4)
            MatEstimates <- matrix(NA, nrow=nDatasets, ncol=nParams)
            MatErrors    <- matrix(NA, nrow=nDatasets, ncol=nParams)
            MatTrueVal   <- matrix(NA, nrow=nDatasets, ncol=nParams)
            ArrayCredInt <- array(NA, dim=c(nDatasets,nParams,2))
            MatRhats     <- matrix(NA, nrow=nDatasets, ncol=(nParams+1))
            betaChain <- array(NA, dim=c(400,4,nDatasets))
            timeCount <- c()
            # ~~~~~~~~~~~~~~~~~~ Only Show output for a random iteration
            showChains <- rep(FALSE,nDatasets)
            if(Show){showChains[sample(nDatasets,1)] <- TRUE}
            #####################
            #   Run datasets    #
            #####################
            for(k in 1:nDatasets){
                set.seed(k)
                cat("============>> Dataset", k, "of", nDatasets,"\n")
                # Sample "true parameters" for the simulation using the priors
                parameter_set <- sample_parameters(priors = priors, nPart = nParticipants, 
                                                   X = X, Show=FALSE, betaweight = betaweight)
                # Generate and prepare data
                rawData = sample_data(nPart = nParticipants, nTrialsPerCondition = nTrialsPerCondition, 
                                      parameter_set = parameter_set)
                summData = getStatistics(rawData)
                correct <- summData[,"sum_correct"]
                varRT   <- summData[,"varRT"]
                meanRT  <- summData[,"meanRT"]
                
                # Run JAGS model and get samples
                tic <- clock::date_now(zone="UTC")
                suppressMessages(samples <- jags(data=jagsData, 
                                                 parameters.to.save=jagsParameters, 
                                                 model=modelFile, 
                                                 n.chains=n.chains, 
                                                 n.iter=500, 
                                                 n.burnin=100, 
                                                 n.thin=1, 
                                                 DIC=T, 
                                                 inits=jagsInits))
                toc <- clock::date_now(zone="UTC")
                if(showChains[k]){  plot_Chain(samples) }
                clock <- as.numeric(toc-tic, units="secs")  # Record time
                timeCount <- c(timeCount, clock)
                object <- samples$BUGSoutput$sims.array
                betaChain[,,k] <- object[,,"betaweight"]
                c <- 0; d <- 0
                for(i in jagsParameters){
                    posteriorParameters <- extractSamples(i, samples)
                    if(length(dim(posteriorParameters))==3){
                        m <- dim(posteriorParameters)[3]
                        MatEstimates[k,(c+1):(c+m)]   <- apply(posteriorParameters,3,mean)
                        MatErrors[k,(c+1):(c+m)]      <- apply(posteriorParameters,3,sd)
                        ArrayCredInt[k,(c+1):(c+m),1] <- apply(posteriorParameters,3, quantile, probs=0.025)
                        ArrayCredInt[k,(c+1):(c+m),2] <- apply(posteriorParameters,3, quantile, probs=0.975)
                    }else{
                        m <- 1
                        MatEstimates[k,c+1]    <- mean(posteriorParameters)
                        MatErrors[k,c+1]       <- sd(posteriorParameters)
                        ArrayCredInt[k,c+1,1:2]   <- quantile(posteriorParameters,probs = c(0.025,0.975))
                    }
                    w <- length(parameter_set[[i]])
                    MatTrueVal[k,(d+1):(d+w)]   <- parameter_set[[i]]
                    c <- c+m; d <- d+w
                }
                MatRhats[k,] <- apply(object,3,Rhat)
            }
            
            # Naming values retrieved
            paramNames <- c()
            for(i in jagsParameters){
                posteriorParameters <- extractSamples(i, samples)
                if(length(dim(posteriorParameters))==3){
                   paramNames <- c(paramNames,dimnames(posteriorParameters)[[3]])
                }else{
                   paramNames <- c(paramNames, i)
                }
            }
            colnames(MatEstimates) <- paramNames
            colnames(MatErrors) <- paramNames
            colnames(ArrayCredInt) <- paramNames
            colnames(MatTrueVal)   <- paramNames
            colnames(MatRhats) <- names(apply(object,3,Rhat))
            
            if(Show){check_Rhat(MatRhats)}
            
            output <- list("rhats"  = MatRhats, "estimates" = MatEstimates, "variance" = MatErrors, 
                           "credIntervals" = ArrayCredInt, "trueValues" = MatTrueVal, "settings" = settings, 
                           "n.chains" = n.chains, "beta_chain" = betaChain, "time_elapsed" = timeCount)
            save(output, file=outputFile)
            return(output)
    }else{  cat("This simulation had been run before.\nLoading stored results: COMPLETE!")  
            return(output)
    }
}