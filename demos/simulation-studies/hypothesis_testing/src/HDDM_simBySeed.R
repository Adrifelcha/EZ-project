HDDM_simBySeed_withinSubject <- function(seed, settings, forceRun=FALSE, redo_if_bad_rhat=TRUE, rhat_cutoff=1.05){
  #################################################################################
  # This function runs a single simulation for a hierarchical drift diffusion model
  # with fixed effects for a given random seed
  #
  # Parameters:
  #   seed: Random seed for reproducibility
  #   settings: List containing simulation parameters
  #   forceRun: Whether to run even if results for this seed already exist
  #   redo_if_bad_rhat: Whether to repeat simulations with poor MCMC convergence
  #   rhat_cutoff: Threshold for acceptable Rhat values (convergence diagnostic)
  #################################################################################
  
  # Set random seed and load required packages
  set.seed(seed)
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  
  # Check if this seed has already been run
  fileName <- paste(settings$output.folder, "seed-", seed, ".RData", sep="")
  if(file.exists(fileName) & !forceRun){
    stop("Seed already run")
  }
  
  # Create a file to indicate the start of processing this seed
  write('Seed has been initiated', paste(settings$output.folder, "seed-", seed, "_start.txt", sep=""))
  
  # Initialize output lists for null effect (no difference) and non-null effect conditions
  out_NoDiff <- list()
  out_Diff <- list()
  
  # Initialize counters
  cell <- 0  # Counter for beta levels processed
  redo_JAGS <- 0  # Counter for JAGS failures
  redo_Rhat <- 0  # Counter for Rhat failures
  
  # Extract simulation settings
  nParticipants <- settings$nParticipants
  nTrialsPerCondition <- settings$nTrialsPerCondition
  X <- settings$X  # Design matrix for conditions
  P <- settings$P  # Participant IDs
  
  # Loop through each beta level (effect size)
  for(b in settings$beta_levels){
        # Record start time for this beta level
        tic0 <- clock::date_now(zone="UTC")
        rhat_not_verified <- TRUE  # Flag to control MCMC convergence checking
        this.seed <- seed  # Initialize seed for this beta level
        
        # Keep trying until we get acceptable MCMC convergence
        while(rhat_not_verified){
            # Set seed for this attempt
            set.seed(this.seed)
            
            # Sample parameters from prior distributions
            # betaweight is the effect size parameter we're manipulating
            parameter_set <- sample_parameters(priors = settings$priors, 
                                              nPart = nParticipants, 
                                              X = X, 
                                              Show = FALSE, 
                                              fixedBeta = b,
                                              withinSubject = TRUE)
            
            # Generate simulated data based on these parameters
            rawData = sample_data(nPart = nParticipants, 
                                 nTrialsPerCondition = nTrialsPerCondition, 
                                 parameter_set = parameter_set)
            
            # Calculate summary statistics from raw data
            summData = getStatistics(rawData)
            correct <- summData[,"sum_correct"]  # Accuracy
            varRT   <- summData[,"varRT"]        # Response time variance
            meanRT  <- summData[,"meanRT"]       # Mean response time
            
            # Run JAGS model to estimate parameters
            tic <- clock::date_now(zone="UTC")
            z <- try(samples <- jags(data = settings$jagsData, 
                                   parameters.to.save = settings$jagsParameters, 
                                   model = settings$modelFile, 
                                   n.chains = settings$n.chains, 
                                   n.iter = settings$n.iter, 
                                   n.burnin = settings$n.burnin, 
                                   n.thin = settings$n.thin, 
                                   DIC = T,
                                   inits = settings$jagsInits))
            
            # Check if JAGS encountered an error
            if(inherits(z, "try-error")){ 
                cat("Repeating beta level", b, "due to a JAGS error \n")
                this.seed <- this.seed + 0.01  # Slightly modify seed for next attempt
                redo_JAGS <- redo_JAGS + 1
                if(redo_JAGS > 5){ 
                    warning(paste("Failed to converge after", redo_JAGS, "attempts for beta =", b))
                    break 
                }
                next  # Try again with new seed
            }
            
            # Record JAGS runtime
            toc <- clock::date_now(zone="UTC")
            clock <- toc - tic
            
            # Extract MCMC samples
            object <- samples$BUGSoutput$sims.array
            
            # Calculate Rhat values to check MCMC convergence
            rhats <- apply(object, 3, Rhat)
            count_bad_rhats <- sum(rhats > rhat_cutoff)
            
            # Check if Rhats are acceptable
            if((!redo_if_bad_rhat) || (count_bad_rhats == 0)){ 
                rhat_not_verified <- FALSE  # Convergence is good, proceed
            } else {
                cat("Repeating beta level", b, "due to bad Rhats \n")
                this.seed <- this.seed + 0.01  # Slightly modify seed for next attempt
                redo_Rhat <- redo_Rhat + 1
                next  # Try again with new seed
            }
            
            # Initialize lists for parameter estimates
            estimates <- list()
            errors <- list()
            CI_low <- list()
            CI_up <- list()
            trueVal <- list()
            
            # Extract parameter estimates from MCMC samples
            for(i in settings$jagsParameters){
                # Get posterior samples for this parameter
                posteriorParameters <- JAGS_extractSamples(i, samples)
                
                # Handle multi-dimensional parameters (e.g., participant-specific parameters)
                if(length(dim(posteriorParameters)) == 3){
                    meanPost <- apply(posteriorParameters, 3, mean)
                    sdPost <- apply(posteriorParameters, 3, sd)
                    percentiles <- apply(posteriorParameters, 3, quantile, probs=c(0.025, 0.975))
                    
                    # Store as lists to preserve names
                    estimates <- c(estimates, list(meanPost))
                    errors <- c(errors, list(sdPost))
                    CI_low <- c(CI_low, list(percentiles[1,]))
                    CI_up <- c(CI_up, list(percentiles[2,]))
                } else {
                    # Handle scalar parameters
                    meanPost <- mean(posteriorParameters)
                    sdPost <- sd(posteriorParameters)
                    percentiles <- quantile(posteriorParameters, probs=c(0.025, 0.975))
                    
                    # Store as lists to preserve names
                    estimates <- c(estimates, list(meanPost))
                    errors <- c(errors, list(sdPost))
                    CI_low <- c(CI_low, list(percentiles[1]))
                    CI_up <- c(CI_up, list(percentiles[2]))
                }
                
                # Store true parameter values
                trueVal <- c(trueVal, list(parameter_set[[i]]))
            }
            
            # Assign names to the lists
            names(estimates) <- settings$jagsParameters
            names(errors) <- settings$jagsParameters
            names(CI_low) <- settings$jagsParameters
            names(CI_up) <- settings$jagsParameters
            names(trueVal) <- settings$jagsParameters
            
            # Record total runtime for this beta level
            toc0 <- clock::date_now(zone="UTC")
            
            # Store results based on whether this is a null effect (beta=0) or not
            if(b == 0){
                # For null effect condition
                out_NoDiff <- rbind(out_NoDiff, list(
                    seed = this.seed,
                    beta = b,
                    rhats = rhats,
                    true.values = trueVal,
                    mean.estimates = estimates,
                    std.estimates = errors,
                    CI_low = CI_low,
                    CI_up = CI_up,
                    jags.time = clock,
                    total.time = toc0 - tic0,
                    beta_chains = JAGS_extractSamples("betaweight", samples)
                ))
            } else {
                # For non-null effect conditions
                out_Diff <- rbind(out_Diff, list(
                    seed = this.seed,
                    beta = b,
                    rhats = rhats,
                    true.values = trueVal,
                    mean.estimates = estimates,
                    std.estimates = errors,
                    CI_low = CI_low,
                    CI_up = CI_up,
                    jags.time = clock,
                    total.time = toc0 - tic0,
                    beta_chains = JAGS_extractSamples("betaweight", samples)
                ))
            }
            
            # Increment cell counter and report progress
            cell <- 1 + cell
            cat("Seed:", seed, "| Beta level", cell, "of", settings$nCells, "\n")
        }
  }
  
  # Create a file to indicate completion of this seed
  write('Seed has ended running', paste(settings$output.folder, "seed-", seed, "_end.txt", sep=""))
  
  # Compile all results into a single list
  resultado <- list(
      "noDiff" = out_NoDiff,
      "Diff" = out_Diff, 
      "settings" = settings,
      "reps" = data.frame("bad_JAGS" = redo_JAGS, "bad_Rhat" = redo_Rhat)
  )
  
  # Save results to file
  save(resultado, file = paste(settings$output.folder, "seed-", seed, ".RData", sep=""))
  
  # Return results
  return(resultado)
}
  
#x <- HDDM_simBySeed_withinSubject(1,settings, forceRun=TRUE)
#a = x$Diff
#a[,"true.values"]
#b = x$noDiff
#b[,"true.values"]

#x2 = resultado[1,]
#a2 = x2$Diff
#a2[,"true.values"]
#b2 = x2$noDiff
#b2[,"true.values"]

