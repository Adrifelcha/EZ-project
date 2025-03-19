#################################################################################
# S T O R E   P A R A L L E L   O U T P U T
#################################################################################
# This function processes and stores the results from a parallel simulation study
# 
# Inputs:
#   "output": Combined results from parallel processing (from foreach %dopar%)
#   "settings": List containing simulation parameters
#   "saveTo": Directory path where to save the processed results
#################################################################################

store_BetaParallelOutput <- function(output, settings, saveTo = NA){  
  # Extract simulation settings from the settings list
  nDatasets      <- settings$nDatasets      # Number of datasets/seeds simulated
  output.folder  <- settings$output.folder  # Folder where raw results are stored
  nP             <- settings$nParticipants  # Number of participants per dataset
  nT             <- settings$nTrialsPerCondition  # Number of trials per condition
  beta_levels    <- settings$beta_levels    # The effect sizes tested
  nchain         <- settings$n.chains       # Number of MCMC chains used
  n.iter         <- settings$n.iter         # Number of MCMC iterations
  n.burnin       <- settings$n.burnin       # Number of MCMC burn-in iterations
  n.thin         <- settings$n.thin         # Number of MCMC thinning
  nIter <- (n.iter - n.burnin) / n.thin
  
  # Create file names for different beta levels (B, B1, B2, etc.)
  B.files <- paste("B", c("", 1:(length(settings$beta_levels)-1)), sep="")
  
  # Calculate total number of parameters (7 global + 4 per participant)
  nParams <- 7 + (4 * nP)
  
  # Process each beta level separately
  i <- 1
  for(b in beta_levels){
      # Select appropriate data based on whether this is a null effect (beta=0) or not
      if(b == 0){ 
          out = output[,"noDiff"]  
      } else {  
          out = output[,"Diff"]  
      }
      
      # Initialize arrays to store timing information
      clock <- rep(NA, nDatasets)   # JAGS runtime
      clock2 <- rep(NA, nDatasets)  # Total runtime
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Extract parameter names from the first dataset to use as array dimensions
      #################################################################################################
      # Find the first result with this beta level
      first_result_idx <- which(out[[1]][,"beta"] == b)[1]
      
      # Extract parameter names from different components of the results
      names_true      <- sub('.*true.values.', '', names(unlist(out[[1]][first_result_idx, "true.values"])))
      names_estimates <- sub(".*\\.", "", sub('.*mean.estimates.', '', names(unlist(out[[1]][first_result_idx, "mean.estimates"]))))
      names_rhats     <- sub(".*\\.", "", names(unlist(out[[1]][first_result_idx, "rhats"])))
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create empty arrays to store aggregated results
      #################################################################################################
      # Array for beta parameter chains across all datasets
      betaChains <- array(NA, dim=c(nIter, nchain, nDatasets))
      
      # Array for true parameter values
      trueVals <- array(NA, dim=c(nDatasets, nParams), 
                       dimnames = list(paste("seed", 1:nDatasets), names_true))
      
      # Array for posterior mean estimates
      meanPosts <- array(NA, dim=c(nDatasets, nParams), 
                        dimnames = list(paste("seed", 1:nDatasets), names_estimates))
      
      # Array for posterior standard deviations
      sdevPosts <- array(NA, dim=c(nDatasets, nParams), 
                        dimnames = list(paste("seed", 1:nDatasets), names_estimates))
      
      # Array for MCMC convergence diagnostics (Rhat values)
      rhats <- array(NA, dim=c(nDatasets, nParams+1), 
                    dimnames = list(paste("seed", 1:nDatasets), names_rhats))
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Extract data from each dataset and fill the arrays
      #################################################################################################
      for(k in 1:nDatasets){
          # Find which row in this result corresponds to the current beta level
          thisH <- as.numeric(which(out[[k]][,"beta"] == b))
          
          # Extract values and store in the appropriate arrays
          trueVals[k,] <- unlist(out[[k]][thisH, "true.values"])
          meanPosts[k,] <- unlist(out[[k]][thisH, "mean.estimates"])
          sdevPosts[k,] <- unlist(out[[k]][thisH, "std.estimates"])
          rhats[k,] <- unlist(out[[k]][thisH, "rhats"])
          betaChains[,,k] <- as.matrix(out[[k]][thisH, "beta_chains"]$beta_chains)
          
          # Store timing information
          clock[k] <- as.numeric(out[[k]][thisH, "jags.time"])
          clock2[k] <- as.numeric(out[[k]][thisH, "total.time"])
      }
   
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create a list with all processed results for this beta level
      #################################################################################################
      simStudy_Beta <- list(
          "true" = trueVals,                # True parameter values
          "estimates" = meanPosts,          # Posterior mean estimates
          "variance" = sdevPosts^2,         # Posterior variance (square of SD)
          "rhats" = rhats,                  # MCMC convergence diagnostics
          "jagsTime" = clock,               # JAGS runtime
          "totalTime" = clock2,             # Total runtime
          "settings" = settings,            # Simulation settings
          "beta_chain" = betaChains  # Beta parameter chains
      )
      
      # Create the output file name with participant and trial counts
      # Normalize the path to remove any double slashes
      if(is.na(saveTo)){
        saveTo <- here("output", "RData-results")
        dir.create(saveTo, recursive = TRUE, showWarnings = FALSE)
      }

      saveTo <- gsub("/$", "", saveTo)  # Remove trailing slash if present
      outputFile <- file.path(saveTo, paste0("simHypTesting_P", nP, "T", nT, "_", B.files[i], ".RData"))
      
      # Save the processed results to a file
      save(simStudy_Beta, file=outputFile)
      
      # Move to the next beta level
      i <- i + 1      
   }
} 
