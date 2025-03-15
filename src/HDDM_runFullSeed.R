###################################################################################
# MODULAR FUNCTION CODING AHEAD:
#
# HDDM_runFullSeed is a higher-level function intended for running
#         SIMULATION STUDIES with PARALLEL COMPUTING.
# It generates a single parameter recovery check across all simulation cells.
###################################################################################
# Inputs:
# - seed: Master random seed to use for all simulations
# - settings: List containing all simulation settings and design factors
# - forceRun: Whether to force new simulations even if results exist
# - redo_if_bad_rhat: Whether to repeat simulations with poor convergence
# - rhat_cutoff: Threshold for acceptable R-hat values (default: 1.05)
#
# Returns a list containing:
#   * betaEffect: Results from all simulation cells
#   * reps: Count of repeated simulations due to errors or poor convergence
###################################################################################

HDDM_runFullSeed <- function(seed, settings, forceRun, redo_if_bad_rhat=FALSE, rhat_cutoff=NA){
  # Start timing the entire simulation study
  grand_tic <- clock::date_now(zone="UTC")
  
  # Set master random seed for reproducibility
  set.seed(seed)
  
  # Load required libraries with suppressed messages
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  
  # Determine output file name to store seed-specific results
  fileName <- paste(settings$output.folder, "seed-", seed, ".RData", sep="")
  
  # Check if this seed has already been run
  if(file.exists(fileName) & !forceRun){
     stop("Seed already run")
  }
  
  # Set default R-hat cutoff if not specified
  if(redo_if_bad_rhat & is.na(rhat_cutoff)){
    rhat_cutoff <- 1.05
  }
  
  # Create a marker file to indicate simulation has started
  write('Seed has been initiated', paste(settings$output.folder, "seed-", seed, "_start.txt", sep=""))
  
  # Initialize storage for results
  out_Beta <- list()  # Will store results from all cells
  cell <- 0           # Counter for current cell
  redo_JAGS <- 0      # Counter for JAGS errors
  redo_Rhat <- 0      # Counter for R-hat convergence issues
  
  # Loop through all design factors (hierarchical, metaregression, t-test)
  for(d in settings$design_levels){
      
      # Loop through all participant count levels
      for(p in settings$participant_levels){
          # Create design matrix for predictors
          # First column: binary predictor (0/1) for t-test designs
          # Second column: continuous predictor (0-1) for metaregression
          X <- cbind((0:(p-1))%%2, (0:(p-1))/p)
          colnames(X) <- settings$design_levels
          
          # Loop through all trial count levels
          for(t in settings$trial_levels){
            
                # Loop through all criterion parameters (which parameter is affected by predictors)
                for(c in settings$criterion_levels){
                    # Flag to control R-hat checking loop
                    rhat_not_verified <- TRUE
                    
                    # Initialize seed for this cell based on master seed
                    this.seed <- seed
                    
                    # Display progress information
                    cat("Running cell", cell, "of", settings$nCells, "\n")
                    
                    # Keep generating and analyzing datasets until R-hat criteria are met
                    while(rhat_not_verified){
                        # Set seed for this attempt
                        set.seed(this.seed)
                        
                        # Generate dataset with known parameters
                        design <- HDDM_setup(settings$priors[[d]], p, t, d, X[,d], c, settings$fromPrior, Show = FALSE)
                        
                        # Attempt to run JAGS with error handling
                        z <- try(runJags <- HDDM_runJAGS(
                            summaryData = design$sumData, 
                            nTrials = t, 
                            X = X[,d], 
                            jagsData = settings$jagsData[[d]], 
                            jagsParameters = settings$jagsParameters[[d]], 
                            jagsInits = settings$jagsInits[[as.character(p)]], 
                            n.chains = settings$n.chains, 
                            n.burnin = settings$n.burnin, 
                            n.iter = settings$n.iter, 
                            n.thin = settings$n.thin, 
                            modelFile = settings$modelFile[d,c], 
                            Show = FALSE, 
                            track_allParameters = FALSE))
                        
                        # If JAGS error occurs, retry with slightly different seed
                        if(inherits(z, "try-error")){ 
                              cat("Repeating cell", cell, "of", settings$nCells, "due to a JAGS error \n")
                              this.seed <- this.seed + 0.01  # Increment seed slightly
                              set.seed(this.seed)
                              
                              # Generate new dataset and try again
                              design <- HDDM_setup(settings$priors[[d]], p, t, d, X[,d], c, settings$fromPrior, Show = FALSE)
                              z <- try(runJags <- HDDM_runJAGS(
                                  summaryData = design$sumData, 
                                  nTrials = t, 
                                  X = X[,d], 
                                  jagsData = settings$jagsData[[d]], 
                                  jagsParameters = settings$jagsParameters[[d]], 
                                  jagsInits = settings$jagsInits[[as.character(p)]], 
                                  n.chains = settings$n.chains, 
                                  n.burnin = settings$n.burnin, 
                                  n.iter = settings$n.iter, 
                                  n.thin = settings$n.thin, 
                                  modelFile = settings$modelFile[d,c], 
                                  Show = FALSE, 
                                  track_allParameters = FALSE))
                              
                              # Increment error counter and break if too many errors
                              redo_JAGS <- redo_JAGS + 1
                              if(redo_JAGS > 5){ 
                                  break  # Give up after 5 attempts
                              }
                        }
                        
                        # Check if R-hat values indicate good convergence
                        count_bad_rhats <- sum(runJags$rhats[settings$jagsParameters[[d]]] > rhat_cutoff)
                        
                        # Exit loop if R-hat check is disabled or all R-hats are good
                        if((!redo_if_bad_rhat) | (count_bad_rhats == 0)){ 
                            rhat_not_verified <- FALSE
                        } else { 
                            # Otherwise, try again with slightly different seed
                            cat("Repeating cell", cell, "of", settings$nCells, "due to bad Rhats \n")
                            this.seed <- this.seed + 0.01     
                            redo_Rhat <- redo_Rhat + 1
                        }
                    } # Close while() loop for R-hat verification
                    
                    # Store results for this design cell
                    out_Beta <- rbind(out_Beta, list(
                        seed = this.seed,           # Seed used for this cell
                        p = p,                      # Number of participants
                        t = t,                      # Number of trials
                        d = d,                      # Design type
                        c = c,                      # Criterion parameter
                        rhats = runJags$rhats,      # Convergence diagnostics
                        true.values = design$parameter_set,      # True parameter values
                        mean.estimates = runJags$estimates,      # Posterior means
                        std.estimates = runJags$estimates,       # Posterior SDs
                        elapsed.time = runJags$clock             # Computation time
                    ))
                    
                    # Increment cell counter
                    cell <- 1 + cell
                }
          }
      }
  }
  
  # Create a marker file to indicate simulation has completed
  grand_toc <- clock::date_now(zone="UTC")
  total_time <- difftime(grand_toc, grand_tic, units="mins")
  write(paste("Running this seed took ", total_time, "minutes.\n"), 
        paste(settings$output.folder, "seed-", seed, "_end.txt", sep=""))
  
  # Create and save output object
  output <- list(
      "betaEffect" = out_Beta,                                # Results from all cells
      "reps" = data.frame("bad_JAGS" = redo_JAGS,            # Count of JAGS errors
                          "bad_Rhat" = redo_Rhat)             # Count of R-hat issues
  )
  
  # Save results to file
  save(output, file=fileName)  
  
  # Return results
  return(output)
}
  