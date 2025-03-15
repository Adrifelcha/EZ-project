###################################################################################
# MODULAR FUNCTION CODING AHEAD:
#
# HDDM_runSims is a higher-level function that manages a simulation study 
#                  FOR A SPECIFIC SIMULATION DESIGN CELL: 
# FIXED: nParticipants, nTrials, modelType, and criterion.
# REPEAT: nDatasets times, with different random seeds.
###################################################################################
# This function orchestrates the entire simulation study process:
# 1. Sets up the simulation environment
# 2. Generates multiple datasets from sampled parameters
# 3. Runs JAGS on each dataset to obtain posterior estimates
# 4. Performs convergence diagnostics and saves results into an external file
# 5. Returns results as a list
#
# Inputs:
# - nParticipants: Number of participants per dataset
# - nTrials: Number of trials per participant
# - nDatasets: Number of datasets to simulate (default: 10)
# - priors: Data frame containing prior distribution parameter specifications
# - modelType: Type of model ("hierarchical", "metaregression", "ttest") - (default: "hierarchical")
# - criterion: Parameter affected by the predictor ("drift", "bound", "nondt") - (default: "drift")
# - n.chains: Number of MCMC chains to run (default: 3)
# - n.burnin: Number of burn-in iterations (default: 250)
# - n.iter: Total number of iterations including burn-in (default: 2000)
# - n.thin: Thinning interval for MCMC samples (default: 1)
# - Show: Whether to display diagnostic information to the console (default: TRUE)
# - forceSim: Whether to force new simulations even if results exist (default: FALSE)
# - fromPrior: Whether to sample true parameters from priors (default: TRUE)
# - output.folder: Directory to save results (default: "repo-root/output/RData-results")
# - track_allParameters: Whether to track all parameters or just hierarchical ones (default: hierarchical)
# - rhatCheck: Whether to check convergence diagnostics (default: TRUE)
# - redo_if_bad_rhat: Whether to repeat simulations with poor convergence (default: FALSE)
#
# Returns a list containing:
#   * rhats: Convergence diagnostics for all parameters and datasets
#   * estimates: Posterior means for all parameters and datasets
#   * credIntervals: 95% credible intervals for all parameters and datasets
#   * trueValues: True parameter values used to generate each dataset
#   * settings: Simulation settings used
#   * n.chains: Number of MCMC chains used
#   * totalTime: Total computation time
#   * seed_id: Random seeds used for each dataset
###################################################################################

HDDM_runSims <- function(nParticipants, nTrials, nDatasets = 10, priors = NA, modelType = NA, criterion = NA, n.chains = 3, 
                         n.burnin=250, n.iter=2000, n.thin=1, Show=TRUE, forceSim = FALSE, fromPrior=TRUE, output.folder = NA,
                         track_allParameters = FALSE, rhatCheck=TRUE, redo_if_bad_rhat=FALSE, init_drift_sd = 0.3){
    
    # Start timing the entire simulation study
    grand_tic <- clock::date_now(zone="UTC")
    
    #################################
    # Initial checks and setup
    #################################
    # Load necessary R libraries
    suppressMessages(library(R2jags))
    
    # Validate and set default model type if needed
    if(is.na(modelType)){    
        modelType = "hierarchical"    # Default model type
    } else {  
        valid.models <- c("hierarchical", "metaregression", "ttest") 
        if(!(modelType %in% valid.models)){
            stop("Please specify a valid modelType: 'hierarchical' (default), 'metaregression' 'ttest'")
        }
    }
    
    # Set default output folder if not specified
    if(is.na(output.folder)){
        output.folder <- here("output", "RData-results")
    }

    # Ensure output folder exists
    if(!dir.exists(output.folder)){
        dir.create(output.folder, recursive = TRUE)
        cat("Created output directory:", output.folder, "\n")
    }

    # Determine output file name based on simulation parameters
    outputFile <- nameOutput(nTrials, nParticipants, nDatasets, modelType, fromPrior, output.folder)
    
    # Check if we need to run simulations again or can use existing results    
    if(!forceSim){
        if(file.exists(outputFile)){
            # Load default priors if needed
            if(sum(is.na(priors))>0){  
                myPriors <- JAGS_priors(Show = FALSE, modelType)
            } else {  
                myPriors <- priors
            }
            
            myNChains <- n.chains
            load(outputFile)  # Load existing results
            
            # Check if existing results used the same priors and chains
            checkPriors <- sum(output$settings$prior != myPriors, na.rm = TRUE)
            checkNChains <- sum(output$n.chains != myNChains, na.rm = TRUE)
            needToRun <- sum(checkPriors, checkNChains) > 0
        } else {  
            needToRun <- TRUE  # No existing results found
        }
    } else {  
        needToRun <- TRUE  # Force new simulations
    }
    
    ###################################
    # Run simulation study (if needed)
    ###################################
    if(needToRun){
        
        # SET UP
        #########
        # Create settings list to store simulation design parameters
        settings <- list("nPart"= nParticipants, "nTrials"= nTrials,
                         "modelType" = modelType, "nDatasets" = nDatasets)
        
        # Configure predictor variable X based on model type
        if(modelType != "hierarchical"){
            # Set default criterion to drift if not specified
            if(is.na(criterion)){
                criterion <- "drift"
            }
            
            # Create predictor variable based on model type
            X <- 0:nParticipants  # Base sequence
            if(modelType == "ttest"){
                X <- X %% 2    # Binary predictor for t-test (0/1)
            } else {
                X <- X/nParticipants  # Continuous predictor scaled to [0,1]
            }        
            settings <- c(settings, list("X" = X, "criterion" = criterion))
        } else {    
            X <- NA  # No predictor for hierarchical model
        }
        
        # Display design information if requested
        if(Show){  
            show_design(settings)  
        }
        
        # Load default priors if needed and add to settings
        if(sum(is.na(priors)) > 0){    
            priors <- JAGS_priors(Show, modelType)    
        } else {
            if(Show){    
                show_priors(priors)
            }                         
        }
        settings <- c(settings, list("prior" = priors))
        
        # ~~~~~~~~~~~~~~~~ JAGS configuration ~~~~~~~~~~~~~~~~
        # Define parameters to track in JAGS based on user preferences
        if(track_allParameters){
            # Track all parameters including individual-level ones
            jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                                "drift_sdev", "nondt_sdev", "bound_sdev", "drift")
        } else {                  
            # Track only hierarchical parameters (more efficient)
            jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean")                    
        }
        
        # Add regression coefficient for models with a regression structure
        if(modelType != "hierarchical"){  
            jagsParameters <- c(jagsParameters, "betaweight")  
        }
        
        # Write appropriate JAGS model file based on model type and criterion
        name_start <- paste(here("output", "BUGS-models/"), "EZHBDDM", sep="")
        if(modelType == "hierarchical"){  
            modelFile <- paste(name_start, ".bug", sep="")
        } else {
            if(criterion == "bound"){ 
                modelFile <- paste(name_start, "_BetaBound.bug", sep="")  
            } else if(criterion == "nondt"){ 
                modelFile <- paste(name_start, "_BetaNondt.bug", sep="")  
            } else {  
                modelFile <- paste(name_start, "_BetaDrift.bug", sep="")  
            }
        }
        JAGS_writeModel(priors, modelType, criterion, modelFile)
        
        # Prepare data structure for JAGS
        jagsData = JAGS_passData(modelType)
        
        # Set initial values for MCMC chains
        jagsInits <- JAGS_inits(n.chains, nParticipants, custom_sd = init_drift_sd)
        
        # ~~~~~~~~~~~~~~~~ Initialize storage matrices ~~~~~~~~~~~~~~~~
        # Calculate number of parameters to track
        if(track_allParameters){  
            nParams <- (length(jagsParameters) - 3) + (nParticipants * 3)    
        } else {                    
            nParams <- length(jagsParameters)                           
        }
        
        # Create matrices to store results across all datasets
        MatEstimates <- matrix(NA, nrow=nDatasets, ncol=nParams)  # Posterior means
        MatTrueVal   <- matrix(NA, nrow=nDatasets, ncol=nParams)  # True values
        ArrayCredInt <- array(NA, dim=c(nDatasets, nParams, 2))   # Credible intervals
        MatRhats     <- matrix(NA, nrow=nDatasets, ncol=(nParams+1))  # R-hat values
        
        # Randomly select one dataset to show diagnostic plots for
        showChains <- rep(FALSE, nDatasets)
        seed_id <- rep(NA, nDatasets)
        if(Show){
            showChains[sample(nDatasets, 1)] <- TRUE
        }
        
        ######################
        #   Run iterations   #
        ######################
        repetition_counts <- 0  # Counter for repeated simulations due to errors
        
        # Loop through each dataset
        for(k in 1:nDatasets){
            rhat_not_verified <- TRUE  # Flag to control R-hat checking loop
            seed <- k  # Initial seed based on dataset number
            cat("============>> Dataset", k, "of", nDatasets, "\n")
            
            # Keep generating and analyzing datasets until R-hat criteria are met
            while(rhat_not_verified){
                # Set random seed for reproducibility
                set.seed(seed)
                
                # Generate dataset with known parameters
                design <- HDDM_setup(priors = priors, nPart = nParticipants, nTrials = nTrials, 
                                     modelType = modelType, X = X, criterion = criterion, 
                                     fromPrior = fromPrior, Show = FALSE)
                
                # For later datasets, use try() to handle potential errors
                if(k > 100){
                    # Attempt to run JAGS with error handling
                    runJags <- try(HDDM_runJAGS(summaryData = design$sumData, nTrials = nTrials, X = X, 
                                                jagsData = jagsData, jagsParameters = jagsParameters, 
                                                jagsInits = jagsInits, n.chains = n.chains, 
                                                modelFile = modelFile, Show = showChains[k],
                                                track_allParameters = track_allParameters))
                    
                    # If error occurs, keep trying with new seeds until successful
                    while(inherits(runJags, "try-error")){
                        repetition_counts <- repetition_counts + 1
                        seed <- seed + 10000  # Change seed substantially
                        set.seed(seed)
                        cat("============>> Dataset", k, "of", nDatasets, "+", repetition_counts, "\n")
                        
                        # Generate new dataset and try again
                        design <- HDDM_setup(priors = priors, nPart = nParticipants, nTrials = nTrials, 
                                             modelType = modelType, X = X, criterion = criterion, 
                                             fromPrior = fromPrior, Show = FALSE)
                        runJags <- try(HDDM_runJAGS(summaryData = design$sumData, nTrials = nTrials, X = X, 
                                                    jagsData = jagsData, jagsParameters = jagsParameters, 
                                                    jagsInits = jagsInits, n.chains = n.chains, 
                                                    modelFile = modelFile, Show = showChains[k],
                                                    track_allParameters = track_allParameters))
                    }
                } else {
                    # For earlier datasets, run without try() for faster debugging
                    runJags <- HDDM_runJAGS(summaryData = design$sumData, nTrials = nTrials, X = X, 
                                            jagsData = jagsData, jagsParameters = jagsParameters, 
                                            jagsInits = jagsInits, n.chains = n.chains, 
                                            modelFile = modelFile, Show = showChains[k],
                                            track_allParameters = track_allParameters)
                }
                
                # Check if R-hat values indicate good convergence
                count_bad_rhats <- sum(runJags$rhats[jagsParameters] > 1.05)
                
                # Exit loop if R-hat check is disabled or all R-hats are good
                if((!redo_if_bad_rhat) | (count_bad_rhats == 0)){ 
                    rhat_not_verified <- FALSE
                }
                
                # Update seed for next attempt if needed
                seed <- seed + 10000
            }
            
            # Store R-hat values for this dataset
            MatRhats[k,] <- runJags$rhats
            
            # Extract and store parameter estimates, true values, and credible intervals
            c <- 0  # Counter for estimate matrix columns
            d <- 0  # Counter for true value matrix columns
            
            # Loop through each parameter
            for(j in 1:length(runJags$estimates)){
                this <- names(runJags$estimates[j])  # Current parameter name
                m <- length(runJags$estimates[[this]])  # Number of estimates for this parameter
                w <- length(design$parameter_set[[this]])  # Number of true values for this parameter
                
                # Store posterior means and true values
                MatEstimates[k, (c+1):(c+m)] <- runJags$estimates[[this]]
                MatTrueVal[k, (d+1):(d+w)] <- design$parameter_set[[this]]
                
                # Store credible intervals, handling both scalar and vector parameters
                if(is.vector(runJags$credInterval[[j]])){
                    # For scalar parameters
                    ArrayCredInt[k, (c+1):(c+m), 1] <- runJags$credInterval[[this]][1]
                    ArrayCredInt[k, (c+1):(c+m), 2] <- runJags$credInterval[[this]][2]
                } else {
                    # For vector parameters
                    ArrayCredInt[k, (c+1):(c+m), 1] <- runJags$credInterval[[this]][1,]
                    ArrayCredInt[k, (c+1):(c+m), 2] <- runJags$credInterval[[this]][2,]
                }
                
                # Update counters
                c <- c + m
                d <- d + w
            }
            
            # Store seed used for this dataset
            seed_id[k] <- seed
        }
        
        # ~~~~~~~~~~~~~~~~ Create parameter names for output matrices ~~~~~~~~~~~~~~~~
        paramNames <- NA   # For posterior estimates
        paramNames2 <- NA  # For true values
        
        # Loop through each parameter to create appropriate column names
        for(j in 1:length(runJags$estimates)){
            this <- names(runJags$estimates[j])
            
            # Handle parameter names for estimates and credible intervals
            if(is.vector(runJags$credInterval[[this]])){
                paramNames <- c(paramNames, names(runJags$credInterval[this]))
            } else {
                paramNames <- c(paramNames, colnames(runJags$credInterval[[this]]))
            }
            
            # Handle parameter names for true values
            if(length(design$parameter_set[[this]]) == 1){
                paramNames2 <- c(paramNames2, names(design$parameter_set[this]))
            } else {
                # For vector parameters, create indexed names (e.g., "drift[1]", "drift[2]")
                labels <- paste(names(design$parameter_set[this]), "[", 1:length(design$parameter_set[[this]]), "]", sep="")
                paramNames2 <- c(paramNames2, labels)
            }
        }
        
        # Remove initial NA values and assign column names to result matrices
        paramNames <- paramNames[-1]
        paramNames2 <- paramNames2[-1]
        colnames(MatEstimates) <- paramNames
        colnames(ArrayCredInt) <- paramNames
        colnames(MatTrueVal) <- paramNames2
        colnames(MatRhats) <- names(runJags$rhats)
        
        # Check R-hat convergence diagnostics if requested
        if(Show | rhatCheck){
            JAGS_RhatCheck(MatRhats)
        }
        
        # Calculate total computation time
        grand_toc <- clock::date_now(zone="UTC")
        total_time <- difftime(grand_toc, grand_tic, units="mins")
        
        # Create and save output object
        output <- list("rhats" = MatRhats, 
                       "estimates" = MatEstimates, 
                       "credIntervals" = ArrayCredInt,
                       "trueValues" = MatTrueVal, 
                       "settings" = settings, 
                       "n.chains" = n.chains,
                       "totalTime" = total_time, 
                       "seed_id" = seed_id)
        tryCatch({
            save(output, file=outputFile)
            cat("Successfully saved results to:\n", outputFile, "\n\n")
        }, error = function(e) {
            cat("Error saving results:\n", e$message, "\n")
        })
        
        cat("Running this simulation study took ", total_time, "minutes.\n")
    } else {  
        # If using existing results, load and report
        cat("This simulation had been run before.\nLoading stored results: COMPLETE!\n",
            "Running this simulation study took ", output$totalTime, "minutes.\n")
        
        # Check R-hat convergence diagnostics if requested
        if(Show | rhatCheck){   
            JAGS_RhatCheck(output$rhats)      
        }
    }
    
    # Return results
    return(output)
}