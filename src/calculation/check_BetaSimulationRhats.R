#################################################################################
# C H E C K   S I M U L A T I O N   R H A T S
#################################################################################
# This function checks the Rhat convergence diagnostics from simulation study results
# 
# Inputs:
#   "resultsFile": Path to the RData file containing simulation results
#   "threshold": Threshold for acceptable Rhat values (default: 1.05)
#   "plotHistogram": Whether to plot a histogram of Rhat values (default: TRUE)
#   "returnData": Whether to return the problematic Rhats (default: FALSE)
#################################################################################

check_BetaSimulationRhats <- function(resultsFile, threshold = 1.05, 
                                      plotHistogram = TRUE, returnData = FALSE) {    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read simulation study results
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load the simulation results (if available)
  if (!file.exists(resultsFile)){      
        stop("Results file not found: ", resultsFile)
  }else{        load(resultsFile)                        }
  
  # Get the rhats data
  rhats <- simStudy_Beta$rhats
  # Exclude deviance column (if present)
  if ("deviance" %in% colnames(rhats)) {
        parameters <- colnames(rhats) != "deviance"
        rhats <- rhats[,parameters]
  }
  # Identify dimensions
  nDatasets <- nrow(rhats)
  nParams <- ncol(rhats)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check convergence issues
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find parameters with R-hat values exceeding the threshold
  bad_rhats <- which(rhats > threshold, arr.ind = TRUE)
  # Count problematic datasets and parameters
  n_bad_rhats <- nrow(bad_rhats)
  convergence_issues <- n_bad_rhats > 0

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Display results
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Identify beta values from filename for reporting
  beta_value <- unique(simStudy_Beta$true[,"betaweight"])
  cat("\n================================================================\n")
  cat(paste("RHAT CHECK FOR BETA =", beta_value, "\n"))
  cat("================================================================\n")
  
  # Extract re-run information
  reruns <- simStudy_Beta$reruns  
  # Total number of reruns
  jags_reruns <- sum(reruns$jags, na.rm = TRUE)
  rhat_reruns <- sum(reruns$rhat, na.rm = TRUE)
  # Number of seeds with reruns
  datasets_with_jags_reruns <- sum(reruns$jags > 0, na.rm = TRUE)
  datasets_with_rhat_reruns <- sum(reruns$rhat > 0, na.rm = TRUE)
  # Percentages of seeds with reruns
  pct_datasets_jags_reruns <- (datasets_with_jags_reruns / nDatasets) * 100
  pct_datasets_rhat_reruns <- (datasets_with_rhat_reruns / nDatasets) * 100
  
  # Display re-run information
  cat("\n----------------------------------------------------------------\n")
  cat("RE-RUN SUMMARY\n")
  cat("----------------------------------------------------------------\n")
  
  cat(paste0("JAGS failures requiring reruns: ", jags_reruns, " across ", 
            datasets_with_jags_reruns, " datasets (", 
            round(pct_datasets_jags_reruns, 2), "%)\n"))
  
  cat(paste0("Rhat failures requiring reruns: ", rhat_reruns, " across ", 
            datasets_with_rhat_reruns, " datasets (", 
            round(pct_datasets_rhat_reruns, 2), "%)\n"))
  
  # More detailed information if there were any reruns
  if(jags_reruns > 0 || rhat_reruns > 0) {
    cat("\nRerun counts per dataset:\n")
    
    # Find max reruns per dataset for both types
    max_jags_reruns <- max(reruns$jags, na.rm = TRUE)
    max_rhat_reruns <- max(reruns$rhat, na.rm = TRUE)
    
    if(max_jags_reruns > 0) {
      cat("\nJAGS reruns frequency:\n")
      jags_table <- table(reruns_data$jags)
      print(jags_table)
    }
    
    if(max_rhat_reruns > 0) {
      cat("\nRhat reruns frequency:\n")
      rhat_table <- table(reruns_data$rhat)
      print(rhat_table)
    }
    
    # Identify datasets with many reruns (if any exceed a threshold)
    many_reruns_threshold <- 3
    datasets_with_many_jags <- which(reruns$jags >= many_reruns_threshold)
    datasets_with_many_rhats <- which(reruns$rhat >= many_reruns_threshold)
    
    if(length(datasets_with_many_jags) > 0) {
      cat("\nDatasets with many JAGS reruns (≥", many_reruns_threshold, "):\n")
      for(ds in datasets_with_many_jags) {
        cat(paste0("seed", ds, ": ", reruns_data$jags[ds], " reruns\n"))
      }
    }
    
    if(length(datasets_with_many_rhats) > 0) {
      cat("\nDatasets with many Rhat reruns (≥", many_reruns_threshold, "):\n")
      for(ds in datasets_with_many_rhats) {
        cat(paste0("seed", ds, ": ", reruns_data$rhat[ds], " reruns\n"))
      }
    }
  }
  
  cat("----------------------------------------------------------------\n")
  
  # If problematic R-hat values found, create diagnostic information
  if (convergence_issues) {
  
    # Count occurrences of each problematic parameter
    param_counts <- table(colnames(rhats)[bad_rhats[, 2]])
    
    # Count datasets with convergence issues
    bad_datasets <- unique(bad_rhats[, 1])
    n_bad_datasets <- length(bad_datasets)
    
    # Calculate percentages
    pct_bad_chains <- (n_bad_rhats / (nDatasets * nParams)) * 100
    pct_bad_datasets <- (n_bad_datasets / nDatasets) * 100
    
    # Create histogram if requested
    if (plotHistogram) {
      par(mfrow = c(1, 1))
      hist(as.vector(rhats), breaks = 50, 
           main = paste("Rhat Distribution (Beta =", beta_value, ")"),
           xlab = "Rhat Value")
      abline(v = threshold, col = "red", lty = 2)
      legend("topright", 
             paste0("Rhat > ", threshold, " (", round(pct_bad_chains, 2), "% of all values)"),
             col = "red", lty = 2)
    }
    
    # Print summary information
    cat(paste0("Convergence issues detected in ", n_bad_datasets, " out of ", 
              nDatasets, " datasets (", round(pct_bad_datasets, 2), "%)\n"))
    cat(paste0("Total problematic Rhat values: ", n_bad_rhats, " (", 
              round(pct_bad_chains, 2), "% of all values)\n"))
    cat("\nProblematic parameters and occurrence counts:\n")
    print(param_counts)
    
    # List datasets with issues
    cat("\nDatasets with convergence issues:\n")
    cat(paste("seed", bad_datasets, collapse = ", "), "\n")
    
    # Create a detailed table of problematic values
    if (n_bad_rhats <= 100) {  # Only show details if not too many
      cat("\nDetailed problematic Rhat values:\n")
      bad_rhat_table <- data.frame(
        Dataset = paste0("seed", bad_rhats[, 1]),
        Parameter = colnames(rhats)[bad_rhats[, 2]],
        Rhat = apply(bad_rhats, 1, function(idx) rhats[idx[1], idx[2]])
      )
      bad_rhat_table <- bad_rhat_table[order(bad_rhat_table$Rhat, decreasing = TRUE), ]
      print(bad_rhat_table)
    } else {
      cat("\nToo many problematic values to display individually.\n")
      cat("Highest Rhat value:", max(rhats, na.rm = TRUE), "\n")
    }
  } else {
    # If all R-hat values are acceptable, print confirmation message
    cat(paste0("All Rhat values are below ", threshold, " - good convergence!\n"))
    
    if (plotHistogram) {
      par(mfrow = c(1, 1))
      hist(as.vector(rhats), breaks = 50, 
           main = paste("Rhat Distribution (Beta =", beta_value, ")"),
           xlab = "Rhat Value")
      abline(v = threshold, col = "green", lty = 2)
      legend("topright", paste0("All Rhat < ", threshold), col = "green", lty = 2)
    }
  }
  
  cat("================================================================\n")
  
  # If returning data, include rerun information
  if (returnData && has_convergence_issues) {
    return_data <- list(
      bad_rhats = bad_rhats,
      rhats = rhats,
      param_counts = param_counts,
      bad_datasets = bad_datasets,
      n_bad_rhats = n_bad_rhats,
      n_bad_datasets = n_bad_datasets,
      pct_bad_chains = pct_bad_chains,
      pct_bad_datasets = pct_bad_datasets
    )
    
    # Add rerun data if available
    if(exists("simStudy_Beta") && "reruns" %in% names(simStudy_Beta)) {
      return_data$reruns = simStudy_Beta$reruns
    }
    
    return(return_data)
  } else if (returnData) {
    return_data <- list(
      rhats = rhats,
      has_convergence_issues = FALSE
    )
    
    # Add rerun data if available
    if(exists("simStudy_Beta") && "reruns" %in% names(simStudy_Beta)) {
      return_data$reruns = simStudy_Beta$reruns
    }
    
    return(return_data)
  }
}