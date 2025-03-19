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
  # Load the simulation results
  if (!file.exists(resultsFile)) {
    stop("Results file not found: ", resultsFile)
  }
  
  # Load the data
  load(resultsFile)
  
  # Extract Rhat values from the loaded data
  if (!exists("simStudy_Beta")) {
    stop("Expected 'simStudy_Beta' object not found in the results file")
  }
  
  rhats_data <- simStudy_Beta$rhats
  
  # Get dimensions
  nDatasets <- nrow(rhats_data)
  nParams <- ncol(rhats_data)
  
  # Exclude deviance column if present
  if ("deviance" %in% colnames(rhats_data)) {
    model_parameters <- colnames(rhats_data) != "deviance"
    rhats_data <- rhats_data[, model_parameters]
    nParams <- ncol(rhats_data)
  }
  
  # Find parameters with R-hat values exceeding the threshold
  bad_rhats <- which(rhats_data > threshold, arr.ind = TRUE)
  
  # Count problematic datasets and parameters
  n_bad_rhats <- nrow(bad_rhats)
  has_convergence_issues <- n_bad_rhats > 0
  
  # Extract beta value from filename for reporting
  beta_value <- sub(".*_B([0-9]*).*\\.RData$", "\\1", resultsFile)
  if (beta_value == resultsFile) beta_value <- "0" # If no number after B, it's B0
  
  # Print header
  cat("\n================================================================\n")
  cat(paste("RHAT CHECK FOR BETA =", beta_value, "\n"))
  cat("================================================================\n")
  
  # If problematic R-hat values found, create diagnostic information
  if (has_convergence_issues) {
    # Get names of problematic parameters
    bad_param_indices <- unique(bad_rhats[, 2])
    bad_param_names <- colnames(rhats_data)[bad_param_indices]
    
    # Count occurrences of each problematic parameter
    param_counts <- table(colnames(rhats_data)[bad_rhats[, 2]])
    
    # Count datasets with convergence issues
    bad_datasets <- unique(bad_rhats[, 1])
    n_bad_datasets <- length(bad_datasets)
    
    # Calculate percentages
    pct_bad_chains <- (n_bad_rhats / (nDatasets * nParams)) * 100
    pct_bad_datasets <- (n_bad_datasets / nDatasets) * 100
    
    # Create histogram if requested
    if (plotHistogram) {
      par(mfrow = c(1, 1))
      hist(as.vector(rhats_data), breaks = 50, 
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
        Parameter = colnames(rhats_data)[bad_rhats[, 2]],
        Rhat = apply(bad_rhats, 1, function(idx) rhats_data[idx[1], idx[2]])
      )
      bad_rhat_table <- bad_rhat_table[order(bad_rhat_table$Rhat, decreasing = TRUE), ]
      print(bad_rhat_table)
    } else {
      cat("\nToo many problematic values to display individually.\n")
      cat("Highest Rhat value:", max(rhats_data, na.rm = TRUE), "\n")
    }
  } else {
    # If all R-hat values are acceptable, print confirmation message
    cat(paste0("All Rhat values are below ", threshold, " - good convergence!\n"))
    
    if (plotHistogram) {
      par(mfrow = c(1, 1))
      hist(as.vector(rhats_data), breaks = 50, 
           main = paste("Rhat Distribution (Beta =", beta_value, ")"),
           xlab = "Rhat Value")
      abline(v = threshold, col = "green", lty = 2)
      legend("topright", paste0("All Rhat < ", threshold), col = "green", lty = 2)
    }
  }
  
  cat("================================================================\n")
  
  # Return data if requested
  if (returnData && has_convergence_issues) {
    return(list(
      bad_rhats = bad_rhats,
      rhats_data = rhats_data,
      param_counts = param_counts,
      bad_datasets = bad_datasets,
      n_bad_rhats = n_bad_rhats,
      n_bad_datasets = n_bad_datasets,
      pct_bad_chains = pct_bad_chains,
      pct_bad_datasets = pct_bad_datasets
    ))
  } else if (returnData) {
    return(list(
      rhats_data = rhats_data,
      has_convergence_issues = FALSE
    ))
  }
}

# Example usage:
# check_SimulationRhats("output/RData-results/simHypTesting_P30T20_B.RData")
# check_SimulationRhats("output/RData-results/simHypTesting_P30T20_B1.RData") 