#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function recovers the grand result matrix by loading available seed-specific .RData files
# and stacking them by row.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# MORE DETAILS: 
# When running a simulation study in parallel,
# we generate datasets and parameter estimations per design cell using different seeds.
# These results are stacked by row.
# Each row is stored in a seed-specific .RData file.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

load_seedOutput <- function(directory = NA, object_name = "resultado") {
  # seed-specific results are stored in .RData files
  pattern = ".RData$"

  # Get list of files matching the pattern in the directory
  files <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    warning("No '", pattern, "' files found in directory: ", directory)
    return(NULL)
  }
  
  # Initialize list to store all seed results
  all_seed_results <- list()

  # Load each file and collect results
  for(i in seq_along(files)) {
        archive <- files[i]
        cat("Loading file:", basename(archive), "\n")    
        
        # Load the file into a temporary environment to avoid namespace conflicts
        temp_env <- new.env()
        load(archive, envir = temp_env)
        
        # Check if the loaded file contains the expected object
        if(!exists(object_name, envir = temp_env)) {
          warning("File does not contain a '", object_name, "' object: ", archive)
          next
        }
        
        # Get the object from the environment
        all_seed_results[[i]] <- get(object_name, envir = temp_env)
  }
 
  # Get the number of valid seeds
  n_seeds <- length(all_seed_results)

  # Initialize data frame to store reps information
  reps_data <- data.frame(
    bad_JAGS = numeric(n_seeds),
    bad_Rhat = numeric(n_seeds)
  )
 
  # Detect simulation-study type based on the last seed
  detect <- all_seed_results[[n_seeds]]   # Use first seed as reference
  settings <- detect$settings       # Extract settings information

  # Create matrix-like structure expected by store_BetaParallelOutput
  # We're building a matrix where each row corresponds to a seed
  # and columns are the components of the results

  # Hypothesis testing: Within-subject design - two columns (noDiff, Diff)
  if(!is.null(detect$noDiff)){  
          # Create a matrix with n_seeds rows and 2 columns (for noDiff and Diff)
          result_matrix <- matrix(list(), nrow = n_seeds, ncol = 2)
          colnames(result_matrix) <- c("noDiff", "Diff")

          # Fill the matrix with results from each seed
          for(i in seq_len(n_seeds)) {
            result_matrix[i, "noDiff"] <- list(all_seed_results[[i]]$noDiff)
            result_matrix[i, "Diff"] <- list(all_seed_results[[i]]$Diff)
            reps_data[i, ] <- all_seed_results[[i]]$reps
          }
  }else{
          available_results <- c()
          if("hierarchical" %in% names(detect)){
             available_results <- c(available_results, "hierarchical")
          }
          if("betaEffect" %in% names(detect)){
             available_results <- c(available_results, "betaEffect")
          }          

          # Create a matrix with n_seeds rows and 2 columns (for noDiff and Diff)
          result_matrix <- matrix(list(), nrow = n_seeds, ncol = length(available_results))
          colnames(result_matrix) <- available_results

          # Fill the matrix with results from each seed
          for(i in seq_len(n_seeds)) {
            for(j in seq_along(available_results)){
              result_matrix[i, available_results[j]] <- list(all_seed_results[[i]][[available_results[j]]])
            }
            reps_data[i, ] <- all_seed_results[[i]]$reps
          }
  }

  # Create the final output structure
  resultado <- structure(result_matrix,
                         settings = settings,
                         reps = reps_data)
  
  attr(resultado, "n_seeds") <- n_seeds
  
  # Add settings as a separate element in the returned list
  final_output <- list(
    results = resultado,
    settings = settings
  )
  
  cat("Successfully combined", n_seeds, "seed-specific output files\n")
  
  return(final_output)
}
