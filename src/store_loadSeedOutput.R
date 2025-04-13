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
  # Validate directory
  if(is.na(directory)) {
    stop("Directory not specified")
  }else{
    if(!dir.exists(directory)) {
      stop("Directory does not exist: ", directory)
    }
  }
  
  # seed-specific results are stored in .RData files
  pattern = ".RData$"

  # Get list of files matching the pattern
  files <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    warning("No files matching pattern '", pattern, "' found in directory: ", directory)
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
  
  if(length(all_seed_results) == 0) {
    warning("No valid files were found with object '", object_name, "'")
    return(list())
  }
  
  # Create matrix-like structure expected by store_BetaParallelOutput
  # We're building a matrix where each row corresponds to a seed
  # and columns are the components of the results
  
  # Get the number of valid seeds
  n_seeds <- length(all_seed_results)
  
  # Create a matrix with n_seeds rows and 2 columns (for noDiff and Diff)
  result_matrix <- matrix(list(), nrow = n_seeds, ncol = 2)
  colnames(result_matrix) <- c("noDiff", "Diff")
  
  # Store settings from first file
  settings <- all_seed_results[[1]]$settings
  
  # Create a data frame for the reps information
  reps_data <- data.frame(
    bad_JAGS = numeric(n_seeds),
    bad_Rhat = numeric(n_seeds)
  )
  
  # Fill the matrix with results from each seed
  for(i in seq_len(n_seeds)) {
    result_matrix[i, "noDiff"] <- list(all_seed_results[[i]]$noDiff)
    result_matrix[i, "Diff"] <- list(all_seed_results[[i]]$Diff)
    reps_data[i, ] <- all_seed_results[[i]]$reps
  }
  
  # Create the final output structure
  resultado <- structure(
    result_matrix,
    settings = settings,
    reps = reps_data
  )
  
  attr(resultado, "n_seeds") <- n_seeds
  
  cat("Successfully combined", n_seeds, "seed-specific output files\n")
  
  return(resultado)
}
