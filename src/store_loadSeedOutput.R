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

load_seedOutput <- function(directory = NA) {
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
  
  # Initialize empty list to store combined output
  recover_output <- list()
  
  # Load each file and combine outputs
  for(archive in files) {
    
    cat("Loading file:", basename(archive), "\n")    
    
    # Load the file into a temporary environment to avoid namespace conflicts
    temp_env <- new.env()
    load(archive, envir = temp_env)
    
    # Check if the loaded file contains an 'output' object
    if(!exists("output", envir = temp_env)) {
      warning("File does not contain an 'output' object: ", archive)
      next
    }
    
    # Combine with existing output
    recover_output <- rbind(recover_output, temp_env$output)
  }
  
  cat("Successfully combined", length(files), "seed-specific output files\n")
  
  return(recover_output)
}
