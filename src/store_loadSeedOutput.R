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
  
  # Initialize empty list to store combined output
  recover_output <- list()
  first_file <- TRUE
  
  # Load each file and combine outputs
  for(archive in files) {
    
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
    file_output <- get(object_name, envir = temp_env)
    
    # If this is the first valid file, initialize the output structure
    if(first_file) {
      recover_output <- file_output
      first_file <- FALSE
    } else {
      # For subsequent files, rbind the appropriate components
      recover_output <- list(
        "noDiff" = rbind(recover_output$noDiff, file_output$noDiff),
        "Diff" = rbind(recover_output$Diff, file_output$Diff),
        "settings" = recover_output$settings,  # Keep settings from first file
        "reps" = rbind(recover_output$reps, file_output$reps)
      )
    }
  }
  
  if(first_file) {
    warning("No valid files were found with object '", object_name, "'")
    return(list())
  }
  
  cat("Successfully combined", length(files), "seed-specific output files\n")
  
  return(recover_output)
}
