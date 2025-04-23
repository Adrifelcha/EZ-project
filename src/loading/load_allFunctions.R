#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function sources all R scripts from a specified directory and its subdirectories
# to load all custom functions into the current R environment.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# MORE DETAILS: 
# The function recursively searches through the specified source directory,
# finding all .R files both in the main directory and in all subdirectories.
# Each R script is then sourced, making all functions defined in those files
# available in the current R session.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
load_allCustomFunctions <- function(source_dir = here("src")) {
  ruler <- paste(rep("=", 60), collapse = "")
  sub_ruler <- paste(rep("-", 40), collapse = "")
  
  cat("\n", ruler, "\n")
  cat(" LOADING CUSTOM FUNCTIONS FROM:\n")
  cat(paste(" ", source_dir, "\n"))
  cat(ruler, "\n\n")
  
  # Get all directories within source folder
  src_dirs <- list.dirs(source_dir, recursive = FALSE)
  
  # Loop through each directory
  for (dir_path in src_dirs) {
    dir_name <- basename(dir_path)
    cat(sub_ruler, "\n")
    cat(paste(" Subdirectory:", dir_name, "\n"))
    cat(sub_ruler, "\n")
    
    # Get all R files in the current directory
    r_files <- list.files(dir_path, pattern = "\\.R$", full.names = TRUE)
    
    # Source each R file
    for (r_file in r_files) {
      file_name <- basename(r_file)
      cat(paste("  Sourcing:", file_name, "\n"))
      source(r_file)
    }
    
    cat("\n") # Add vertical space between directories
  }
  
  # Also source any R files directly in the source directory
  r_files_root <- list.files(source_dir, pattern = "\\.R$", full.names = TRUE)
  if (length(r_files_root) > 0) {
    cat(sub_ruler, "\n")
    cat(paste(" Root directory:", basename(source_dir), "\n"))
    cat(sub_ruler, "\n")
    
    for (r_file in r_files_root) {
      file_name <- basename(r_file)
      cat(paste("  Sourcing:", file_name, "\n"))
      source(r_file)
    }
    
    cat("\n")
  }
  
  cat(ruler, "\n")
  cat(" All custom functions loaded successfully!\n")
  cat(ruler, "\n\n")
}