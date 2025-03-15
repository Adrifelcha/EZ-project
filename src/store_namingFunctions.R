#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function generates standardized filenames for simulation output files
# Note: This function ISN'T made to work for within-subject designs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
nameOutput <- function(nTrials, nParticipants, nDatasets, modelType, fromPrior, output.folder = NA) {
    # Set default output folder using here() if not specified
    if(is.na(output.folder)) {
        output.folder <- here::here("output", "RData-results")
    }
    
    # Ensure output folder exists
    if(!dir.exists(output.folder)){
        dir.create(output.folder, recursive = TRUE)
    }
    
    # Create a compact base filename
    start <- paste("sim_P", nParticipants, "T", nTrials, "D", nDatasets, sep="")
    
    # Add a short code for model type
    modelCode <- switch(modelType,
                        "hierarchical" = "Hier",
                        "metaregression" = "Meta",
                        "ttest" = "Ttst",
                        "X")  # Default if unknown
    
    # Add model code and prior indicator
    filename <- paste0(start, "_", modelCode, "_", ifelse(fromPrior, "fromPrior", "fromUnif"), ".RData")
    
    # Combine folder and filename, ensuring proper path separator
    if(substr(output.folder, nchar(output.folder), nchar(output.folder)) != "/" && 
       substr(output.folder, nchar(output.folder), nchar(output.folder)) != "\\") {
        output.folder <- paste0(output.folder, "/")
    }
    outputFile <- paste0(output.folder, filename)    
    
    return(outputFile)
}