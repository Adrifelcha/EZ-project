#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function extracts MCMC samples for a specified parameter from JAGS output
# Inputs:
# - parameter.name: String specifying the parameter to extract (e.g., "drift", "bound")
# - samples: JAGS output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
JAGS_extractSamples <- function(parameter.name, samples){
      # Extract the 3D array of MCMC samples from the samples object
      # Dimensions: iterations × chains × parameters
      postParam.Array <- samples$BUGSoutput$sims.array
      
      # Get the names of all parameters in the posterior array
      samplesID <- names(postParam.Array[1,1,])
      
      # Find indices of all parameters matching the requested parameter name
      locateParameter <- which(grepl(parameter.name, samplesID))
      
      # Check if the parameter is hierarchical (contains underscore)
      # Hierarchical parameters have names like "drift_mean", "bound_sdev", etc.
      param.is.hierarchical <- length(which(grepl("_", parameter.name))) != 0
      
      # If not hierarchical, exclude any hierarchical parameters that might have been matched
      # For example, if parameter.name is "drift", exclude "drift_mean" and "drift_sdev"
      if(!param.is.hierarchical){
        # Identify hierarchical parameters
        locateHierPar <- which(grepl("_", samplesID))
        # Exclude hierarchical parameters from the list of parameters to extract
        locateParameter <- locateParameter[!locateParameter %in% locateHierPar]   
      }
      
      # Get the final set of parameter names that will be extracted
      samplesRelated <- samplesID[locateParameter]
      
      # Check if the parameter has two dimensions (contains comma)
      # For example, parameters like "beta[1,2]" have two dimensions
      param.has.twoDim <- length(which(grepl(",", samplesRelated))) != 0
      
      # If two-dimensional, sort the parameters by their indices
      # This ensures parameters are returned in the correct order (e.g., [1,1], [1,2], [2,1], [2,2])
      if(param.has.twoDim){
          # Extract numeric indices from parameter names
          indices <- as.numeric(gsub("\\D", "", samplesRelated))          
          # Create ordering based on indices
          ordered <- order(indices)          
          # Reorder the parameter locations
          locateParameter <- locateParameter[ordered]
      }
      
      # Extract the samples for the identified parameters
      # This creates a subset of the original array containing only the requested parameters
      x <- postParam.Array[, , locateParameter]
      
      # Return the extracted samples
      return(x)
}