# A function to extract individual samples for any parameter
extractSamples <- function(parameter.name, samples){
      postParam.Array <- samples$BUGSoutput$sims.array
      samplesID <- names(postParam.Array[1,1,])
      locateParameter  <- which(grepl(parameter.name,samplesID))
      
      param.is.hierarchical <- length(which(grepl("_",parameter.name)))!=0
      if(!param.is.hierarchical){
        locateHierPar <- which(grepl("_",samplesID))
        locateParameter <- locateParameter[!locateParameter %in% locateHierPar]   
      }
      
      # Now, for individual/task level parameters...
      if(length(locateParameter)>1){
        # We isolate the pages ID that contain the parameter of interest
        samplesRelated <- samplesID[locateParameter]
        # We identify the maximum index
        locateParticipantID <-as.numeric(gsub("\\D", "", samplesRelated))
        nP <- max(locateParticipantID,na.rm = TRUE)
        # We locate the page ID containing the last index
        lastP <- which(grepl(nP,samplesRelated))
        # And isolate all IDs from there
        locateParameter <- samplesRelated[(lastP-nP)+1:lastP]
      }
      # We retrieve only the pages containing the parameter
      x <- postParam.Array[,,locateParameter]
return(x)
}