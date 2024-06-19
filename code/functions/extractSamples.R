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
      
      samplesRelated <- samplesID[locateParameter]
      param.has.twoDim <- length(which(grepl(",",samplesRelated)))!=0
      if(param.has.twoDim){
          indices <-as.numeric(gsub("\\D", "", samplesRelated))
          ordered <- order(indices)
          locateParameter <- locateParameter[ordered]
      }
      
      # We retrieve only the pages containing the parameter
      x <- postParam.Array[,,locateParameter]
return(x)
}