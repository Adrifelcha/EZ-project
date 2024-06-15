HDDM_runJAGS <- function(summaryData, nTrials, X, jagsData, jagsParameters, jagsInits, 
                         n.chains=4, modelFile="./EZHBDDM.bug", Show = TRUE){
  # Prepare data
  sub     <- summaryData[,"sub"]
  correct <- summaryData[,"sum_correct"]
  varRT   <- summaryData[,"varRT"]
  meanRT  <- summaryData[,"meanRT"]
  nTrialsPerPerson <- nTrials
  nParticipants    <- nrow(summaryData)
  # Run model and get samples
  tic <- clock::date_now(zone="UTC")
  suppressMessages(samples <- jags(data=jagsData, 
                                   parameters.to.save=jagsParameters, 
                                   model=modelFile, 
                                   n.chains=n.chains, 
                                   n.iter=500, 
                                   n.burnin=100, 
                                   n.thin=1, 
                                   DIC=T, 
                                   inits=jagsInits))
  toc <- clock::date_now(zone="UTC")
  clock <- as.numeric(toc-tic, units="secs")  # Record time
  object <- samples$BUGSoutput$sims.array
  rhats <- apply(object,3,Rhat)
  if(Show){  
    plot_Chain(samples)   
  }
  
  estimates <- list()
  credInterval <- list()
  for(i in 1:length(jagsParameters)){
      posteriorParameters <- extractSamples(jagsParameters[i], samples)
         if(length(dim(posteriorParameters))==3){
            meanPost    <- apply(posteriorParameters,3,mean)
            percentiles <- apply(posteriorParameters,3, quantile, probs=c(0.025,0.975))
         }else{
            meanPost    <- mean(posteriorParameters)
            percentiles <- quantile(posteriorParameters,probs = c(0.025,0.975))
         }
     estimates   <- c(estimates, list(meanPost))
     credInterval <- c(credInterval, list(percentiles))
  }
  names(estimates)   <- jagsParameters
  names(credInterval) <- jagsParameters
  
  return(list("estimates" = estimates, "credInterval" = credInterval,
              "rhats" = rhats, "clock" = clock))
}