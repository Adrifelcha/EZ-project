x <- HDDM_setup(10,300)

# A function to run JAGS model
summaryData_page <- x$sumData[,,1]

Hddm_runJAGS <- function(summaryData_page, jagsData, settings, n.chains=4, modelFile="./EZHBDDM.bug", plot.Chains = FALSE){
  # Load settings
  parameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                  "drift_sdev", "nondt_sdev", "bound_sdev", "drift")
  myinits    <- default_inits(n.chains, settings$nPart)
  data <- jagsData
  # Prepare data
  sub     <- summaryData_page[,"sub"]
  correct <- summaryData_page[,"sum_correct"]
  varRT   <- summaryData_page[,"varRT"]
  meanRT  <- summaryData_page[,"meanRT"]
  nTrialsPerPerson <- settings$nTrials
  nParticipants    <- settings$nPart
  # Run model and get samples
  suppressMessages(samples <- jags(data=data, 
                                   parameters.to.save=parameters, 
                                   model=modelFile, 
                                   n.chains=n.chains, 
                                   n.iter=4000, 
                                   n.burnin=200, 
                                   n.thin=1, 
                                   DIC=T, 
                                   inits=myinits))
  object <- samples$BUGSoutput$sims.array
  rhats <- apply(object,3,Rhat)
  if(plot.Chains){  
                      plot_Chain(samples)   
                      check_Rhat(rhats)
  }
  
  return(list("estimates" = list("drift" = apply(extractSamples("drift", samples),3,mean), 
                                 "drift_mean" = mean(extractSamples("drift_mean", samples)),
                                 "drift_sdev" = mean(extractSamples("drift_sdev", samples)),  
                                 "bound" = apply(extractSamples("bound", samples),3,mean), 
                                 "bound_mean" = mean(extractSamples("bound_mean", samples)),
                                 "bound_sdev" = mean(extractSamples("bound_sdev", samples)),
                                 "nondt" = apply(extractSamples("nondt", samples),3,mean), 
                                 "nondt_mean" = mean(extractSamples("nondt_mean", samples)),
                                 "nondt_sdev" = mean(extractSamples("nondt_sdev", samples))),
              "rhats" = Rhats))
}