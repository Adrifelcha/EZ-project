nParticipants <- 10
nTrials <- 300
nDatasets <- 10
x <- HDDM_setup(nParticipants,nTrials,nDatasets)
n.chains <- 4
Show <- TRUE
modelFile="./EZHBDDM.bug"
# A function to run JAGS model
summaryData_page <- x$sumData[,,1]
settings <- x$settings
jagsData <- x$jagsData
jagsParameters <- x$jagsParameters
jagsInits <- x$jagsInits
y <- HDDM_runJAGS(summaryData_page, settings, jagsData, jagsParameters, jagsInits)



HDDM_runSims <- function(nParticipants, nTrials, nDatasets = 10, n.chains = 4, Show=TRUE, forceSim = FALSE){
  outputFile <- nameOutput(nTrials, nParticipants, nDatasets)
  
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  jagsInits    <- default_inits(n.chains, nParticipants)

  design <- HDDM_setup(nParticipants,nTrials,nDatasets, priors=NA, Show=Show)
  paramNames <- design$jagsParameters
  nParams <- sum(lengths(design$parameter_set))
  
  estimates <- matrix(NA, nrow=nDatasets, ncol=nParams)
  credIntervals <- array(NA, dim=c(nDatasets,nParams,2))
  for(k in 1:nDatasets){
      set.seed(k)
      cat("Iteration", k, "of", nDatasets,"\n")
      runJags <- HDDM_runJAGS(summaryData_page = design$sumData[,,k], design$settings, 
                              design$jagsData, design$jagsParameters, design$jagsInits, 
                              n.chains, modelFile="./EZHBDDM.bug", Show)  
      estimates <- runJags$estimates
  }
  
  return(list("values" = list("trueValues" = parameter_set, "estValues" = estimates, "error" = error),
              "rhats"  = runJags$rhats))
}



  runSim <- TRUE
  
  design.parameters <- Hddm_Parameter_Set(nParticipants,nTrials)
  parameter_set <- design.parameters$parameter_set
  nPar <- sum(lapply(parameter_set, length) > 1)
  no.Rhats  <- (nParticipants*nPar)+(length(parameter_set)-nPar)+1
  
  if((!forceSim)&(file.exists(outputFile))){runSim <- FALSE}
  if(runSim){
    # Empty arrays to store hierarchical and individual parameters
    # Pages = Parameter | Rows = Point values | Cols = True/Estimated/Error
    indiv_init = 1
    for(k in 1:nSim){
      set.seed(k)
      cat("Iteration", k, "of", nSim,"\n")
      tryCatch({
        runSim <- Hddm_runSim(nParticipants = nParticipants, nTrials = nTrials, Show = FALSE)
        sim <- runSim$values
        rhats <- runSim$rhats
        sim_rhats[k,]   <- runSim$rhats
        sim_means[k,,1] <- c(sim$trueValues$drift_mean, sim$estValues$drift_mean, sim$error$drift_mean)
        sim_means[k,,2] <- c(sim$trueValues$bound_mean, sim$estValues$bound_mean, sim$error$bound_mean)
        sim_means[k,,3] <- c(sim$trueValues$nondt_mean, sim$estValues$nondt_mean, sim$error$nondt_mean)
        Last = nParticipants*k
        sim_indiv[indiv_init:Last,,1] = c(sim$trueValues$drift, sim$estValues$drift, sim$error$drift)
        sim_indiv[indiv_init:Last,,2] = c(sim$trueValues$bound, sim$estValues$bound, sim$error$bound)
        sim_indiv[indiv_init:Last,,3] = c(sim$trueValues$nondt, sim$estValues$nondt, sim$error$nondt)
        indiv_init = Last + 1        
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    sim_means <- sim_means[which(!is.na(sim_means[,1,1])),,]
    sim_indiv <- sim_indiv[which(!is.na(sim_indiv[,1,1])),,]
    dimnames(sim_means) <- list(NULL, c("true","est","error"), c("drift_mean","bound_mean","nondt_mean"))
    dimnames(sim_indiv) <- list(NULL, c("true","est","error"), c("drift","bound","nondt"))
    colnames(sim_rhats) <- names(rhats)
    simOutput = list("sim_means" = sim_means, "sim_indiv" = sim_indiv, "sim_rhats" = sim_rhats)
    save(simOutput,file=outputFile)
  }else{    load(outputFile)     }
  return(simOutput)
}