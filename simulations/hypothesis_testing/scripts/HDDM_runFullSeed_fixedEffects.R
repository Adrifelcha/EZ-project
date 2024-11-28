HDDM_runFullSeed_fixEff <- function(seed, settings){
  set.seed(seed)
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  
  ########## Print a file to indicate the start of a new seed ###############
  write('Seed has been initiated', paste(settings$output.folder,"seed-",seed,"_start.txt",sep=""))
  
  out_NoDiff <- list()
  out_Diff <- list()
  cell <- 0
  
  for(b in settings$beta_levels){
      for(p in settings$participant_levels){
              X <- rep(c(1,0),p)
              P <- rep(1:p, each=2)
              for(t in settings$trial_levels){
                    parameter_set <- sample_parameters(priors = settings$priors, nPart = p, modelType = "ttest", 
                                                       X = X, criterion = NA, fromPrior = settings$fromPrior, Show=FALSE, 
                                                       fixedBeta = b)
                    # Generate and prepare data
                    rawData = sample_data(p, nTrials = NA, parameter_set, t)
                    summData = getStatistics(rawData)
                    correct <- summData[,"sum_correct"]
                    varRT   <- summData[,"varRT"]
                    meanRT  <- summData[,"meanRT"]
                    
                    # Run JAGS model and get samples
                    tic <- clock::date_now(zone="UTC")
                    suppressMessages(samples <- jags(data=settings$jagsData, 
                                                     parameters.to.save=settings$jagsParameters, 
                                                     model=settings$modelFile, 
                                                     n.chains=settings$n.chains, 
                                                     n.iter=500, 
                                                     n.burnin=100, 
                                                     n.thin=1, 
                                                     DIC=T, 
                                                     inits=settings$jagsInits[[as.character(p)]]))
                    toc <- clock::date_now(zone="UTC")
                    if(showChains[k]){  plot_Chain(samples) }
                    clock <- as.numeric(toc-tic, units="secs")  # Record time
                    object <- samples$BUGSoutput$sims.array
                    
                    estimates <- c(); errors <- c(); CI_low <- c(); CI_up <- c() 
                    trueVal <- c(); rhats <- c(); paramNames <- c()
                    for(i in jagsParameters){
                        posteriorParameters <- extractSamples(i, samples)
                        if(length(dim(posteriorParameters))==3){
                            estimates     <- c(estimates, apply(posteriorParameters,3,mean))
                            errors        <- c(errors,apply(posteriorParameters,3,sd))
                            CI_low <- c(CI_low, apply(posteriorParameters,3, quantile, probs=0.025))
                            CI_up  <- c(CI_up, pply(posteriorParameters,3, quantile, probs=0.975))
                            paramNames       <- c(paramNames,dimnames(posteriorParameters)[[3]])
                        }else{
                            estimates     <- c(estimates,mean(posteriorParameters))
                            errors        <- c(errors,sd(posteriorParameters))
                            CI_low <- c(CI_low, quantile(posteriorParameters,probs = c(0.025)))
                            CI_up  <- c(CI_up, quantile(posteriorParameters,probs = c(0.975)))
                            paramNames       <- c(paramNames, i)
                        }
                      trueVal  <- c(trueVal, parameter_set[[i]])
                    }
                    rhats <- c(rhats, apply(object,3,Rhat))
                    
                    if(b==0){
                              out_NoDiff <- rbind(out_NoDiff,list(seed = seed, p = p, t = t, d = d, rhats = rhats,
                                             true.values = trueVal, mean.estimates = estimates, std.estimates  = errors, 
                                             elapsed.time   = clock))
                              cell <- 1 + cell
                              cat("Iteration", cell, "of",  settings$nCells,"\n")
                    }else{
                              out_Diff <- rbind(out_Diff,list(seed = seed, p = p, t = t, d = d, rhats = rhats,
                                                true.values = trueVal, mean.estimates = estimates, std.estimates  = errors, 
                                                elapsed.time   = clock))
                              cell <- 1 + cell
                              cat("Iteration", cell, "of",  settings$nCells,"\n")
                    }
              }
        }
  }
  ########## Print a file to indicate the end of a the seed ###############
  write('Seed has ended running', paste(settings$output.folder,"seed-",seed,"_end.txt",sep=""))
  resultado <- list("noDiff" = out_NoDiff,"Effect" = out_Diff)
  save(resultado, file=paste(settings$output.folder,"seed-",seed,".RData",sep=""))  
  return(resultado)
}
  