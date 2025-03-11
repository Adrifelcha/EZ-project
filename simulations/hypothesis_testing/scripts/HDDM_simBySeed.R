HDDM_simBySeed_fixEff <- function(seed, settings, forceRun=FALSE, redo_if_bad_rhat=TRUE, rhat_cutoff=1.05){
  set.seed(seed)
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  
  fileName <- paste(settings$output.folder,"seed-",seed,".RData",sep="")
  if(file.exists(fileName)&!forceRun){
    stop("Seed already run")
  }
  ########## Print a file to indicate the start of a new seed ###############
  write('Seed has been initiated', paste(settings$output.folder,"seed-",seed,"_start.txt",sep=""))
  
  out_NoDiff <- list()
  out_Diff <- list()
  cell <- 0
  redo_JAGS <- 0
  redo_Rhat <- 0
  nParticipants <- settings$nParticipants
  nTrialsPerCondition <- settings$nTrialsPerCondition
  X <- settings$X
  P <- settings$P
  for(b in settings$beta_levels){
        tic0 <- clock::date_now(zone="UTC")
        rhat_not_verified <- TRUE
        this.seed <- seed
        
        while(rhat_not_verified){
            set.seed(this.seed)
            parameter_set <- sample_parameters(priors = settings$priors, nPart = nParticipants, X = X, Show=FALSE, betaweight = b)
            # Generate and prepare data
            rawData = sample_data(nPart = nParticipants, nTrialsPerCondition = nTrialsPerCondition, parameter_set = parameter_set)
            summData = getStatistics(rawData)
            correct <- summData[,"sum_correct"]
            varRT   <- summData[,"varRT"]
            meanRT  <- summData[,"meanRT"]
            
            # Run JAGS model and get samples
            tic <- clock::date_now(zone="UTC")
            z <- try(samples <- jags(data=settings$jagsData, 
                                   parameters.to.save=settings$jagsParameters, 
                                   model=settings$modelFile, 
                                   n.chains=settings$n.chains, 
                                   n.iter=2000, 
                                   n.burnin=500, 
                                   n.thin=3, 
                                   DIC=T,
                                   inits=settings$jagsInits))
            
            if(inherits(z, "try-error")){ 
                cat("Repeating beta level", b, "due to a JAGS error \n")
                this.seed <- this.seed + 0.01
                redo_JAGS <- redo_JAGS + 1
                if(redo_JAGS > 5){ 
                    warning(paste("Failed to converge after", redo_JAGS, "attempts for beta =", b))
                    break 
                }
                next
            }
            
            toc <- clock::date_now(zone="UTC")
            clock <- toc-tic
            object <- samples$BUGSoutput$sims.array
            
            # Check Rhats before proceeding with parameter extraction
            rhats <- apply(object, 3, Rhat)
            count_bad_rhats <- sum(rhats > rhat_cutoff)
            
            if((!redo_if_bad_rhat) || (count_bad_rhats == 0)){ 
                rhat_not_verified <- FALSE
            } else {
                cat("Repeating beta level", b, "due to bad Rhats \n")
                this.seed <- this.seed + 0.01     
                redo_Rhat <- redo_Rhat + 1
                next
            }
            
            # Extract parameters only if Rhats are acceptable
            estimates <- c(); errors <- c(); CI_low <- c(); CI_up <- c() 
            trueVal <- c(); paramNames <- c()
            for(i in settings$jagsParameters){
                posteriorParameters <- extractSamples(i, samples)
                if(length(dim(posteriorParameters))==3){
                    estimates     <- c(estimates, apply(posteriorParameters,3,mean))
                    errors       <- c(errors,apply(posteriorParameters,3,sd))
                    CI_low      <- c(CI_low, apply(posteriorParameters,3, quantile, probs=0.025))
                    CI_up       <- c(CI_up, apply(posteriorParameters,3, quantile, probs=0.975))
                    paramNames  <- c(paramNames,dimnames(posteriorParameters)[[3]])
                }else{
                    estimates    <- c(estimates,mean(posteriorParameters))
                    errors      <- c(errors,sd(posteriorParameters))
                    CI_low     <- c(CI_low, quantile(posteriorParameters,probs = c(0.025)))
                    CI_up      <- c(CI_up, quantile(posteriorParameters,probs = c(0.975)))
                    paramNames <- c(paramNames, i)
                }
                trueVal  <- c(trueVal, parameter_set[[i]])
            }
            
            toc0 <- clock::date_now(zone="UTC")
            if(b==0){
                out_NoDiff <- rbind(out_NoDiff,list(seed = this.seed, beta = b, rhats = rhats, true.values = trueVal, 
                                  mean.estimates = estimates, std.estimates  = errors, CI = cbind(CI_low,CI_up),
                                  jags.time = clock, total.time = toc0-tic0, beta_chains = extractSamples("betaweight", samples)))
            }else{
                out_Diff <- rbind(out_Diff,list(seed = this.seed, beta = b, rhats = rhats, true.values = trueVal, 
                                mean.estimates = estimates, std.estimates  = errors, jags.time = clock, total.time = toc0-tic0,
                                beta_chains = extractSamples("betaweight", samples)))
            }
            cell <- 1 + cell
            cat("Seed:", seed, "| Beta level", cell, "of",  settings$nCells,"\n")
        }
  }
  ########## Print a file to indicate the end of a the seed ###############
  write('Seed has ended running', paste(settings$output.folder,"seed-",seed,"_end.txt",sep=""))
  resultado <- list("noDiff" = out_NoDiff,
                   "Diff" = out_Diff, 
                   "settings" = settings,
                   "reps" = data.frame("bad_JAGS" = redo_JAGS, "bad_Rhat" = redo_Rhat))
  save(resultado, file=paste(settings$output.folder,"seed-",seed,".RData",sep=""))  
  return(resultado)
}
  
#x <- HDDM_simBySeed_fixEff(1,settings, forceRun=TRUE)