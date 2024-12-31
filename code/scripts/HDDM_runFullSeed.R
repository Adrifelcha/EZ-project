HDDM_runFullSeed <- function(seed, settings,forceRun, redo_if_bad_rhat=FALSE, rhat_cutoff=NA){
  grand_tic <- clock::date_now(zone="UTC")
  set.seed(seed)
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  
  fileName <- paste(settings$output.folder,"seed-",seed,".RData",sep="")
  if(file.exists(fileName)&!forceRun){
     stop("Seed already run")
  }
  
  if(redo_if_bad_rhat&is.na(rhat_cutoff)){
    rhat_cutoff <- 1.05
  }
  
  ########## Print a file to indicate the start of a new seed ###############
  write('Seed has been initiated', paste(settings$output.folder,"seed-",seed,"_start.txt",sep=""))
  
  out_Beta <- list()
  cell <- 0
  redo_JAGS <- 0
  redo_Rhat <- 0
  for(d in settings$design_levels){
      
      for(p in settings$participant_levels){
          X <- cbind((0:(p-1))%%2,(0:(p-1))/p)
          colnames(X) <- settings$design_levels
          
          for(t in settings$trial_levels){
            
                for(c in settings$criterion_levels){
                    rhat_not_verified <- TRUE
                    this.seed <- seed
                    cat("Running cell", cell, "of", settings$nCells, "\n")
                    while(rhat_not_verified){
                        set.seed(this.seed)
                        design <- HDDM_setup(settings$priors[[d]], p, t, d, X[,d], c, settings$fromPrior, Show = FALSE)
                        z <- try(runJags <- HDDM_runJAGS(summaryData = design$sumData, nTrials = t, X = X[,d], 
                                                         jagsData = settings$jagsData[[d]], jagsParameters = settings$jagsParameters[[d]], 
                                                         jagsInits = settings$jagsInits[[as.character(p)]], n.chains = settings$n.chains, 
                                                         n.burnin = settings$n.burnin, n.iter = settings$n.iter, n.thin = settings$n.thin, 
                                                         modelFile = settings$modelFile[d,c], Show = FALSE, track_allParameters = FALSE))
                        if(inherits(z, "try-error")){ 
                              cat("Repeating cell", cell, "of", settings$nCells, "due to a JAGS error \n")
                              this.seed <- this.seed+.01
                              set.seed(this.seed)
                              design <- HDDM_setup(settings$priors[[d]], p, t, d, X[,d], c, settings$fromPrior, Show = FALSE)
                              z <- try(runJags <- HDDM_runJAGS(summaryData = design$sumData, nTrials = t, X = X[,d], 
                                                               jagsData = settings$jagsData[[d]], jagsParameters = settings$jagsParameters[[d]], 
                                                               jagsInits = settings$jagsInits[[as.character(p)]], n.chains = settings$n.chains, 
                                                               n.burnin = settings$n.burnin, n.iter = settings$n.iter, n.thin = settings$n.thin, 
                                                               modelFile = settings$modelFile[d,c], Show = FALSE, track_allParameters = FALSE))
                              redo_JAGS <- redo_JAGS + 1
                              if(redo_JAGS>5){ break  }
                        }
                        count_bad_rhats <- sum(runJags$rhats[jagsParameters]>rhat_cutoff)
                        if((!redo_if_bad_rhat)|(count_bad_rhats==0)){ 
                                  rhat_not_verified <-  FALSE
                        }else{ cat("Repeating cell", cell, "of", settings$nCells, "due to bad Rhats \n")
                               this.seed <- this.seed+.01     
                               redo_Rhat <- redo_Rhat + 1}
                    } # Close while()
                    out_Beta <- rbind(out_Beta,list(seed = this.seed, p = p, t = t, d = d, c = c, rhats = runJags$rhats,
                                                  true.values    = design$parameter_set, mean.estimates = runJags$estimates,
                                                  std.estimates  = runJags$estimates, elapsed.time   = runJags$clock))
                    cell <- 1 + cell
                }
          }
      }
  }
  ########## Print a file to indicate the end of a the seed ###############
  grand_toc <- clock::date_now(zone="UTC")
  total_time <- difftime(grand_toc, grand_tic, units="mins")
  write(paste("Running this seed took ", total_time, "minutes.\n"), paste(settings$output.folder,"seed-",seed,"_end.txt",sep=""))
  output <- list("betaEffect" = out_Beta, 
                 "reps" = data.frame("bad_JAGS" = redo_JAGS, "bad_Rhat" = redo_Rhat))
  save(output, file=fileName)  
  return(output)
}
  