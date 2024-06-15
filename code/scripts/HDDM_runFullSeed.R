HDDM_runFullSeed <- function(seed, settings){
  set.seed(seed)
  suppressMessages(library(R2jags))
  suppressMessages(library(rstan))
  
  ########## Print a file to indicate the start of a new seed ###############
  write('Seed has been initiated', paste(settings$output.folder,"seed-",seed,"_start.txt",sep=""))
  
  out_H <- list()
  out_Beta <- list()
  cell <- 0
  for(d in settings$design_levels){
      
      for(p in settings$participant_levels){
          X <- cbind(rep(NA,p),(0:(p-1))%%2,(0:(p-1))/p)
          colnames(X) <- settings$design_levels
          
          for(t in settings$trial_levels){
            
              if(d=="hierarchical"){
                design <- HDDM_setup(settings$priors[[d]], p, t, d, X[,d], criterion = NA, settings$fromPrior, Show = FALSE)
                runJags <- HDDM_runJAGS(summaryData = design$sumData, t, X[,d], settings$jagsData[[d]], 
                                        settings$jagsParameters[[d]], settings$jagsInits[[as.character(p)]],  
                                        settings$n.chains, settings$modelFile[d,1], Show = FALSE)
                out_H <- rbind(out_H,list(seed = seed, p = p, t = t, d = d, rhats = runJags$rhats,
                               true.values    = design$parameter_set, mean.estimates = runJags$estimates,
                               std.estimates  = runJags$estimates, elapsed.time   = runJags$clock))
                cell <- 1 + cell
                cat("Iteration", cell, "of",  settings$nCells,"\n")
              }else{
                    for(c in settings$criterion_levels){
                        design <- HDDM_setup(settings$priors[[d]], p, t, d, X[,d], c, settings$fromPrior, Show = FALSE)
                        runJags <- HDDM_runJAGS(summaryData = design$sumData, t, X[,d], settings$jagsData[[d]], 
                                                settings$jagsParameters[[d]], settings$jagsInits[[as.character(p)]],
                                                settings$n.chains, settings$modelFile[d,c], Show = FALSE)
                        out_Beta <- rbind(out_Beta,list(seed = seed, p = p, t = t, d = d, c = c, rhats = runJags$rhats,
                                          true.values    = design$parameter_set, mean.estimates = runJags$estimates,
                                          std.estimates  = runJags$estimates, elapsed.time   = runJags$clock))
                        cell <- 1 + cell
                        cat("Iteration", cell, "of",  settings$nCells,"\n")
                    }
              }
          }
      }
  }
  ########## Print a file to indicate the end of a the seed ###############
  write('Seed has ended running', paste(settings$output.folder,"seed-",seed,"_end.txt",sep=""))
  output <- list("hierarchical" = out_H,"betaEffect" = out_Beta)
  save(output, file=paste(settings$output.folder,"seed-",seed,".RData",sep=""))  
  return(output)
}
  