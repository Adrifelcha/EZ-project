store_parallelOutput <- function(output, settings, saveTo = "./"){
  #################################################################################
  # Identify relevant properties of the simulation study
  #################################################################################   
   nCells               <- settings$nCells
   output.folder        <- settings$output.folder
   allP   <- settings$participant_levels
   allT   <- settings$trial_levels
   allD   <- settings$design_levels
   allC   <- settings$criterion_levels
   nDatasets  <- nrow(output)
   
   # Is a simple Hierarchial study included?
   Hierarchical <- "hierarchical" %in% settings$design_levels
   # Is a Beta-effect design included?
   BetaEffect <- "ttest" %in% settings$design_levels | "metaregression" %in% settings$design_levels
   if(Hierarchical){   outH <- output[,"hierarchical"]    }
   if(BetaEffect){     outB <- output[,"betaEffect"]      }
   Ttest <- "ttest" %in% settings$design_levels
   Meta <- "metaregression" %in% settings$design_levels
   
   ################################################################################
   # Empty lists to story P-specific arrays with T-specific pages an nDataset rows
   ################################################################################
   if(Hierarchical){      trueVals_Hier <- list()                 
                          meanPosts_Hier <- list()
                          sdevPosts_Hier <- list()
                          rhats_Hier <- list()                    }
   if(Ttest){             trueVals_Ttst_nondt <- list()           
                          trueVals_Ttst_drift <- list()
                          trueVals_Ttst_bound <- list()
                          meanPosts_Ttst_nondt <- list()           
                          meanPosts_Ttst_drift <- list()
                          meanPosts_Ttst_bound <- list()
                          sdevPosts_Ttst_nondt <- list()
                          sdevPosts_Ttst_drift <- list()
                          sdevPosts_Ttst_bound <- list()
                          rhats_Ttst_nondt <- list()
                          rhats_Ttst_drift <- list()
                          rhats_Ttst_bound <- list()               }
   if(Meta){              trueVals_Meta_nondt <- list()
                          trueVals_Meta_drift <- list()    
                          trueVals_Meta_bound <- list()
                          meanPosts_Meta_nondt <- list()
                          meanPosts_Meta_drift <- list()    
                          meanPosts_Meta_bound <- list()
                          sdevPosts_Meta_nondt <- list()
                          sdevPosts_Meta_drift <- list()
                          sdevPosts_Meta_bound <- list()
                          rhats_Meta_nondt <- list()
                          rhats_Meta_drift <- list()   
                          rhats_Meta_bound <- list()               }
   # Store running times (in seconds)
   clock_base <- array(NA, dim=c(nDatasets,length(allT),length(allP)), 
                       dimnames = list(paste("seed", 1:nDatasets), paste("T", allT, sep=""), paste("P", allP,sep="")))
   if(Hierarchical){     clock_Hier <- clock_base                   }
   if(Ttest){ clock_Ttst <- list("bound" = clock_base, "drift" = clock_base, "nondt" = clock_base)}
   if(Meta){  clock_Meta <- list("bound" = clock_base, "drift" = clock_base, "nondt" = clock_base)}
   
   ###############################################################################################################################
   # Fill the empty storage objects
   ###############################################################################################################################
   i <- 1
   for(p in allP){
       # No. parameters depend on No. of participants
       nParamsH <- 6+(3*p)
       nParamsB <- nParamsH+1
       
       # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # Fill the empty lists created above with empty arrays to store output
       #################################################################################################
       if(Hierarchical){
           # Identify parameter names
           Hnames_true      <- sub('.*true.values.','',names(unlist(outH[[1]][which(outH[[1]][,"p"]==p)[1],"true.values"])))
           Hnames_estimates <- sub(".*\\.", "", sub('.*mean.estimates.','',names(unlist(outH[[1]][which(outH[[1]][,"p"]==p)[1],"mean.estimates"]))))
           Hnames_rhats     <- sub(".*\\.", "", names(unlist(outH[[1]][which(outH[[1]][,"p"]==p)[1],"rhats"])))
           # Create empty arrays to store True values
           emptyObj_Htrue <- array(NA, dim=c(nDatasets,nParamsH,length(allT)), 
                                   dimnames = list(paste("seed", 1:nDatasets), Hnames_true, paste("T",allT,sep="")))
           # Create empty arrays for estimates and errors
           emptyObj_H <- array(NA, dim=c(nDatasets,nParamsH,length(allT)), 
                               dimnames = list(paste("seed", 1:nDatasets), Hnames_estimates, paste("T",allT,sep="")))
           # Create empty arrays for Rhats
           emptyObj_R_H <- array(NA, dim=c(nDatasets,nParamsH+1,length(allT)), 
                                 dimnames = list(paste("seed", 1:nDatasets), Hnames_rhats, paste("T",allT,sep="")))
           # Add these arrays (specific number of columns) to each list
           trueVals_Hier <- c(trueVals_Hier, list(emptyObj_Htrue))
           meanPosts_Hier <- c(meanPosts_Hier, list(emptyObj_H))
           sdevPosts_Hier <- c(sdevPosts_Hier, list(emptyObj_H))
           rhats_Hier <- c(rhats_Hier, list(emptyObj_R_H))
       }
       if(BetaEffect){
           # Identify parameter names
           Bnames_true      <- sub('.*true.values.','',names(unlist(outB[[1]][which(outB[[1]][,"p"]==p)[1],"true.values"])))
           Bnames_estimates <- sub(".*\\.", "", sub('.*mean.estimates.','',names(unlist(outB[[1]][which(outB[[1]][,"p"]==p)[1],"mean.estimates"]))))
           Bnames_rhats     <- sub(".*\\.", "", names(unlist(outB[[1]][which(outB[[1]][,"p"]==p)[1],"rhats"])))
           # Create empty arrays to store True values
           emptyObj_Btrue <- array(NA, dim=c(nDatasets,nParamsB,length(allT)), 
                               dimnames = list(paste("seed", 1:nDatasets), Bnames_true, paste("T",allT,sep="")))
           # Create empty arrays for estimates and errors
           emptyObj_B <- array(NA, dim=c(nDatasets,nParamsB,length(allT)), 
                               dimnames = list(paste("seed", 1:nDatasets), Bnames_estimates, paste("T",allT,sep="")))
           # Create empty arrays for Rhats
           emptyObj_R_B <- array(NA, dim=c(nDatasets,nParamsB+1,length(allT)), 
                                 dimnames = list(paste("seed", 1:nDatasets), Bnames_rhats, paste("T",allT,sep="")))
           # Add these arrays (specific number of columns) to each list
           if(Ttest){
                      trueVals_Ttst_nondt <- c(trueVals_Ttst_nondt, list(emptyObj_Btrue))     # True values
                      trueVals_Ttst_drift <- c(trueVals_Ttst_drift, list(emptyObj_Btrue))
                      trueVals_Ttst_bound <- c(trueVals_Ttst_bound, list(emptyObj_Btrue))
                      meanPosts_Ttst_nondt <- c(meanPosts_Ttst_nondt, list(emptyObj_B))       # Mean Posteriors
                      meanPosts_Ttst_drift <- c(meanPosts_Ttst_drift, list(emptyObj_B))
                      meanPosts_Ttst_bound <- c(meanPosts_Ttst_bound, list(emptyObj_B))
                      sdevPosts_Ttst_nondt <- c(sdevPosts_Ttst_nondt, list(emptyObj_B))       # Posterior variance
                      sdevPosts_Ttst_drift <- c(sdevPosts_Ttst_drift, list(emptyObj_B))
                      sdevPosts_Ttst_bound <- c(sdevPosts_Ttst_bound, list(emptyObj_B))
                      rhats_Ttst_nondt <- c(rhats_Ttst_nondt, list(emptyObj_R_B))             # Rhats
                      rhats_Ttst_drift <- c(rhats_Ttst_drift, list(emptyObj_R_B))
                      rhats_Ttst_bound <- c(rhats_Ttst_bound, list(emptyObj_R_B))
           }
           if(Meta){
                       trueVals_Meta_nondt <- c(trueVals_Meta_nondt, list(emptyObj_Btrue))    # True values
                       trueVals_Meta_drift <- c(trueVals_Meta_drift, list(emptyObj_Btrue))
                       trueVals_Meta_bound <- c(trueVals_Meta_bound, list(emptyObj_Btrue))
                       meanPosts_Meta_nondt <- c(meanPosts_Meta_nondt, list(emptyObj_B))      # Mean Posteriors
                       meanPosts_Meta_drift <- c(meanPosts_Meta_drift, list(emptyObj_B))
                       meanPosts_Meta_bound <- c(meanPosts_Meta_bound, list(emptyObj_B))
                       sdevPosts_Meta_nondt <- c(sdevPosts_Meta_nondt, list(emptyObj_B))      # Posterior variance
                       sdevPosts_Meta_drift <- c(sdevPosts_Meta_drift, list(emptyObj_B))
                       sdevPosts_Meta_bound <- c(sdevPosts_Meta_bound, list(emptyObj_B))
                       rhats_Meta_nondt <- c(rhats_Meta_nondt, list(emptyObj_R_B))            # Rhats
                       rhats_Meta_drift <- c(rhats_Meta_drift, list(emptyObj_R_B))
                       rhats_Meta_bound <- c(rhats_Meta_bound, list(emptyObj_R_B))             
           }
       }
       
       # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # Fill the arrays corresponding to each level 'p'
       #################################################################################################
       for(k in 1:nDatasets){
           j <- 1
           for(t in allT){
               if(Hierarchical){
               thisH <- which(outH[[k]][,"p"]==p&outH[[k]][,"t"]==t)
                        trueVals_Hier[[i]][k,,j] <- unlist(outH[[k]][thisH,"true.values"])
                        meanPosts_Hier[[i]][k,,j] <- unlist(outH[[k]][thisH,"mean.estimates"])
                        sdevPosts_Hier[[i]][k,,j] <- unlist(outH[[k]][thisH,"std.estimates"])
                        rhats_Hier[[i]][k,,j] <- unlist(outH[[k]][thisH,"rhats"])
                        clock_Hier[k,j,i] <- as.numeric(outH[[k]][thisH,"elapsed.time"])
               }
               if(Meta){
                       # Meta-regression - Criterion: nondt
                       thisMn <- which(outB[[k]][,"p"]==p&outB[[k]][,"t"]==t&outB[[k]][,"c"]=="nondt"&outB[[k]][,"d"]=="metaregression")
                       if(length(thisMn)>0){
                         trueVals_Meta_nondt[[i]][k,,j] <- unlist(outB[[k]][thisMn,"true.values"])
                         meanPosts_Meta_nondt[[i]][k,,j] <- unlist(outB[[k]][thisMn,"mean.estimates"])
                         sdevPosts_Meta_nondt[[i]][k,,j] <- unlist(outB[[k]][thisMn,"std.estimates"])
                         rhats_Meta_nondt[[i]][k,,j] <- unlist(outB[[k]][thisMn,"rhats"])
                         clock_Meta[["nondt"]][k,j,i] <- as.numeric(outB[[k]][thisMn,"elapsed.time"])
                       }
                       # Meta-regression - Criterion: drift
                       thisMd <- which(outB[[k]][,"p"]==p&outB[[k]][,"t"]==t&outB[[k]][,"c"]=="drift"&outB[[k]][,"d"]=="metaregression")
                       if(length(thisMd)>0){
                         trueVals_Meta_drift[[i]][k,,j] <- unlist(outB[[k]][thisMd,"true.values"])
                         meanPosts_Meta_drift[[i]][k,,j] <- unlist(outB[[k]][thisMd,"mean.estimates"])
                         sdevPosts_Meta_drift[[i]][k,,j] <- unlist(outB[[k]][thisMd,"std.estimates"])
                         rhats_Meta_drift[[i]][k,,j] <- unlist(outB[[k]][thisMd,"rhats"])
                         clock_Meta[["drift"]][k,j,i] <- as.numeric(outB[[k]][thisMd,"elapsed.time"])
                       }
                       # Meta-regression - Criterion: bound
                       thisMb <- which(outB[[k]][,"p"]==p&outB[[k]][,"t"]==t&outB[[k]][,"c"]=="bound"&outB[[k]][,"d"]=="metaregression")
                       if(length(thisMb)>0){
                         trueVals_Meta_bound[[i]][k,,j] <- unlist(outB[[k]][thisMb,"true.values"])
                         meanPosts_Meta_bound[[i]][k,,j] <- unlist(outB[[k]][thisMb,"mean.estimates"])
                         sdevPosts_Meta_bound[[i]][k,,j] <- unlist(outB[[k]][thisMb,"std.estimates"])
                         rhats_Meta_bound[[i]][k,,j] <- unlist(outB[[k]][thisMb,"rhats"])
                         clock_Meta[["bound"]][k,j,i] <- as.numeric(outB[[k]][thisMb,"elapsed.time"])
                       }
               }
               if(Ttest){
                       # ttest - Criterion: nondt
                       thisTn <- which(outB[[k]][,"p"]==p&outB[[k]][,"t"]==t&outB[[k]][,"c"]=="nondt"&outB[[k]][,"d"]=="ttest")
                       if(length(thisTn)>0){
                         trueVals_Ttst_nondt[[i]][k,,j] <- unlist(outB[[k]][thisTn,"true.values"])
                         meanPosts_Ttst_nondt[[i]][k,,j] <- unlist(outB[[k]][thisTn,"mean.estimates"])
                         sdevPosts_Ttst_nondt[[i]][k,,j] <- unlist(outB[[k]][thisTn,"std.estimates"])
                         rhats_Ttst_nondt[[i]][k,,j] <- unlist(outB[[k]][thisTn,"rhats"])
                         clock_Ttst[["nondt"]][k,j,i] <- as.numeric(outB[[k]][thisTn,"elapsed.time"])
                       }
                       # ttest - Criterion: drift
                       thisTd <- which(outB[[k]][,"p"]==p&outB[[k]][,"t"]==t&outB[[k]][,"c"]=="drift"&outB[[k]][,"d"]=="ttest")
                       if(length(thisTd)>0){
                         trueVals_Ttst_drift[[i]][k,,j] <- unlist(outB[[k]][thisTd,"true.values"])
                         meanPosts_Ttst_drift[[i]][k,,j] <- unlist(outB[[k]][thisTd,"mean.estimates"])
                         sdevPosts_Ttst_drift[[i]][k,,j] <- unlist(outB[[k]][thisTd,"std.estimates"])
                         rhats_Ttst_drift[[i]][k,,j] <- unlist(outB[[k]][thisTd,"rhats"])
                         clock_Ttst[["drift"]][k,j,i] <- as.numeric(outB[[k]][thisTd,"elapsed.time"])
                       }
                       # ttest - Criterion: bound
                       thisTb <- which(outB[[k]][,"p"]==p&outB[[k]][,"t"]==t&outB[[k]][,"c"]=="bound"&outB[[k]][,"d"]=="ttest")
                       if(length(thisTb)>0){
                         trueVals_Ttst_bound[[i]][k,,j] <- unlist(outB[[k]][thisTb,"true.values"])
                         meanPosts_Ttst_bound[[i]][k,,j] <- unlist(outB[[k]][thisTb,"mean.estimates"])
                         sdevPosts_Ttst_bound[[i]][k,,j] <- unlist(outB[[k]][thisTb,"std.estimates"])
                         rhats_Ttst_bound[[i]][k,,j] <- unlist(outB[[k]][thisTb,"rhats"])
                         clock_Ttst[["bound"]][k,j,i] <- as.numeric(outB[[k]][thisTb,"elapsed.time"])
                       }
                }
           j <- j+1 
           }
       }
       i <- i + 1
   }

   ################################################################################
   # Name each array by the number-of-participants included
   ################################################################################
   if(Hierarchical){
                      names(trueVals_Hier) <- paste("P",allP,sep="") 
                      names(meanPosts_Hier) <- paste("P",allP,sep="")
                      names(sdevPosts_Hier) <- paste("P",allP,sep="")
                      names(rhats_Hier) <- paste("P",allP,sep="")
   }
   if(Ttest){
                       names(trueVals_Ttst_nondt) <- paste("P",allP,sep="")     # True values
                       names(trueVals_Ttst_drift) <- paste("P",allP,sep="")
                       names(trueVals_Ttst_bound) <- paste("P",allP,sep="")
                       names(meanPosts_Ttst_nondt) <- paste("P",allP,sep="")    # Mean Posteriors
                       names(meanPosts_Ttst_drift) <- paste("P",allP,sep="")
                       names(meanPosts_Ttst_bound) <- paste("P",allP,sep="")
                       names(sdevPosts_Ttst_nondt) <- paste("P",allP,sep="")    # Posterior variance
                       names(sdevPosts_Ttst_drift) <- paste("P",allP,sep="")
                       names(sdevPosts_Ttst_bound) <- paste("P",allP,sep="")
                       names(rhats_Ttst_nondt) <- paste("P",allP,sep="")        # Rhats
                       names(rhats_Ttst_drift) <- paste("P",allP,sep="")
                       names(rhats_Ttst_bound) <- paste("P",allP,sep="")
   }
   if(Meta){
                       names(trueVals_Meta_nondt) <- paste("P",allP,sep="")     # True values
                       names(trueVals_Meta_drift) <- paste("P",allP,sep="")
                       names(trueVals_Meta_bound) <- paste("P",allP,sep="")
                       names(meanPosts_Meta_nondt) <- paste("P",allP,sep="")    # Mean Posteriors
                       names(meanPosts_Meta_drift) <- paste("P",allP,sep="")   
                       names(meanPosts_Meta_bound) <- paste("P",allP,sep="")
                       names(sdevPosts_Meta_nondt) <- paste("P",allP,sep="")    # Posterior variance
                       names(sdevPosts_Meta_drift) <- paste("P",allP,sep="")
                       names(sdevPosts_Meta_bound) <- paste("P",allP,sep="")
                       names(rhats_Meta_nondt) <- paste("P",allP,sep="")        # Rhats
                       names(rhats_Meta_drift) <- paste("P",allP,sep="")
                       names(rhats_Meta_bound) <- paste("P",allP,sep="")
   }
   
   ################################################################################
   # FINAL: Store the output per design-parameter by writing an .RData object
   ################################################################################
   if(Hierarchical){
                     simStudy_Hierarchical <- list("true" = trueVals_Hier,
                                                   "recovered" = meanPosts_Hier,
                                                   "estimates_sdev" = sdevPosts_Hier,
                                                   "rhats" = rhats_Hier)
                     outputFile = paste(saveTo,"/simStudy_Hierarchical.RData",sep="")
                     save(simStudy_Hierarchical, file=outputFile)
   }
   if(Meta){
            if("nondt" %in% settings$criterion_levels){
                     simStudy_Meta_nondt <- list("true" = trueVals_Meta_nondt, 
                                                 "recovered" = meanPosts_Meta_nondt,
                                                 "estimates_sdev" = sdevPosts_Meta_nondt,
                                                 "rhats" = rhats_Meta_nondt)
                     save(simStudy_Meta_nondt, file=paste(saveTo,"/simStudy_Meta_nondt.RData",sep=""))          }
            if("drift" %in% settings$criterion_levels){
                     simStudy_Meta_drift <- list("true" = trueVals_Meta_drift,
                                                 "recovered" = meanPosts_Meta_drift,
                                                 "estimates_sdev" = sdevPosts_Meta_drift,
                                                 "rhats" = rhats_Meta_drift)
                     save(simStudy_Meta_drift, file=paste(saveTo,"/simStudy_Meta_drift.RData",sep=""))           }
            if("bound" %in% settings$criterion_levels){
                     simStudy_Meta_bound <- list("true" = trueVals_Meta_bound,
                                                 "recovered" = meanPosts_Meta_bound,
                                                 "estimates_sdev" = sdevPosts_Meta_bound,
                                                 "rhats" = rhats_Meta_bound)
                     save(simStudy_Meta_bound, file=paste(saveTo,"/simStudy_Meta_bound.RData",sep=""))           }
   }
   if(Ttest){
                     simStudy_Ttst_nondt <- list("true" = trueVals_Ttst_nondt,
                                                 "recovered" = meanPosts_Ttst_nondt,
                                                 "estimates_sdev" = sdevPosts_Ttst_nondt,
                                                 "rhats" = rhats_Ttst_nondt)
                     save(simStudy_Ttst_nondt, file=paste(saveTo,"/simStudy_Ttest_nondt.RData", sep=""))
                     
                     simStudy_Ttest_drift <- list("true" = trueVals_Ttst_drift,
                                                  "recovered" = meanPosts_Ttst_drift,
                                                  "estimates_sdev" = sdevPosts_Ttst_drift,
                                                  "rhats" = rhats_Ttst_drift)
                     save(simStudy_Ttest_drift, file=paste(saveTo,"/simStudy_Ttest_drift.RData",sep=""))
                     
                     simStudy_Ttest_bound <- list("true" = trueVals_Ttst_bound, 
                                                  "recovered" = meanPosts_Ttst_bound,
                                                  "estimates_sdev" = sdevPosts_Ttst_bound,
                                                  "rhats" = rhats_Ttst_bound)
                     save(simStudy_Ttest_bound, file=paste(saveTo,"/simStudy_Ttest_bound.RData",sep=""))
   }
} 