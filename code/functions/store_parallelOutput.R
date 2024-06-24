store_parallelOutput <- function(output, settings){
   outH <- output[,"hierarchical"]
   outB <- output[,"betaEffect"]
   ### R packages
   nDatasets            <- length(outH)
   nCells               <- settings$nCells
   output.folder        <- settings$output.folder
   allP   <- settings$participant_levels
   allT   <- settings$trial_levels
   allD   <- settings$design_levels
   allC   <- settings$criterion_levels
   
   #############################################################################
   # Empty lists to story P-specific arrays with T-specific pages an nDataset rows
   #############################################################################
   # Store true values
   trueVals_Hier <- list()
   trueVals_Meta_nondt <- list();    trueVals_Ttst_nondt <- list()
   trueVals_Meta_drift <- list();    trueVals_Ttst_drift <- list()
   trueVals_Meta_bound <- list();    trueVals_Ttst_bound <- list()
   # Store estimated values (mean posteriors)
   meanPosts_Hier <- list()
   meanPosts_Meta_nondt <- list();    meanPosts_Ttst_nondt <- list()
   meanPosts_Meta_drift <- list();    meanPosts_Ttst_drift <- list()
   meanPosts_Meta_bound <- list();    meanPosts_Ttst_bound <- list()
   # Store posterior variance
   sdevPosts_Hier <- list()
   sdevPosts_Meta_nondt <- list();   sdevPosts_Ttst_nondt <- list()
   sdevPosts_Meta_drift <- list();   sdevPosts_Ttst_drift <- list()
   sdevPosts_Meta_bound <- list();   sdevPosts_Ttst_bound <- list()
   # Store rhats
   rhats_Hier <- list()
   rhats_Meta_nondt <- list();   rhats_Ttst_nondt <- list()
   rhats_Meta_drift <- list();   rhats_Ttst_drift <- list()
   rhats_Meta_bound <- list();   rhats_Ttst_bound <- list()
   # Store running times (in seconds)
   clock_Hier <- array(NA, dim=c(nDatasets,length(allT),length(allP)), 
                       dimnames = list(paste("seed", 1:nDatasets), paste("T", allT, sep=""), paste("P", allP,sep="")))
   clock_Ttst <- list("bound" = clock_Hier, "drift" = clock_Hier, "nondt" = clock_Hier)
   clock_Meta <- list("bound" = clock_Hier, "drift" = clock_Hier, "nondt" = clock_Hier)
   
   i <- 1
   for(p in allP){
       # No. parameters depend on No. of participants
       nParamsH <- 6+(3*p)
       nParamsB <- nParamsH+1
       # Create empty arrays - General
       emptyObj_H <- array(NA, dim=c(nDatasets,nParamsH,length(allT)), 
                           dimnames = list(paste("seed", 1:nDatasets), NA, paste("T",allT,sep="")))
       emptyObj_B <- array(NA, dim=c(nDatasets,nParamsB,length(allT)), 
                           dimnames = list(paste("seed", 1:nDatasets), NA, paste("T",allT,sep="")))
       # Create empty arrays - Rhats include deviance
       emptyObj_R_H <- array(NA, dim=c(nDatasets,nParamsH+1,length(allT)), 
                             dimnames = list(paste("seed", 1:nDatasets), NA, paste("T",allT,sep="")))
       emptyObj_R_B <- array(NA, dim=c(nDatasets,nParamsB+1,length(allT)), 
                             dimnames = list(paste("seed", 1:nDatasets), NA, paste("T",allT,sep="")))
       
       trueVals_Hier <- c(trueVals_Hier, list(emptyObj_H))
       trueVals_Meta_nondt <- c(trueVals_Meta_nondt, list(emptyObj_B))
       trueVals_Ttst_nondt <- c(trueVals_Ttst_nondt, list(emptyObj_B))
       trueVals_Meta_drift <- c(trueVals_Meta_drift, list(emptyObj_B))
       trueVals_Ttst_drift <- c(trueVals_Ttst_drift, list(emptyObj_B))
       trueVals_Meta_bound <- c(trueVals_Meta_bound, list(emptyObj_B))
       trueVals_Ttst_bound <- c(trueVals_Ttst_bound, list(emptyObj_B))
       meanPosts_Hier <- c(meanPosts_Hier, list(emptyObj_H))
       meanPosts_Meta_nondt <- c(meanPosts_Meta_nondt, list(emptyObj_B))
       meanPosts_Ttst_nondt <- c(meanPosts_Ttst_nondt, list(emptyObj_B))
       meanPosts_Meta_drift <- c(meanPosts_Meta_drift, list(emptyObj_B))
       meanPosts_Ttst_drift <- c(meanPosts_Ttst_drift, list(emptyObj_B))
       meanPosts_Meta_bound <- c(meanPosts_Meta_bound, list(emptyObj_B))
       meanPosts_Ttst_bound <- c(meanPosts_Ttst_bound, list(emptyObj_B))
       sdevPosts_Hier <- c(sdevPosts_Hier, list(emptyObj_H))
       sdevPosts_Meta_nondt <- c(sdevPosts_Meta_nondt, list(emptyObj_B))
       sdevPosts_Ttst_nondt <- c(sdevPosts_Ttst_nondt, list(emptyObj_B))
       sdevPosts_Meta_drift <- c(sdevPosts_Meta_drift, list(emptyObj_B))
       sdevPosts_Ttst_drift <- c(sdevPosts_Ttst_drift, list(emptyObj_B))
       sdevPosts_Meta_bound <- c(sdevPosts_Meta_bound, list(emptyObj_B))
       sdevPosts_Ttst_bound <- c(sdevPosts_Ttst_bound, list(emptyObj_B))
       rhats_Hier <- c(rhats_Hier, list(emptyObj_R_H))
       rhats_Meta_nondt <- c(rhats_Meta_nondt, list(emptyObj_R_B))
       rhats_Ttst_nondt <- c(rhats_Ttst_nondt, list(emptyObj_R_B))
       rhats_Meta_drift <- c(rhats_Meta_drift, list(emptyObj_R_B))
       rhats_Ttst_drift <- c(rhats_Ttst_drift, list(emptyObj_R_B))
       rhats_Meta_bound <- c(rhats_Meta_bound, list(emptyObj_R_B))
       rhats_Ttst_bound <- c(rhats_Ttst_bound, list(emptyObj_R_B))
       
       for(k in 1:nDatasets){
           j <- 1
           for(t in allT){
               # Hierarchical design
               thisH <- which(outH[[k]][,"p"]==p&outH[[k]][,"t"]==t)
               if(length(thisH)>0){
                     trueVals_Hier[[i]][k,,j] <- unlist(outH[[k]][thisH,"true.values"])
                     meanPosts_Hier[[i]][k,,j] <- unlist(outH[[k]][thisH,"mean.estimates"])
                     sdevPosts_Hier[[i]][k,,j] <- unlist(outH[[k]][thisH,"std.estimates"])
                     rhats_Hier[[i]][k,,j] <- unlist(outH[[k]][thisH,"rhats"])
                     clock_Hier[k,j,i] <- as.numeric(outH[[k]][thisH,"elapsed.time"])
               }
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
           j <- j+1 
           }
       }
       i <- i + 1
   }

} 


#   c <- 0; d <- 0
#   for(j in 1:length(runJags$estimates)){
#     m <- length(runJags$estimates[[j]])
#     w <- length(design$parameter_set[[j]])
#     MatEstimates[k,(c+1):(c+m)] <- runJags$estimates[[j]]
#     MatTrueVal[k,(d+1):(d+w)]   <- design$parameter_set[[j]]
#     if(is.vector(runJags$credInterval[[j]])){
#       ArrayCredInt[k,(c+1):(c+m),1] <- runJags$credInterval[[j]][1]
#       ArrayCredInt[k,(c+1):(c+m),2] <- runJags$credInterval[[j]][2]
#     }else{
#       ArrayCredInt[k,(c+1):(c+m),1] <- runJags$credInterval[[j]][1,]
#       ArrayCredInt[k,(c+1):(c+m),2] <- runJags$credInterval[[j]][2,]
#     }
#     c <- c+m; d <- d+w
#   }
# }
# 
# paramNames <- NA
# paramNames2 <- NA
# for(j in 1:length(runJags$estimates)){
#   if(is.vector(runJags$credInterval[[j]])){
#     paramNames <- c(paramNames, names(runJags$credInterval[j]))
#   }else{
#     paramNames <- c(paramNames, colnames(runJags$credInterval[[j]]))
#   }
#   if(length(design$parameter_set[[j]])==1){
#     paramNames2 <- c(paramNames2, names(design$parameter_set[j]))
#   }else{
#     labels <- paste(names(design$parameter_set[j]), "[",1:length(design$parameter_set[[j]]),"]",sep="")
#     paramNames2 <- c(paramNames2, labels)
#   }
# }
# paramNames <- paramNames[-1]
# paramNames2 <- paramNames2[-1]
# colnames(MatEstimates) <- paramNames
# colnames(ArrayCredInt) <- paramNames
# colnames(MatTrueVal)   <- paramNames2
# colnames(MatRhats) <- names(runJags$rhats)
#   
#   ########## Create empty arrays to save output #############################
#   ### Size variables
#   # Parameter labels
#   par.labels <- c("mu1","mu2", "bound","ndt")
#   # No. of parameters
#   npar <- length(par.labels)
#   # Extensive no. of columns (for True value matrix and Rhats)
#   ncols <- npar+1
#   # Number of samples kept per chain
#   nrows <- n.iter-n.burnin
#   ### Array 1: True parameter values used to generate data
#   trueValues            <- array(NA, dim=c(possible.combinations,npar))
#   colnames(trueValues)  <- par.labels
#   ### Array 2: Mean posteriors
#   retrievedValues           <- array(NA,dim=c(iterations,npar,possible.combinations))
#   colnames(retrievedValues) <- par.labels
#   ### Array 3: Standard deviation
#   retrievedValues_sd            <- array(NA,dim=c(iterations,npar,possible.combinations))
#   colnames(retrievedValues_sd)  <- par.labels
#   ### Array 4: MAP
#   mapValues           <- array(NA,dim=c(iterations,npar,possible.combinations))
#   colnames(mapValues) <- par.labels
#   ### Array 5: R hats
#   rhats   <- array(NA,dim=c(iterations,ncols,possible.combinations))
#   ### Array 6: Seconds elapsed per simulation
#   timers  <- array(NA,dim=c(iterations,possible.combinations))
#   ### Array 7: Record seeds
#   seeds <- array(NA,dim=c(iterations,possible.combinations))
#   
#   out.size <- possible.combinations * iterations
#   for(set in 1:possible.combinations){
#     J <- seq(set,out.size,possible.combinations) 
#     for(i in 1:iterations){
#       j <- J[i]
#       S <- output[j,]
#       seeds[i,set] <- S$seed
#       timers[i,set] <- S$elapsed.time
#       rhats[i,,set] <- S$rhats
#       trueValues[set,] <- S$true.values
#       retrievedValues[i,,set] <- S$mean.estimates
#       retrievedValues_sd[i,,set] <- S$std.estimates
#       mapValues[i,,set] <-S$map.estimates
#     }
#   }
#   
#   save(timers, file = paste(output.folder,studyName,"_timers.RData",sep=""))
#   save(seeds, file = paste(output.folder,studyName,"_seeds.RData",sep=""))
#   colnames(rhats) <- names(S$rhats)
#   save(rhats, file = paste(output.folder,studyName,"Rhats.RData",sep=""))
#   save(trueValues, file = paste(output.folder,studyName,"trueValues.RData",sep=""))
#   save(retrievedValues, file = paste(output.folder,studyName,"meanPosteriors.RData",sep=""))
#   save(retrievedValues_sd, file = paste(output.folder,studyName,"std.RData",sep=""))
#   save(mapValues, file = paste(output.folder,studyName,"MAPs.RData",sep=""))
#   save(settings, file = paste(output.folder,studyName,"settings.RData",sep=""))
# }