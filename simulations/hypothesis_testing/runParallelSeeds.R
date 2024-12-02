##########################################################
# LOAD FUNCTIONS/PACKAGES
##########################################################
######## Load required R packages for parallel processing
nParticipants <- 40
nTrialsPerCondition <- 40
beta_levels <- c(0,0.2,0.4)
source("../../code/functions/show_priors.R")
source("../../code/functions/generate_dataset.R")
source("../../code/functions/generate_trial.R")
source("../../code/functions/getStatistics.R")
source("../../code/functions/extractSamples.R")
for(archive in dir("./scripts/")){   
  if(archive == "plot_hypothesisTesting_paper.R"|archive == "plot_betaDistributions_paper.R"){next}
  source(paste("./scripts/",archive,sep=""))     }
library(foreach)
library(doParallel)

##########################################################
# SIMULATION SETTINGS
##########################################################
########| Create a "settings" object that specifies all relevant aspects of the simulation study
#################| Fixed variables
settings <- list("output.folder" = "./samples_40x40/", # Before running this script, indicate where to store samples
                 "nParticipants" = nParticipants,
                 "nTrialsPerCondition" = nTrialsPerCondition,
                 "nDatasets" = 1000,
                 "beta_levels" = beta_levels,
                 "n.chains" = 2,
                 "nCells" = length(beta_levels),
                 "X" = rep(c(1,0),nParticipants),
                 "P" = rep(1:nParticipants, each=2))
#################| Specific JAGS objects
jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                    "drift_sdev", "nondt_sdev", "bound_sdev", "drift", "betaweight")
jagsInits <- rep(list(list()), settings$n.chains)
for(i in 1:settings$n.chains){
  jagsInits[[i]] <- list(drift = matrix(rep(rnorm(settings$nParticipants,0,0.25),2),ncol=2, byrow = FALSE))
}
jagsData <- list("nParticipants", "nTrialsPerCondition", "X", "P", "meanRT", "varRT", "correct")
settings <- c(settings, list("modelFile" = "./FixedEffect.bug" ,
                             "jagsParameters" = jagsParameters,
                             "priors" = default_priors(Show=FALSE),
                             "jagsData" = jagsData,
                             "jagsInits" = jagsInits))

writefixEffJAGSmodel(settings$priors,settings$modelFile)


################################################################
# Define simulation functions
################################################################
cores       <-  detectCores()
my.cluster  <-  makeCluster(cores[1]-4)

registerDoParallel(cl = my.cluster)
resultado <- foreach(i = 1:settings$nDatasets, 
                    .errorhandling = "pass",
                    .combine = 'rbind'
                    ) %dopar% {
                      W <- HDDM_simBySeed_fixEff(seed = i, settings, forceRun=TRUE)
                    }
stopCluster(cl = my.cluster)

#res1to30 <- resultado
#res1to300 <- rbind(res1to30,resultado)
#res1to500 <- rbind(res1to300,resultado)

#res2_1to50 <- resultado
#res2_1to300 <- rbind(res2_1to50,resultado)
#res2_1to500 <- rbind(res2_1to300,resultado)
#res2_1to750 <- rbind(res2_1to500,resultado)
#res2_1to1000 <- rbind(res2_1to750,resultado)
#this.output <- resultado

settings$nDatasets <- nrow(resultado)
store_parallelOutput(resultado, settings, saveTo = "./results_40x40/")






