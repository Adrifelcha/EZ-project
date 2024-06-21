##########################################################
# LOAD FUNCTIONS/PACKAGES
##########################################################
######## Load required R packages for parallel processing
for(archive in dir("../functions/")){    source(paste("../functions/",archive,sep=""))     }
source("../scripts/HDDM_runFullSeed_fixedEffects.R")
library(foreach)
library(doParallel)

##########################################################
# SIMULATION SETTINGS
##########################################################
#######| Create fixed variables to be used during the simulations
jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                    "drift_sdev", "nondt_sdev", "bound_sdev", "drift", "betaweight")
modelFile <- modelFile <- "./FixedEffect.bug"  
########| Create a "settings" object that specifies all relevant aspects of the simulation study
#################| Fixed variables
settings <- list("fromPrior" = FALSE,  # This logical variable should be defined before running this script
                 "output.folder" = "../../simulations/hypothesis_testing/", # Before running this script, indicate where to store samples
                 "participant_levels" = c(20,40,80,160,320),
                 "trial_levels" = c(20,40,80,160,320),
                 "nDatasets" = 1000,
                 "beta_levels" = c(0,0.5),
                 "n.chains" = 2)
settings <- c(settings,
              list("nCells" = prod(length(settings$participant_levels),length(settings$trial_levels))*2))
#################| Specific JAGS objects
jagsInits <- list()
for(i in settings$participant_levels){
    tmp <- rep(list(list()), settings$n.chains)
    for(j in 1:settings$n.chains){
      tmp[[j]] <- list(drift = matrix(rnorm(i*2,0,1),ncol=2))
    }
   jagsInits <- c(jagsInits, list(tmp))
}
names(jagsInits) <- settings$participant_levels
jagsData = c(data_toJAGS(modelType="ttest"), list("P"))
jagsData[[2]] <- "nTrialsPerCondition"

settings <- c(settings, list("jagsParameters" = jagsParameters,
                             "modelFile" = modelFile,
                             "priors" = default_priors(Show=FALSE, "ttest"),
                             "jagsData" = jagsData,
                             "jagsInits" = jagsInits))
writefixEffJAGSmodel(settings$priors,beta.effect = 0.5,modelFile)

x <- HDDM_runFullSeed(1,settings)
# x <- load("../../simulations/generative_priors/seed-1.RData")

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
                      W <- HDDM_runFullSeed_fixEff(seed = i, settings)
                    }
stopCluster(cl = my.cluster)







