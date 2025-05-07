# LOAD FUNCTIONS/PACKAGES
library(foreach)
library(doParallel)
source(here::here("simulations", "generative_priors", "header.R"))


# SIMULATION SETTINGS
# Create fixed variables to be used during the simulations

jagsParameters <- c("bound"     , "drift"     , "nondt",
                    "bound_mean", "drift_mean", "nondt_mean", 
                    "bound_sdev", "drift_sdev", "nondt_sdev",
                    "betaweight")

modelFile <- matrix(c(rep(c(here::here("tmp", "EZHBDDM_BetaDrift.bug"),
                            here::here("tmp", "EZHBDDM_BetaNondt.bug"),
                            here::here("tmp", "EZHBDDM_BetaBound.bug")), 2)), 
                    byrow = TRUE, 
                    ncol = 3)


# Create a settings object that specifies all relevant aspects of the 
# simulation study

# Fixed variables
settings <- list("fromPrior" = TRUE,  
                 "output.folder" = here::here("tmp/"),
                 "participant_levels" = c(20,40,80,160,320), 
                 "trial_levels" = c(20,40,80,160,320),
                 "nDatasets" = 1000,
                 "criterion_levels" = c("drift", "nondt", "bound"),
                 "design_levels" = c("ttest","metaregression"),
                 "n.chains" = 3)

settings$participant_levels <- c(20,40)
settings$trial_levels       <- c(20,40)
settings$nDatasets          <- 200

settings <- c(settings,
              list("nCells" = prod(length(settings$participant_levels),
                                   length(settings$trial_levels))*(3*2)))

# Specific JAGS objects
jagsInits <- list()
for(i in settings$participant_levels){
  jagsInits <- c(jagsInits, list(default_inits(settings$n.chains, i)))
}

settings <- c(settings, 
              list("jagsParameters" = list(jagsParameters, jagsParameters),
                   "modelFile"      = modelFile,
                   "priors"         = list(default_priors(Show=FALSE, "ttest"), 
                                           default_priors(Show=FALSE, "metaregression")),
                   "jagsData"       = list(data_toJAGS("ttest"), 
                                           data_toJAGS("metaregression")),
                   "jagsInits"      = jagsInits))

# Name objects so they can be called more easily
names(settings$jagsParameters) <- settings$design_levels
names(settings$priors)         <- settings$design_levels
names(settings$jagsData)       <- settings$design_levels
names(settings$jagsInits)      <- settings$participant_levels
colnames(settings$modelFile)   <- settings$criterion_levels
rownames(settings$modelFile)   <- settings$design_levels

# Write JAGS models
for(model in settings$design_levels){
    for(crit in settings$criterion_levels){
        write_JAGSmodel(settings$priors[[2]], 
                        model,
                        crit,
                        settings$modelFile[model,crit])
    }
}


# Define simulation functions
cores       <-  detectCores()
my.cluster  <-  makeCluster(cores[1]-3)

registerDoParallel(cl = my.cluster)
output <- foreach(i = 1:4, 
                  .errorhandling = "pass",
                  .combine = 'rbind'
                  ) %dopar% {
                    Z <- HDDM_runFullSeed(seed = i, settings, forceRun = TRUE)
                  }
stopCluster(cl = my.cluster)

source(here::here("code", "functions", "store_parallelOutput.R"))
store_parallelOutput(output, settings, saveTo = here::here("results/"))

makeSimStudyPlot(here::here("results", "simStudy_Meta_drift.RData"), plotType = 2)

