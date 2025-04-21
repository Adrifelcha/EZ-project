##########################################################
# LOAD FUNCTIONS/PACKAGES
##########################################################
######## Load required R packages for parallel processing
library(here)
library(foreach)
library(doParallel)

########| Load required R scripts
skip_scripts <- c("README.md")
cat("Sourcing scripts...\n")
for(archive in dir(here("src"))){
    if(archive %in% skip_scripts){
        next
    }else{
        cat(paste("Sourcing:", archive, "\n"))
        source(here("src", archive))
    }
}

##########################################################
# SIMULATION SETTINGS
##########################################################
# Create output directory if it doesn't exist
output_dir <- here("demos", "simulation-studies", "generative_uniforms", "samples")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Fixed simulation design variables
settings <- list("fromPrior" = FALSE, 
                 "output.folder" = file.path(output_dir, "/"), 
                 "participant_levels" = c(20,40,80,160,320), 
                 "trial_levels" = c(20,40,80,160,320),
                 "nDatasets" = 1000,
                 "criterion_levels" = c("drift", "nondt", "bound"),
                 "design_levels" = c("ttest","metaregression"),
                 "n.chains" = 3,
                 "n.burnin" = 500,
                 "n.iter" = 4000,
                 "n.thin" = 1)
# Implied number of cells
settings <- c(settings,
              list("nCells" = prod(length(settings$participant_levels),length(settings$trial_levels))*(3*2)))

# Prepare JAGS objects
jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                    "drift_sdev", "nondt_sdev", "bound_sdev", "drift", "betaweight")
jagsInits <- list()
for(i in settings$participant_levels){
    jagsInits <- c(jagsInits, list(JAGS_inits(n.chains = settings$n.chains, nParticipants = i, custom_sd = 0.1)))
}
# Prepare specific prior distribution parameters
custom_priors_list <- list(
                      "bound_mean_mean" = 2.25,    "bound_mean_sdev" = 1.00,
                      "drift_mean_mean" = 0.00,    "drift_mean_sdev" = 3.00,
                      "nondt_mean_mean" = 0.55,    "nondt_mean_sdev" = 0.25,
                      "bound_sdev_lower" = 0.01,   "bound_sdev_upper" = 2.00,
                      "drift_sdev_lower" = 0.01,   "drift_sdev_upper" = 2.00,
                      "nondt_sdev_lower" = 0.01,   "nondt_sdev_upper" = 0.50,
                      "betaweight_mean" = 0,       "betaweight_sdev" = 1)

# Add JAGS objects to settings
settings <- c(settings, list("jagsParameters" = list(jagsParameters, jagsParameters),
                             "modelFile" = matrix(c(rep(c(here("output", "BUGS-models", "EZHBDDM_genUniforms_BetaDrift.bug"),
                                                          here("output", "BUGS-models", "EZHBDDM_genUniforms_BetaNondt.bug"),
                                                          here("output", "BUGS-models", "EZHBDDM_genUniforms_BetaBound.bug")),2)), 
                                                    byrow = TRUE, ncol = 3),
                             "priors" = list(JAGS_priors(Show=FALSE, "ttest", custom_prior_list = custom_priors_list), 
                                             JAGS_priors(Show=FALSE, "metaregression", custom_prior_list = custom_priors_list)),
                             "jagsData" = list(JAGS_passData("ttest"), JAGS_passData("metaregression")),
                             "jagsInits" = jagsInits))
# Change names so they can be called more easily
names(settings$jagsParameters) <- settings$design_levels
names(settings$priors) <- settings$design_levels
names(settings$jagsData) <- settings$design_levels
names(settings$jagsInits) <- settings$participant_levels
colnames(settings$modelFile) <- settings$criterion_levels
rownames(settings$modelFile) <- settings$design_levels


##########################################################
# Write JAGS models
##########################################################
# Define custom truncation list

for(model in settings$design_levels){
    for(crit in settings$criterion_levels){
        JAGS_writeModel(priors = settings$priors[[2]], modelType = model, 
                        criterion = crit, modelFile = settings$modelFile[model,crit],
                        custom_truncation_list = NULL)
    }
}

################################################################
# Define simulation functions
################################################################
cores       <-  detectCores()
my.cluster  <-  makeCluster(cores[1]-4)

registerDoParallel(cl = my.cluster)
output <- foreach(i = 201:1000, 
                  .errorhandling = "pass",
                  .combine = 'rbind'
                  ) %dopar% {
                    Z <- HDDM_runFullSeed(seed = i, settings, forceRun = TRUE)
                  }
stopCluster(cl = my.cluster)

#output1to200 <- output
this.output <- output
output <- rbind(output1to200,this.output)
#output <- rbind(output1to119,output)
#output1to119 <- output

settings$nDatasets <- nrow(output)
source("../../../code/functions/store_parallelOutput.R")
store_parallelOutput(output, settings, saveTo = "./results/")
makeSimStudyPlot("./results/simStudy_Meta_drift.RData", plotType = 2)


