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
output_dir <- here("demos", "simulation-studies", "generative_priors", "samples")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Fixed design variables
settings <- list("fromPrior" = TRUE,
                 "output.folder" = file.path(output_dir, "/"),
                 "participant_levels" = c(20,40,80,160,320),
                 "trial_levels" = c(20,40,80,160,320),
                 "nDatasets" = 1000,
                 "criterion_levels" = c("drift", "nondt", "bound"),
                 "design_levels" = c("ttest","metaregression"),
                 "n.chains" = 2,
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
for(nPart in settings$participant_levels){
    jagsInits <- c(jagsInits, list(JAGS_inits(settings$n.chains, nPart, custom_sd = 0.1)))
}

# Add JAGS objects to settings
settings <- c(settings, list("jagsParameters" = list(jagsParameters, jagsParameters),
                             "modelFile" = matrix(c(rep(c(here("output", "BUGS-models", "EZHBDDM_genPriors_BetaDrift.bug"),
                                                          here("output", "BUGS-models", "EZHBDDM_genPriors_BetaNondt.bug"),
                                                          here("output", "BUGS-models", "EZHBDDM_genPriors_BetaBound.bug")),2)), 
                                                    byrow = TRUE, ncol = 3),
                             "priors" = list(JAGS_priors(Show=FALSE, "ttest", custom_prior_list = NULL), 
                                             JAGS_priors(Show=FALSE, "metaregression", custom_prior_list = NULL)),
                             "jagsData" = list(JAGS_passData("ttest"), JAGS_passData("metaregression")),
                             "jagsInits" = jagsInits))

# Name objects so they can be called more easily
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
custom_truncation_list <- list(
        "bound_mean" = c(0.1, 3.0), "nondt_mean" = c(0.05, ""), "drift_mean" = c(-3, 3),
        "bound_sdev" = c(0.01, ""), "nondt_sdev" = c(0.01, ""), "drift_sdev" = c(0.01, ""),
        "drift" = c(-3, 3), "bound" = c(0.1, 3.0), "nondt" = c(0.05, ""), "betaweight" = c(-3, 3))

# Write JAGS models
for(model in settings$design_levels){
    for(crit in settings$criterion_levels){
        JAGS_writeModel(priors = settings$priors[[2]], modelType = model, 
                        criterion =crit, modelFile = settings$modelFile[model,crit],
                        custom_truncation_list = custom_truncation_list)
    }
}



################################################################
# Run Parallel Seeds
################################################################
cores       <-  detectCores()
my.cluster  <-  makeCluster(cores[1]-3)

registerDoParallel(cl = my.cluster)
output <- foreach(seed = 1:4, 
                  .errorhandling = "pass",
                  .combine = 'rbind'
                  ) %dopar% {
                    Z <- HDDM_runFullSeed(seed = seed, settings = settings, 
                                          forceRun = TRUE, redo_if_bad_rhat = TRUE, rhat_cutoff = 1.05)
                  }
stopCluster(cl = my.cluster)
