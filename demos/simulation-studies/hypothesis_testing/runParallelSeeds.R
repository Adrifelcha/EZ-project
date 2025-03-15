##########################################################
# Hypothesis testing simulation study
##########################################################
############## Adriana F. Chávez De la Peña ##############
# Main settings
nParticipants <- 40
nTrialsPerCondition <- 40
beta_levels <- c(0,0.2,0.4)

##########################################################
# LOAD FUNCTIONS/PACKAGES
##########################################################
######## Load required R packages for parallel processing
library(here)
library(foreach)
library(doParallel)
########| Load required R scripts
source(here("src", "show_priors.R"))
source(here("src", "generate_dataset.R"))
source(here("src", "generate_trial.R"))
source(here("src", "getStatistics.R"))
source(here("src", "extractSamples.R"))

skip_scripts <- c("plot_hypothesisTesting_paper.R", 
                  "plot_betaDistributions_paper.R",
                  "plot_hypothesisTesting_paper_mgkrp.R")
cat("Sourcing scripts...\n")
for(archive in dir(here("demos", "simulation-studies", "hypothesis_testing", "scripts"))){   
    if(archive %in% skip_scripts){
        next
    }else{                
        cat(paste("Sourcing:", archive, "\n"))
        source(here("demos", "simulation-studies", "hypothesis_testing", "scripts", archive))        
    }    
}

##########################################################
# SIMULATION SETTINGS
##########################################################
# Create output directory if it doesn't exist
output_dir <- here("demos", "simulations", "hypothesis_testing", "samples")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

settings <- list(
    "output.folder" = file.path(output_dir, "/"),  # Ensure trailing slash
    "nParticipants" = nParticipants,
    "nTrialsPerCondition" = nTrialsPerCondition,
    "nDatasets" = 1000,
    "beta_levels" = beta_levels,
    "n.chains" = 2,
    "n.iter" = 2000,
    "n.burnin" = 500,
    "n.thin" = 3,
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
settings <- c(settings, list("modelFile" = here("output", "BUGS-models", "EZHBDDM_within-subject.bug"),
                             "jagsParameters" = jagsParameters,
                             "priors" = default_priors(Show=FALSE),
                             "jagsData" = jagsData,
                             "jagsInits" = jagsInits))

write_within_subject_model(priors = settings$priors, modelFile = settings$modelFile, custom_truncation_list = NULL)

################################################################
# Define simulation functions
################################################################
Big_start <- Sys.time()
cores       <-  detectCores()
my.cluster  <-  makeCluster(cores[1]-4)

registerDoParallel(cl = my.cluster)
resultado <- foreach(i = 1001:1010, 
                    .errorhandling = "pass",
                    .combine = 'rbind'
                    ) %dopar% {
                      W <- HDDM_simBySeed_fixEff(seed = i, settings, forceRun=TRUE,
                                                 redo_if_bad_rhat=TRUE, rhat_cutoff=1.05)
                    }
stopCluster(cl = my.cluster)
Big_end <- Sys.time()
cat("Time taken:", difftime(Big_end, Big_start, units = "mins"), "minutes\n")

#res1to20 <- resultado
#res1to20B <- resultado
#res1to1000 <- rbind(res1to200, resultado)

#res1to200 <- rbind(res1to80, resultado)
resultado <- res1to1000
#settings$nDatasets <- nrow(resultado)

nrow(resultado)
# Store the results
# Default location: repo-root/output/RData-results
# Look for filename starting with: simHypTesting_...RData
store_parallelOutput(resultado, settings)

