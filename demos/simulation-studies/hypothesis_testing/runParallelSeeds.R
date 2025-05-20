##########################################################
# Hypothesis testing simulation study
##########################################################
############## Adriana F. Chávez De la Peña ##############
# Main settings
nParticipants <- 40
nTrialsPerCondition <- 80
beta_levels <- c(0,0.2,0.4)
forceRun <- FALSE
redo_if_bad_rhat <- TRUE
rhat_cutoff <- 1.05

##########################################################
# LOAD FUNCTIONS/PACKAGES
##########################################################
######## Load required R packages for parallel processing
library(here)
library(foreach)
library(doParallel)

# Call the function within the src directory
source(here("src", "loading", "load_allFunctions.R"))
load_allCustomFunctions()

source(here("demos", "simulation-studies", "hypothesis_testing", "src", "store_BetaParallelOutput.R"))
##########################################################
# SIMULATION SETTINGS
##########################################################
# Specify custom priors
custom_priors <- list("nondt_sdev_lower" = 0.025, "drift_mean_mean" = 0.5,
                      "nondt_mean_mean" = 0.4)

# Create output directory if it doesn't exist
output_dir <- here("demos", "simulation-studies", "hypothesis_testing", "samples")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

settings <- list(
    "output.folder" = file.path(output_dir, "/"),  # Ensure trailing slash
    "nParticipants" = nParticipants,
    "nTrialsPerCondition" = nTrialsPerCondition,
    "nDatasets" = 1000,
    "beta_levels" = beta_levels,
    "n.chains" = 2,
    "n.iter" = 4000,
    "n.burnin" = 500,
    "n.thin" = 1,
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
                             "priors" = JAGS_priors(Show = FALSE,custom_prior_list = custom_priors),
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
resultado <- foreach(i = 1:settings$nDatasets, 
                    .errorhandling = "pass",
                    .combine = 'rbind'
                    ) %dopar% {
                      W <- HDDM_simBySeed_withinSubject(seed = i, settings, forceRun = forceRun,
                                                        redo_if_bad_rhat = redo_if_bad_rhat,
                                                        rhat_cutoff = rhat_cutoff)
                    }
stopCluster(cl = my.cluster)
Big_end <- Sys.time()
cat("Time taken:", difftime(Big_end, Big_start, units = "mins"), "minutes\n")

# Load the results from the seed-specific .RData files
resultado <- load_seedOutput(here("demos", "simulation-studies", "hypothesis_testing", "samples"))

# Make sure the number of datasets is correct
settings$nDatasets <- nrow(resultado$results)

# Store the results to repo-root/output/RData-results
store_BetaParallelOutput(output = resultado$results, settings = settings)
