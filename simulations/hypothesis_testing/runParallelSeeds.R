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
source(here("code", "functions", "show_priors.R"))
source(here("code", "functions", "generate_dataset.R"))
source(here("code", "functions", "generate_trial.R"))
source(here("code", "functions", "getStatistics.R"))
source(here("code", "functions", "extractSamples.R"))

skip_scripts <- c("plot_hypothesisTesting_paper.R", 
                  "plot_betaDistributions_paper.R",
                  "plot_hypothesisTesting_paper_mgkrp.R")
cat("Sourcing scripts...\n")
for(archive in dir(here("simulations", "hypothesis_testing", "scripts"))){   
    if(archive %in% skip_scripts){
        next
    }else{                
        cat(paste("Sourcing:", archive, "\n"))
        source(here("simulations", "hypothesis_testing", "scripts", archive))        
    }    
}

##########################################################
# SIMULATION SETTINGS
##########################################################
# Create output directory if it doesn't exist
output_dir <- here("simulations", "hypothesis_testing", "samples")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

settings <- list(
    "output.folder" = file.path(output_dir, "/"),  # Ensure trailing slash
    "nParticipants" = nParticipants,
    "nTrialsPerCondition" = nTrialsPerCondition,
    "nDatasets" = 200,
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
                      W <- HDDM_simBySeed_fixEff(seed = i, settings, forceRun=TRUE,
                                                 redo_if_bad_rhat=TRUE, rhat_cutoff=1.05)
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
store_parallelOutput(resultado, settings, saveTo = "./results/")




save(resultado, file = here("simulations", "hypothesis_testing", "results", "resultadoFullObject_200.RData"))

